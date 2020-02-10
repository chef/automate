package diagnostics

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"time"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
	"github.com/chef/automate/lib/httputils"
)

// ErrKeyNotFound is returned when the requested key is not found in the context
var ErrKeyNotFound = errors.New("Key not found")

// ErrDeploymentServiceUnavailable is returned when it is not possible to talk to
// the deployment service
var ErrDeploymentServiceUnavailable = errors.New("Deployment service unavailable")

// TestContext is the context that is passed to all diagnostics. They may set and
// retrieve values from it. The values can be saved to JSON to be recalled later
type TestContext interface {
	// GetValue returns the value for a given key. It will decode it into value,
	// which should be a pointer to a map or a struct
	GetValue(key string, value interface{}) error
	// SetValue sets key to the given value.
	SetValue(key string, value interface{})
	// WriteJSON writes out to json all the values that have been set, along with
	// the admin token that was used or generated
	WriteJSON(writer io.Writer) error
	// DoLBRequest performs an authenticated request against the automate load balancer
	DoLBRequest(path string, opts ...lbrequest.Opts) (*http.Response, error)
	// PublishViaNATS publishes the messages to the automate event gateway
	PublishViaNATS([][]byte) error
	// GetOption returns the Option for the given key
	GetOption(key string) *Option
	// GetVersion returns the version of automate that is running
	GetVersion() (string, error)
	// IsIAMV2 returns whether or not automate is on the latest version of IAM
	IsIAMV2() (bool, error)
}

// VerificationTestContext is accepted by the Verify stage of a diagnostic. This
// interface will allow the Verify to use the testify testing framework to write
// assertions
type VerificationTestContext interface {
	TestContext
	Errorf(format string, args ...interface{})
	FailNow()
}

type testContext struct {
	save
	lbURL      url.URL
	dsClient   api.DeploymentClient
	httpClient *http.Client
	version    string
}

type globals struct {
	CachedToken string    `json:"token"`
	Options     []*Option `json:"options"`
}

type save struct {
	Globals    globals                `json:"globals"`
	Components map[string]interface{} `json:"components"`
}

// TestContextOpt is an functional option creating a test context
type TestContextOpt func(*testContext)

// WithAdminToken sets the admin token to use
func WithAdminToken(token string) TestContextOpt {
	return func(tstContext *testContext) {
		tstContext.save.Globals.CachedToken = token
	}
}

func WithOptions(options []*Option) TestContextOpt {
	return func(tstContext *testContext) {
		tstContext.save.Globals.Options = options
	}
}

func WithLBURL(url url.URL) TestContextOpt {
	return func(tstContext *testContext) {
		tstContext.lbURL = url
	}
}

// NewTestContext creates a test context with the given DeploymentClient
func NewTestContext(dsClient api.DeploymentClient, opts ...TestContextOpt) TestContext {
	return create(dsClient, save{Components: make(map[string]interface{})}, opts...)
}

// LoadTestContext loads a saved TestContext
func LoadTestContext(dsClient api.DeploymentClient, reader io.Reader, opts ...TestContextOpt) (TestContext, error) {
	decoder := json.NewDecoder(reader)
	save := save{}
	err := decoder.Decode(&save)
	if err != nil {
		return nil, err
	}

	return create(dsClient, save, opts...), nil
}

func create(dsClient api.DeploymentClient, save save, opts ...TestContextOpt) *testContext {
	tr := httputils.NewDefaultTransport()
	tr.TLSClientConfig = &tls.Config{InsecureSkipVerify: true}
	httpClient := &http.Client{Transport: tr}

	tstContext := &testContext{
		save:       save,
		httpClient: httpClient,
		dsClient:   dsClient,
	}

	for _, o := range opts {
		o(tstContext)
	}
	return tstContext
}

func (c *testContext) GetValue(key string, value interface{}) error {
	internalValue, found := c.Components[key]

	if !found {
		return ErrKeyNotFound
	}

	decoder, err := mapstructure.NewDecoder(&mapstructure.DecoderConfig{
		TagName: "json",
		Result:  value,
	})

	if err != nil {
		return err
	}

	return decoder.Decode(internalValue)
}

func (c *testContext) SetValue(key string, value interface{}) {
	c.Components[key] = value
}

func (c *testContext) GetOption(key string) *Option {
	for _, o := range c.Globals.Options {
		if o.Key == key {
			return o
		}
	}
	return nil
}

func (c *testContext) WriteJSON(writer io.Writer) error {
	bytes, err := json.Marshal(c.save)

	if err != nil {
		return err
	}

	_, err = writer.Write(bytes)
	return err
}

func (c *testContext) DoLBRequest(path string, opts ...lbrequest.Opts) (*http.Response, error) {
	token, err := c.adminToken()

	if err != nil {
		return nil, errors.Wrap(err, "Failed to get admin token")
	}

	opts = append(opts, lbrequest.WithDefaultAuthToken(token), lbrequest.WithURL(c.lbURL))
	var lastErr error
	for i := 0; i <= 5; i++ {
		// Backoff: Sleep for (2^i - 1) seconds
		time.Sleep(((1 << i) - 1) * time.Second)
		req, err := lbrequest.New(path, opts...)

		if err != nil {
			return nil, err
		}

		resp, err := c.httpClient.Do(req)
		if err != nil {
			lastErr = err
			continue
		}
		if resp.StatusCode >= 500 {
			lastErr = errors.Errorf("Got 5xx. %d %s: %s", resp.StatusCode, resp.Status, path)
			resp.Body.Close() // nolint: errcheck
			continue
		}
		return resp, nil
	}
	return nil, lastErr
}

func (c *testContext) GetVersion() (string, error) {
	if c.version != "" {
		return c.version, nil
	}

	httpResp, err := c.DoLBRequest("/api/v0/version")
	if err != nil {
		return "", err
	}
	defer httpResp.Body.Close() // nolint: errchech

	resp := struct {
		Build string `json:"build_timestamp"`
	}{}

	if err := json.NewDecoder(httpResp.Body).Decode(&resp); err != nil {
		return "", err
	}

	c.version = resp.Build

	return resp.Build, nil
}

func (c *testContext) IsIAMV2() (bool, error) {
	httpResp, err := c.DoLBRequest("/apis/iam/v2beta/policy_version")
	if err != nil {
		return false, err
	}
	defer httpResp.Body.Close() // nolint: errchech

	if httpResp.StatusCode == 404 {
		// if the policy version endpoint is not found,
		// we're either testing an old version of Automate with only v1
		// or we're testing a version of Automate that has been force-upgraded to v2
		automateVersion, err := c.GetVersion()
		if err != nil {
			return false, err
		}

		automateVersionInt, err := strconv.ParseInt(automateVersion, 10, 64)
		if err != nil {
			return false, err
		}

		// if the version is earlier than the force-upgrade version,
		// we must be testing an earlier version of Automate with only IAM v1.
		// !! TODO change this to the build just before force-upgrade build
		if automateVersionInt < 20200131232134 {
			return false, nil
		}

		// anything after 20200131232134 has been force-upgraded to IAMv2
		return true, nil
	}

	vsn := struct {
		Version struct{ Major, Minor string }
	}{}

	if err := json.NewDecoder(httpResp.Body).Decode(&vsn); err != nil {
		return false, err
	}

	return vsn.Version.Major == "V2", nil
}

func (c *testContext) PublishViaNATS(messages [][]byte) error {
	authToken, err := c.adminToken()
	if err != nil {
		return errors.Wrap(err, "Failed to get admin token")
	}
	port := "4222"
	clientID := "diagnostics"
	url := fmt.Sprintf("nats://%s@%s:%s", authToken, c.lbURL.Hostname(), port)

	client := nats.NewExternalClient(
		url,
		"event-service", clientID, "", "habitat")
	client.InsecureSkipVerify = true

	// We would need to see the event gateway's service config to know whether to
	// set this
	client.DisableTLS = false

	err = client.Connect()
	if err != nil {
		return errors.Wrapf(err, "failed to connenct to nats at URL %s", url)
	}
	defer client.Close()

	for _, message := range messages {
		err = client.PublishBytes(message)
		if err != nil {
			return errors.Wrap(err, "failed to publish message to automate NATS gateway")
		}
	}

	return nil
}

func (c *testContext) adminToken() (string, error) {
	if c.dsClient == nil {
		return "", ErrDeploymentServiceUnavailable
	}

	if c.Globals.CachedToken == "" {
		resp, err := c.dsClient.GenerateAdminToken(context.TODO(), &api.GenerateAdminTokenRequest{
			Description: "This token was generated by the chef-automate diagnostic tool. " +
				"It has admin level access on the entire Automate API.",
		})

		if err != nil {
			return "", err
		}
		c.Globals.CachedToken = resp.ApiToken
	}

	return c.Globals.CachedToken, nil
}
