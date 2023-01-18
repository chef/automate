package diagnostics

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
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
	// CleanupAdminToken deletes the admin token generated for the diagnostics test runner
	CleanupAdminToken() error
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
	TokenID     string    `json:"token_id"`
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
		if resp.StatusCode >= 500 || resp.StatusCode == 403 {
			lastErr = errors.Errorf("Got status: %d %s: %s", resp.StatusCode, resp.Status, path)
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
	defer httpResp.Body.Close()

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
	vsn := struct {
		Version struct{ Major, Minor string }
	}{}

	// this API path was removed on 2/26/20, but this diagnostic still checks it
	// in case it's testing an old version of Automate that has only the v2beta path
	v2betaResp, err := c.DoLBRequest("/apis/iam/v2beta/policy_version")
	if err != nil {
		return false, err
	}
	defer v2betaResp.Body.Close()

	if v2betaResp.StatusCode == 200 {
		if err := json.NewDecoder(v2betaResp.Body).Decode(&vsn); err != nil {
			// on ancient versions of Automate, any unknown	prefix on an API query
			// gets redirected by nginx to serve up the UI and responds with 200.
			// We can assume v1 in this case.
			return false, nil
		}

		return vsn.Version.Major == "V2", nil

	} else if v2betaResp.StatusCode == 404 {
		// before assuming v1, we check the v2 policy_version API,
		// since we might be on a post-force-upgrade version of Automate
		v2Resp, err := c.DoLBRequest("/apis/iam/v2/policy_version")
		if err != nil {
			return false, err
		}
		defer v2Resp.Body.Close()

		if v2Resp.StatusCode == 200 {
			if err := json.NewDecoder(v2Resp.Body).Decode(&vsn); err != nil {
				// See decoder comment above,  we assume v1 in this case.
				return false, nil
			}

			return vsn.Version.Major == "V2", nil

			// if the /policy_version endpoint is not found using the v2 or v2beta prefix,
			// we must be testing an old version of Automate with only v1.
		} else if v2Resp.StatusCode == 404 {
			return false, nil
		}

		return false, errors.Errorf("failed to verify IAM version with status code: %v", v2Resp.StatusCode)
	}

	// any other unexpected responses get caught here
	return false, errors.Errorf("failed to verify IAM version with status code: %v", v2betaResp.StatusCode)
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
		return errors.Wrapf(err, "failed to connect to nats at URL %s", url)
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
	if c.Globals.CachedToken != "" {
		return c.Globals.CachedToken, nil
	}

	if c.dsClient == nil {
		return "", ErrDeploymentServiceUnavailable
	}

	var err error
	var resp *api.GenerateAdminTokenResponse

	for tries := 0; tries < 3; tries++ {
		time.Sleep(time.Duration(tries) * time.Second)
		resp, err = c.dsClient.GenerateAdminToken(context.TODO(), &api.GenerateAdminTokenRequest{
			Name: "admin token for diagnostic tests",
		})

		if err == nil {
			c.Globals.CachedToken = resp.TokenValue
			c.Globals.TokenID = resp.TokenId
			break
		}
	}

	return c.Globals.CachedToken, nil
}

func (c *testContext) CleanupAdminToken() error {
	resp, err := c.DoLBRequest(
		fmt.Sprintf("/apis/iam/v2/tokens/%s", c.Globals.TokenID),
		lbrequest.WithMethod("DELETE"),
	)
	if err != nil {
		return errors.Wrap(err, "Failed to clean up the diagnostics admin token")
	}
	defer resp.Body.Close()

	if resp.StatusCode == 200 {
		c.Globals.CachedToken = ""
		c.Globals.TokenID = ""
		return nil
	}

	r := struct {
		Msg string `json:"message"`
	}{}
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return errors.Wrap(err, "Failed to decode delete response")
	}

	// DoLBRequest only catches 500s and 403
	// for any other non-200 status code, we assume something unexpected occurred
	return errors.Errorf("Unexpected response when cleaning up the diagnostics admin token: %d %s", resp.StatusCode, r.Msg)
}
