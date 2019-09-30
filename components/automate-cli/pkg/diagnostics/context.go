package diagnostics

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"io"
	"net/http"
	"net/url"

	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/deployment"
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
	// GetOption returns the Option for the given key
	GetOption(key string) *Option
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

	req, err := lbrequest.New(path, opts...)

	if err != nil {
		return nil, err
	}

	return c.httpClient.Do(req)
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
