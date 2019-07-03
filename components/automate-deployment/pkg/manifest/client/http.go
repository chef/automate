package client

import (
	"context"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
)

const (
	defaultLatestManifestURLFmt = "https://packages.chef.io/manifests/%s/automate/latest.json"
	defaultManifestURLFmt       = "https://packages.chef.io/manifests/automate/%s.json"
)

// A HTTP client makes HTTP requests to retrieve and parse a Manifest.  The
// default manifests are stored on S3, but the manifestURLFmt can be
// overridden for testing.
type HTTP struct {
	HTTPClient           *http.Client
	latestManifestURLFmt string
	manifestURLFmt       string
}

// An Opt represent an option that can be passed to NewClient
type Opt func(c *HTTP)

// NewHTTPClient returns a client with the given options applied.
func NewHTTPClient(options ...Opt) *HTTP {
	c := &HTTP{
		HTTPClient: &http.Client{},
	}

	for _, option := range options {
		option(c)
	}

	if c.latestManifestURLFmt == "" {
		c.latestManifestURLFmt = defaultLatestManifestURLFmt
	}

	if c.manifestURLFmt == "" {
		c.manifestURLFmt = defaultManifestURLFmt
	}

	return c
}

// CurrentURLFormat returns an Opt that can be passed to NewClient which sets
// the latestManifestURLFmt to the given string. Used in testing.
func LatestURLFormat(urlFormat string) Opt {
	return func(c *HTTP) {
		c.latestManifestURLFmt = urlFormat
	}
}

// URLFormat returns an Opt that can be passed to NewClient which sets
// the manifestURLFmt to the given string. Used in testing.
func URLFormat(urlFormat string) Opt {
	return func(c *HTTP) {
		c.manifestURLFmt = urlFormat
	}
}

// GetCurrentManifest retrieves the current manifest for the given
// channel.
func (c *HTTP) GetCurrentManifest(ctx context.Context, channel string) (*manifest.A2, error) {
	url := fmt.Sprintf(c.latestManifestURLFmt, channel)
	return c.manifestFromURL(ctx, url)
}

// GetCurrentManifest retrieves the current manifest for the given
// channel.
func (c *HTTP) GetManifest(ctx context.Context, release string) (*manifest.A2, error) {
	url := fmt.Sprintf(c.manifestURLFmt, release)
	return c.manifestFromURL(ctx, url)
}

func (c *HTTP) manifestFromURL(ctx context.Context, url string) (*manifest.A2, error) {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, err
	}

	req = req.WithContext(ctx)

	response, err := c.HTTPClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer response.Body.Close()

	switch response.StatusCode {
	case http.StatusOK:
		// Yay!
	case http.StatusNotFound:
		return nil, manifest.NewErrNoSuchManifest(errors.Errorf("%s: %s", url, response.Status))
	default:
		return nil, errors.Errorf("Unexpected HTTP response from %s: %s", url, response.Status)
	}

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return nil, err
	}

	m, err := parser.ManifestFromBytes(body)
	if err != nil {
		return nil, err
	}

	m.HartOverrides = []habpkg.Hart{}

	return m, nil
}
