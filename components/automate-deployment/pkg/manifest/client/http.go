package client

import (
	"bytes"
	"context"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/crypto/openpgp"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
)

const (
	defaultSemanticManifestURLFmt = "https://packages.chef.io/manifests/%s/automate/latest_semver.json"
	defaultLatestManifestURLFmt   = "https://packages.chef.io/manifests/%s/automate/latest.json"
	defaultManifestURLFmt         = "https://packages.chef.io/manifests/automate/%s.json"
	packagesChefIOSigAsc          = `-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1.4.12 (Darwin)
Comment: GPGTools - http://gpgtools.org
	
mQGiBEppC7QRBADfsOkZU6KZK+YmKw4wev5mjKJEkVGlus+NxW8wItX5sGa6kdUu
twAyj7Yr92rF+ICFEP3gGU6+lGo0Nve7KxkN/1W7/m3G4zuk+ccIKmjp8KS3qn99
dxy64vcji9jIllVa+XXOGIp0G8GEaj7mbkixL/bMeGfdMlv8Gf2XPpp9vwCgn/GC
JKacfnw7MpLKUHOYSlb//JsEAJqao3ViNfav83jJKEkD8cf59Y8xKia5OpZqTK5W
ShVnNWS3U5IVQk10ZDH97Qn/YrK387H4CyhLE9mxPXs/ul18ioiaars/q2MEKU2I
XKfV21eMLO9LYd6Ny/Kqj8o5WQK2J6+NAhSwvthZcIEphcFignIuobP+B5wNFQpe
DbKfA/0WvN2OwFeWRcmmd3Hz7nHTpcnSF+4QX6yHRF/5BgxkG6IqBIACQbzPn6Hm
sMtm/SVf11izmDqSsQptCrOZILfLX/mE+YOl+CwWSHhl+YsFts1WOuh1EhQD26aO
Z84HuHV5HFRWjDLw9LriltBVQcXbpfSrRP5bdr7Wh8vhqJTPjrQnT3BzY29kZSBQ
YWNrYWdlcyA8cGFja2FnZXNAb3BzY29kZS5jb20+iGAEExECACAFAkppC7QCGwMG
CwkIBwMCBBUCCAMEFgIDAQIeAQIXgAAKCRApQKupg++Caj8sAKCOXmdG36gWji/K
+o+XtBfvdMnFYQCfTCEWxRy2BnzLoBBFCjDSK6sJqCu0IENIRUYgUGFja2FnZXMg
PHBhY2thZ2VzQGNoZWYuaW8+iGIEExECACIFAlQwYFECGwMGCwkIBwMCBhUIAgkK
CwQWAgMBAh4BAheAAAoJEClAq6mD74JqX94An26z99XOHWpLN8ahzm7cp13t4Xid
AJ9wVcgoUBzvgg91lKfv/34cmemZn7kCDQRKaQu0EAgAg7ZLCVGVTmLqBM6njZEd
Zbv+mZbvwLBSomdiqddE6u3eH0X3GuwaQfQWHUVG2yedyDMiG+EMtCdEeeRebTCz
SNXQ8Xvi22hRPoEsBSwWLZI8/XNg0n0f1+GEr+mOKO0BxDB2DG7DA0nnEISxwFkK
OFJFebR3fRsrWjj0KjDxkhse2ddU/jVz1BY7Nf8toZmwpBmdozETMOTx3LJy1HZ/
Te9FJXJMUaB2lRyluv15MVWCKQJro4MQG/7QGcIfrIZNfAGJ32DDSjV7/YO+IpRY
IL4CUBQ65suY4gYUG4jhRH6u7H1p99sdwsg5OIpBe/v2Vbc/tbwAB+eJJAp89Zeu
twADBQf/ZcGoPhTGFuzbkcNRSIz+boaeWPoSxK2DyfScyCAuG41CY9+g0HIw9Sq8
DuxQvJ+vrEJjNvNE3EAEdKl/zkXMZDb1EXjGwDi845TxEMhhD1dDw2qpHqnJ2mtE
WpZ7juGwA3sGhi6FapO04tIGacCfNNHmlRGipyq5ZiKIRq9mLEndlECr8cwaKgkS
0wWu+xmMZe7N5/t/TK19HXNh4tVacv0F3fYK54GUjt2FjCQV75USnmNY4KPTYLXA
dzC364hEMlXpN21siIFgB04w+TXn5UF3B4FfAy5hevvr4DtV4MvMiGLu0oWjpaLC
MpmrR3Ny2wkmO0h+vgri9uIP06ODWIhJBBgRAgAJBQJKaQu0AhsMAAoJEClAq6mD
74Jq4hIAoJ5KrYS8kCwj26SAGzglwggpvt3CAJ0bekyky56vNqoegB+y4PQVDv4K
zA==
=IxPr
-----END PGP PUBLIC KEY BLOCK-----`
)

var packagesChefIOKeyRing openpgp.EntityList

func init() {
	entityList, err := openpgp.ReadArmoredKeyRing(strings.NewReader(packagesChefIOSigAsc))
	if err != nil {
		panic(errors.Wrap(err, "Failed to read packages.chef.io public key"))
	}
	packagesChefIOKeyRing = entityList
}

// A HTTP client makes HTTP requests to retrieve and parse a Manifest.  The
// default manifests are stored on S3, but the manifestURLFmt can be
// overridden for testing.
type HTTP struct {
	HTTPClient                   *http.Client
	latestManifestURLFmt         string
	latestSemanticManifestURLFmt string
	manifestURLFmt               string
	noVerify                     bool
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

	if c.latestSemanticManifestURLFmt == "" {
		c.latestSemanticManifestURLFmt = defaultSemanticManifestURLFmt
	}

	if c.latestManifestURLFmt == "" {
		c.latestManifestURLFmt = defaultLatestManifestURLFmt
	}

	if c.manifestURLFmt == "" {
		c.manifestURLFmt = defaultManifestURLFmt
	}

	// We allow skipping manifest verification if needed by setting this environment
	// variable. Set it only if you must
	if os.Getenv("CHEF_AUTOMATE_SKIP_MANIFEST_VERIFICATION") == "true" {
		c.noVerify = true
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

func LatestSemanticURLFormat(urlFormat string) Opt {
	return func(c *HTTP) {
		c.latestSemanticManifestURLFmt = urlFormat
	}
}

// URLFormat returns an Opt that can be passed to NewClient which sets
// the manifestURLFmt to the given string. Used in testing.
func URLFormat(urlFormat string) Opt {
	return func(c *HTTP) {
		c.manifestURLFmt = urlFormat
	}
}

// NoVerify disables signature verification of the manifest
func NoVerify(noVerify bool) Opt {
	return func(c *HTTP) {
		c.noVerify = noVerify
	}
}

// GetCurrentManifest retrieves the current manifest for the given
// channel.
func (c *HTTP) GetCurrentManifest(ctx context.Context, channel string) (*manifest.A2, error) {
	//try to get semantic version manifest
	url := fmt.Sprintf(c.latestSemanticManifestURLFmt, channel)
	m, err := c.manifestFromURL(ctx, url)
	if err == nil && m.SemVersion != "" && m.SchemaVersion == "2" {
		return m, nil
	}
	if (m != nil && (m.SemVersion == "" || m.SchemaVersion == "1")) || strings.Contains(err.Error(), "failed to locate manifest") {
		//since received error in fetching semantic version, try to fetch timestamp versioned manifest
		url = fmt.Sprintf(c.latestManifestURLFmt, channel)
		return c.manifestFromURL(ctx, url)
	}

	return nil, err
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
	defer response.Body.Close() // nolint: errcheck

	switch response.StatusCode {
	case http.StatusOK:
		// Yay!
	case http.StatusNotFound:
		return nil, manifest.NewNoSuchManifestError(errors.Errorf("%s: %s", url, response.Status))
	default:
		return nil, errors.Errorf("Unexpected HTTP response from %s: %s", url, response.Status)
	}

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return nil, err
	}

	if !c.noVerify {
		signatureURL := fmt.Sprintf("%s.asc", url)
		logrus.WithField("url", signatureURL).Debug("Checking manifest signature")
		sigReq, err := http.NewRequest("GET", signatureURL, nil)
		if err != nil {
			return nil, errors.Wrap(err, "failed to GET manifest signature")
		}
		sigReq = sigReq.WithContext(ctx)

		sigResp, err := c.HTTPClient.Do(sigReq)
		if err != nil {
			return nil, err
		}
		defer sigResp.Body.Close() // nolint: errcheck

		if sigResp.StatusCode != http.StatusOK {
			return nil, errors.Errorf("Failed to GET manifest signature. status=%s", sigResp.Status)
		}

		sigBody, err := ioutil.ReadAll(sigResp.Body)
		if err != nil {
			return nil, errors.Wrap(err, "failed to read signature response")
		}

		_, err = openpgp.CheckArmoredDetachedSignature(packagesChefIOKeyRing,
			bytes.NewBuffer(body), bytes.NewBuffer(sigBody))
		if err != nil {
			return nil, errors.Wrap(err,
				"Failed to verify manifest signature. This may indicate that the manifest is corrupt or has been tampered with. If this problem persists, please contact Chef Support")
		}
	}

	m, err := parser.ManifestFromBytes(body)
	if err != nil {
		return nil, err
	}

	m.HartOverrides = []habpkg.Hart{}

	return m, nil
}
