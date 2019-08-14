package oidc

import (
	"context"
	"crypto/tls"
	"fmt"
	"net/http"
	"net/url"
	"time"

	oidc "github.com/coreos/go-oidc"
	"github.com/pkg/errors"
	"golang.org/x/oauth2"

	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/tls/certs"
)

// These are the scopes we request (no need to configure these so far)
var scopes = []string{"openid profile email offline_access groups federated:id"}

// Client captures the pieces we care about of *oauth2.Config
type Client interface {
	TokenSource(context.Context, *oauth2.Token) oauth2.TokenSource
	AuthCodeURL(string, ...oauth2.AuthCodeOption) string
	Exchange(context.Context, string) (*oauth2.Token, error)
	Verify(context.Context, string) (*oidc.IDToken, error)
	Client() *http.Client
}

type client struct {
	hc       *http.Client
	oauth2   *oauth2.Config
	verifier *oidc.IDTokenVerifier
	log      OIDCLogger
}

// Config contains necessary elements for
// initializing OIDC client
type Config struct {
	ClientID     string
	ClientSecret string
	RedirectURL  *url.URL
	IssuerURL    *url.URL
	DexURL       *url.URL
}

// OIDCLogger captures this package's logging needs
type OIDCLogger interface {
	Warn(...interface{})
}

// New initializes an OIDC client for our purposes
func New(cfg Config, retrySec int, certs *certs.ServiceCerts, l OIDCLogger) (Client, error) {
	httpClient := httpClientForIssuer("automate-dex", cfg.DexURL, certs)
	ctx := context.WithValue(context.Background(), oauth2.HTTPClient, httpClient)

	var provider *oidc.Provider
	var err error
	retryTime := time.Duration(retrySec) * time.Second
	for {
		provider, err = newProvider(ctx, cfg.IssuerURL)
		if err == nil {
			break
		}

		if retryTime == 0 {
			return nil, errors.Wrap(err, "oidc: failed to get provider")
		}

		l.Warn(fmt.Sprintf("attempting to connect to OIDC Issuer, retry in %d seconds (last error %v)", retrySec, err))
		time.Sleep(retryTime)
	}
	oauth2cfg := oauth2.Config{
		ClientID:     cfg.ClientID,
		ClientSecret: cfg.ClientSecret,
		Scopes:       scopes,
		RedirectURL:  cfg.RedirectURL.String(),
		Endpoint:     provider.Endpoint(),
	}

	verifier := provider.Verifier(&oidc.Config{
		// we want to allow for expired tokens => they'll just trigger a refresh
		// anyways.
		SkipExpiryCheck:   true,
		SkipClientIDCheck: true, // we don't care
	})

	return &client{
		hc:       httpClient,
		oauth2:   &oauth2cfg,
		verifier: verifier,
		log:      l}, nil
}

func (c *client) TokenSource(ctx context.Context, t *oauth2.Token) oauth2.TokenSource {
	return oauth2.ReuseTokenSource(t, c.oauth2.TokenSource(c.clientContext(ctx), t))
}

func (c *client) AuthCodeURL(state string, opts ...oauth2.AuthCodeOption) string {
	return c.oauth2.AuthCodeURL(state, opts...)
}

func (c *client) Exchange(ctx context.Context, code string) (*oauth2.Token, error) {
	return c.oauth2.Exchange(c.clientContext(ctx), code)
}

func (c *client) clientContext(ctx context.Context) context.Context {
	return context.WithValue(ctx, oauth2.HTTPClient, c.hc)
}

func (c *client) Verify(ctx context.Context, rawIDToken string) (*oidc.IDToken, error) {
	return c.verifier.Verify(ctx, rawIDToken)
}

func (c *client) Client() *http.Client {
	return c.hc
}

func newProvider(ctx context.Context, issuerURL *url.URL) (*oidc.Provider, error) {
	provider, err := oidc.NewProvider(ctx, issuerURL.String())
	if err != nil {
		return nil, errors.Wrapf(err, "query provider %q", issuerURL.String())
	}
	return provider, nil
}

// This function allows us to "pretend" the issuer really is
// https://EXTERNAL-FQDN/dex, while we're actually connecting to our load
// balancer's internal_http endpoint. This decouples us from whatever TLS cert
// has been used for external-fqdn. We don't even need to able to resolve it.

// It works by injecting a http.Transport that always returns the targetURL as
// "proxy to be used".
func httpClientForIssuer(targetServiceName string, targetURL *url.URL,
	serviceCerts *certs.ServiceCerts) *http.Client {

	customTransport := httputils.NewDefaultTransport()
	customTransport.TLSClientConfig = &tls.Config{
		ServerName:   targetServiceName,
		Certificates: []tls.Certificate{*serviceCerts.ServiceKeyPair},
		RootCAs:      serviceCerts.NewCertPool(),
	}
	hc := http.Client{
		Transport: &localTransport{tr: customTransport, target: targetURL},
	}
	return &hc
}

type localTransport struct {
	target *url.URL
	tr     *http.Transport
}

func (lt *localTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	req.URL.Scheme = lt.target.Scheme
	req.URL.Host = lt.target.Host
	newURL, err := lt.target.Parse(req.URL.Path)
	if err != nil {
		return nil, errors.Wrap(err, "override destination")
	}
	req.URL.Path = newURL.Path

	return lt.tr.RoundTrip(req)
}
