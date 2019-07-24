package oidc

import (
	"context"
	"crypto/tls"
	"fmt"
	"net/http"
	"net/url"
	"time"

	"go.uber.org/zap"
	"golang.org/x/oauth2"

	"github.com/coreos/go-oidc"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/components/authn-service/constants"
	"github.com/chef/automate/lib/httputils"
	util "github.com/chef/automate/lib/oidc"
	"github.com/chef/automate/lib/tls/certs"
)

type requestor struct {
	userID, connID string
	teams          []string
}

func (r *requestor) Subject() string {
	return fmt.Sprintf("user:%s:%s", r.connID, r.userID)
}

func (r *requestor) Teams() []string {
	teams := make([]string, len(r.teams))
	for i, team := range r.teams {
		teams[i] = "team:" + r.connID + ":" + team
	}
	return teams
}

type localRequestor struct {
	email string

	requestor
}

// ensure we satisfy the interface
var _ authenticator.LocalUser = (*localRequestor)(nil)

func (l *localRequestor) Subject() string {
	return fmt.Sprintf("user:local:%s", l.email)
}

func (l *localRequestor) AppendTeams(teams []string) {
	l.teams = append(l.teams, teams...)
}

func (l *localRequestor) UserID() string {
	return l.userID
}

// Config is used for configuring oidc authenticators
type Config struct {
	Issuer   string `json:"issuer"`
	ClientID string `json:"client_id"`
}

// Authenticator is used for configuring oidc authenticators
type Authenticator struct {
	ctx      context.Context
	cancel   context.CancelFunc
	verifier IDTokenVerifier
	logger   *zap.Logger
}

// NewAuthenticator returns an oidc authenticator that does full ID token
// validation using the provider's pub keys.
func NewAuthenticator(
	issuer, clientID string,
	upstream *url.URL,
	skipExpiry bool,
	retrySeconds time.Duration,
	serviceCerts *certs.ServiceCerts,
	logger *zap.Logger) (authenticator.Authenticator, error) {
	ctx, cancel := context.WithCancel(context.Background())

	// inject custom client based on well-known context key (oauth2.HTTPClient)
	client := customClient("automate-dex", upstream, serviceCerts, logger)
	ctx = context.WithValue(ctx, oauth2.HTTPClient, client)

	var provider *oidc.Provider
	var err error
	retryTime := retrySeconds * time.Second
	for {
		provider, err = oidc.NewProvider(ctx, issuer)
		if err == nil {
			break
		}

		if retryTime == 0 {
			cancel()
			return nil, errors.Wrap(err, "oidc: failed to get provider")
		}

		logger.Info("attempting to connect to OIDC Issuer", zap.Duration("retry_seconds", retrySeconds))
		time.Sleep(retryTime)
	}
	logger.Info("success connecting to issuer")
	return &Authenticator{
		verifier: newVerifier(provider, clientID, skipExpiry),
		ctx:      ctx,
		cancel:   cancel,
		logger:   logger,
	}, nil
}

// customClient used to check the issuer: if it's localhost, it was assumed to
// be a local development setup, and requests would have been overridden to go
// to automate-load-balancer instead of the actual issuer.
// However, since the path through the load-balancer should always be available
// AND allows us to ignore whatever broken cert/external-fqdn setup the
// deployment has, this is now _always_ overridden.
func customClient(targetServiceName string, upstream *url.URL, serviceCerts *certs.ServiceCerts,
	logger *zap.Logger) *http.Client {

	customTransport := httputils.NewDefaultTransport()
	customTransport.TLSClientConfig = &tls.Config{
		ServerName:   targetServiceName,
		Certificates: []tls.Certificate{*serviceCerts.ServiceKeyPair},
		RootCAs:      serviceCerts.NewCertPool(),
	}

	return &http.Client{Transport: &overrideLocalTransport{
		upstream:  upstream,
		transport: customTransport,
		logger:    logger,
	}}
}

type overrideLocalTransport struct {
	upstream  *url.URL
	transport *http.Transport
	logger    *zap.Logger
}

func (tr *overrideLocalTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	old := req.URL.String()
	req.URL.Scheme = tr.upstream.Scheme
	req.URL.Host = tr.upstream.Host
	newURL, err := tr.upstream.Parse(req.URL.Path)
	if err != nil {
		return nil, errors.Wrap(err, "override destination")
	}
	req.URL.Path = newURL.Path
	tr.logger.Debug("RoundTrip override", zap.String("old", old), zap.String("new", req.URL.String()))

	return tr.transport.RoundTrip(req)
}

// Open returns an OIDC authenticator for the configured settings
func (c *Config) Open(upstream *url.URL, serviceCerts *certs.ServiceCerts,
	logger *zap.Logger) (authenticator.Authenticator, error) {

	// create child logger, with added fields
	logger = logger.With(
		zap.String("issuer", c.Issuer),
		zap.String("client_id", c.ClientID))
	return NewAuthenticator(c.Issuer, c.ClientID, upstream, false, 1, serviceCerts, logger)
}

// Authenticate processes the passed request, validating its ID token in the
// `Authorization: Bearer XY` header using the provider configured in `Open()`.
// This will check the issuer
func (a *Authenticator) Authenticate(r *http.Request) (authenticator.Requestor, error) {
	rawIDToken, err := util.ExtractBearerToken(r)
	if err != nil {
		return nil, errors.Wrap(err, "oidc: failed to extract bearer token")
	}
	idToken, err := a.verifier.Verify(r.Context(), rawIDToken)
	if err != nil {
		return nil, errors.Wrap(err, "oidc: failed to verify ID Token")
	}

	var claims struct {
		Email           string   `json:"email"` // only care for emails of local users
		Groups          []string `json:"groups"`
		FederatedClaims struct {
			ConnectorID string `json:"connector_id"`
			UserID      string `json:"user_id"`
		} `json:"federated_claims"`
	}
	err = idToken.Claims(&claims)
	if err != nil {
		return nil, err
	}
	req := requestor{
		connID: claims.FederatedClaims.ConnectorID,
		userID: claims.FederatedClaims.UserID,
		teams:  claims.Groups,
	}
	if claims.FederatedClaims.ConnectorID == constants.LocalConnectorID {
		return &localRequestor{email: claims.Email, requestor: req}, nil
	}

	return &req, nil
}
