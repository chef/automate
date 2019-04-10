// +build !prod

package mock

import (
	"errors"
	"net/http"
	"net/url"

	"go.uber.org/zap"

	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/lib/tls/certs"
)

// HeaderTokenConfig is used for configuring static header-token authenticators
type HeaderTokenConfig struct {
	Header string            `json:"header"`
	Tokens map[string]string `json:"tokens"`
}

// HeaderTokenAuthenticator is used for configuring static header-token authenticators
type HeaderTokenAuthenticator struct {
	header string
	tokens map[string]string
	logger *zap.Logger
}

type htaRequestor struct {
	id string
}

func (r *htaRequestor) Subject() string {
	return "token:" + r.id
}

func (*htaRequestor) Teams() []string {
	return nil
}

// NewHeaderTokenAuthenticator returns a mock authenticator which requires no server
// interaction. It only checks hard-coded tokens from the configured header.
func NewHeaderTokenAuthenticator(
	header string,
	tokens map[string]string,
	logger *zap.Logger) authenticator.Authenticator {
	return &HeaderTokenAuthenticator{
		header: header,
		tokens: tokens,
		logger: logger,
	}
}

// Open returns an header token authenticator
func (c *HeaderTokenConfig) Open(u *url.URL, _ *certs.ServiceCerts,
	logger *zap.Logger) (authenticator.Authenticator, error) {

	return NewHeaderTokenAuthenticator(c.Header, c.Tokens, logger), nil
}

// Authenticate processes the passed request, checking if the configured
// header's FIRST value matches any of the the hard-coded tokens list.
func (a *HeaderTokenAuthenticator) Authenticate(r *http.Request) (authenticator.Requestor, error) {
	token := r.Header.Get(a.header)
	if token == "" {
		// error out early
		return nil, errors.New("no token in request")
	}

	for clientID, clientToken := range a.tokens {
		if token == clientToken {
			return &htaRequestor{id: clientID}, nil
		}
	}
	// not authenticated, no error
	return nil, nil
}
