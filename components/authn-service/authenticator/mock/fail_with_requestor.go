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

// StaticFailWithRequestorConfig is used for configuring the fail_with_requestor authenticator
// Note: This config is NOT exposed in server/authenticators.go, so it can't be chosen for any
//       purpose outside of tests -- because it really shouldn't.
type StaticFailWithRequestorConfig struct {
	Requestor string `json:"requestor"`
	ErrorMsg  string `json:"error"`
}

// StaticFailWithRequestorAuthenticator holds the state of the fail_with_requestor authenticator
type StaticFailWithRequestorAuthenticator struct {
	requestor string
	err       error
}

type requestor struct {
	subject string
}

func (r *requestor) Subject() string {
	return r.subject
}

func (*requestor) Teams() []string {
	return nil
}

// NewStaticFailWithRequestorAuthenticator returns a mock authenticator always returns the passed
// requestor. Only for testing.
func NewStaticFailWithRequestorAuthenticator(requestor string, err error) authenticator.Authenticator {
	return &StaticFailWithRequestorAuthenticator{
		requestor: requestor,
		err:       err,
	}
}

// Open returns an authentication strategy that always returns the configured
// requestor AND an error != nil.
func (c *StaticFailWithRequestorConfig) Open(u *url.URL, _ *certs.ServiceCerts,
	logger *zap.Logger) (authenticator.Authenticator, error) {

	return NewStaticFailWithRequestorAuthenticator(c.Requestor, errors.New(c.ErrorMsg)), nil
}

// Authenticate returns the configured requestor AND the configured error != nil
func (a *StaticFailWithRequestorAuthenticator) Authenticate(r *http.Request) (authenticator.Requestor, error) {
	return &requestor{subject: a.requestor}, a.err
}
