// +build prod

// This package satisfies all the "mock" interfaces we employ in non-prod-builds
// but always fails.

// Note (2017/12/05) sr: Alternatively, we could have relied on `init()` funcs,
// to support some kind of self-registration of the mock modules; but it turned
// out to be too cumbersome for what's to gain.
package mock

import (
	"errors"
	"net/url"

	"go.uber.org/zap"

	"github.com/chef/automate/components/authn-service/authenticator"
	"github.com/chef/automate/lib/tls/certs"
)

var mockErr = errors.New("mock not included in production build")

type mock struct{}

type OidcConfig struct { // "mock-oidc"
	*mock
}

type StaticConfig struct { // "mock-static"
	*mock
}

type HeaderTokenConfig struct { // "mock-header-token"
	*mock
}

func (*mock) Open(_ *url.URL, _ *certs.ServiceCerts, _ *zap.Logger) (authenticator.Authenticator, error) {
	return nil, mockErr
}
