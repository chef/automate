// +build prod

package mock

import (
	"github.com/pkg/errors"
	"go.uber.org/zap"

	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/tls/certs"
)

type Config struct{}

func (*Config) Open(_ *certs.ServiceCerts, _ *zap.Logger, _ authz_v2.AuthorizationClient) (tokens.Storage, error) {
	return nil, errors.New("mock storage not included in production build")
}
