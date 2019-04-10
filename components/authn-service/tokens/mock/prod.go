// +build prod

package mock

import (
	"github.com/pkg/errors"
	"go.uber.org/zap"

	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/tls/certs"
)

type Config struct{}

func (*Config) Open(_ *certs.ServiceCerts, _ *zap.Logger) (tokens.Storage, error) {
	return nil, errors.New("mock storage not included in production build")
}
