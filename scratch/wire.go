// +build wireinject

package scratch

import (
	"context"

	"github.com/google/wire"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/lib/grpc/secureconn"
)

func Setup() (*authz.Policy, func(), error) {
	wire.Build(
		context.Background,
		wire.Bind(new(grpc.ClientConnInterface), new(*grpc.ClientConn)),
		DevCerts, Conn, secureconn.NewFactory, FactoryOpts,
		authn.NewTokensMgmtClient, Token,
		authz.NewPoliciesClient, Policy)
	return nil, nil, nil
}
