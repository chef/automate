package scratch

import (
	"context"
	"fmt"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tls/test/helpers"
	"google.golang.org/grpc"
)

func main() {
	pol, cleanup, err := Setup()
	fmt.Printf("policy: %v, err: %v\n", pol, err)
	if err != nil {
		panic(err.Error())
	}
	cleanup()
}

// needed?
func FactoryOpts() []secureconn.FactoryOpt {
	return nil
}

func DevCerts() (certs.ServiceCerts, error) {
	name := "authn-service"
	cfg := certs.TLSConfig{
		CertPath:       helpers.DevCertPath(name),
		KeyPath:        helpers.DevKeyPath(name),
		RootCACertPath: helpers.DevRootCACert(),
	}

	crts, err := cfg.ReadCerts()
	if err != nil {
		return certs.ServiceCerts{}, err
	}
	return *crts, nil
}

func Conn(ctx context.Context, f *secureconn.Factory) (*grpc.ClientConn, error) {
	return f.DialContext(ctx, "authn-service", "127.0.0.1:9091")
}

func Token(ctx context.Context, c authn.TokensMgmtClient) (*authn.Token, func(), error) {
	req := authn.CreateTokenReq{}
	token, err := c.CreateToken(ctx, &req)
	if err != nil {
		return nil, nil, err
	}
	cleanup := func() {
		_, _ = c.DeleteToken(ctx, &authn.DeleteTokenReq{Id: token.Id})
	}
	return token, cleanup, err
}

func Policy(ctx context.Context, c authz.PoliciesClient, tok *authn.Token) (*authz.Policy, func(), error) {
	req := authz.CreatePolicyReq{Id: "test-policy", Name: "test policy",
		Members: []string{tok.Id}}
	pol, err := c.CreatePolicy(ctx, &req)
	if err != nil {
		return nil, nil, err
	}
	cleanup := func() {
		_, _ = c.DeletePolicy(ctx, &authz.DeletePolicyReq{Id: pol.Id})
	}
	return pol, cleanup, err
}
