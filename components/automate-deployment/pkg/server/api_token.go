package server

import (
	"context"

	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authn"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// GenerateAdminToken returns a new API token with admin-level
// access (aka access to the entire API). This function should only be used internally
// and should never be exposed to the external API.
func (s *server) GenerateAdminToken(ctx context.Context,
	req *api.GenerateAdminTokenRequest) (*api.GenerateAdminTokenResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	authnAddr := s.AddressForService("authn-service")
	authzAddr := s.AddressForService("authz-service")
	return generateAdminToken(ctx, req, s.connFactory, authnAddr, authzAddr)
}

func generateAdminToken(ctx context.Context,
	req *api.GenerateAdminTokenRequest, connFactory *secureconn.Factory,
	authNAddress, authZAddress string) (*api.GenerateAdminTokenResponse, error) {

	ctx = auth_context.NewOutgoingContext(auth_context.NewContext(ctx, []string{"tls:service:deployment-service:internal"}, []string{}, "", "", "v2.1"))
	authnConnection, err := connFactory.DialContext(
		ctx,
		"authn-service",
		authNAddress,
		grpc.WithBlock(),
	)
	if err != nil {
		return nil, errors.Wrap(err, "connecting to authn-service")
	}

	defer authnConnection.Close() // nolint: errcheck

	authnClient := authn.NewTokensMgmtClient(authnConnection)

	authzConnection, err := connFactory.DialContext(
		ctx,
		"authz-service",
		authZAddress,
		grpc.WithBlock(),
	)
	if err != nil {
		return nil, errors.Wrap(err, "connecting to authz-service")
	}

	defer authzConnection.Close() // nolint: errcheck

	authzV2Client := authz_v2.NewPoliciesClient(authzConnection)

	response, err := authnClient.CreateToken(ctx, &authn.CreateTokenReq{
		Description: req.Description,
		Active:      true,
	})
	if err != nil {
		return nil, errors.Wrap(err, "create API token")
	}
	tokenID := response.Id

	_, err = authzV2Client.CreatePolicy(ctx, &authz_v2.CreatePolicyReq{
		Id:   "admin-token-" + tokenID,
		Name: "admin policy for token " + tokenID,
		Statements: []*authz_v2.Statement{
			{
				Effect:    authz_v2.Statement_ALLOW,
				Resources: []string{"*"},
				Actions:   []string{"*"},
				Projects:  []string{"*"},
			},
		},
		Members: []string{"token:" + tokenID},
	})

	if err != nil && status.Convert(err).Code() != codes.AlreadyExists {
		// Attempt to be transactional
		_, deleteTokenError := authnClient.DeleteToken(ctx, &authn.DeleteTokenReq{
			Id: tokenID,
		})
		if deleteTokenError != nil {
			return nil, errors.Wrap(deleteTokenError,
				"failed to permission API token and rollback of token creation failed")
		}
		return nil, errors.Wrap(err, "permission token error")
	}
	return &api.GenerateAdminTokenResponse{
		ApiToken: response.Value,
	}, nil
}
