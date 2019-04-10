package server

import (
	"context"
	"fmt"

	"github.com/pkg/errors"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// GenerateAdminToken returns a new API token with admin-level
// access (aka access to the entire API).
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
	if err != nil {
		return nil, errors.Wrap(err, "initialize AuthN client")
	}

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

	authzClient := authz.NewAuthorizationClient(authzConnection)

	response, err := authnClient.CreateToken(ctx, &authn.CreateTokenReq{
		Description: req.Description,
		Active:      true,
	})
	if err != nil {
		return nil, errors.Wrap(err, "create API token")
	}

	_, err = authzClient.CreatePolicy(ctx, &authz.CreatePolicyReq{
		Action:   "*",
		Subjects: []string{fmt.Sprintf("token:%s", response.Id)},
		Resource: "*",
	})
	if err != nil {
		// Attempt to be transactional
		_, deleteTokenError := authnClient.DeleteToken(ctx, &authn.DeleteTokenReq{
			Id: response.Id,
		})
		if deleteTokenError != nil {
			return nil, errors.Wrap(deleteTokenError,
				"failed to permission API token and rollback of token creation failed")
		}

		return nil, errors.Wrap(err, "permission API token")
	}

	return &api.GenerateAdminTokenResponse{
		ApiToken: response.Value,
	}, nil
}
