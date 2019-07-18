package server

import (
	"context"
	"fmt"

	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
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
	authzV2Client := authz_v2.NewPoliciesClient(authzConnection)

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
	if isUseV2Error(err) {
		_, err = authzV2Client.CreatePolicy(ctx, &authz_v2.CreatePolicyReq{
			Id:   "diagnostics-admin-token",
			Name: req.Description,
			Statements: []*authz_v2.Statement{
				{
					Effect:    authz_v2.Statement_ALLOW,
					Resources: []string{"*"},
					Actions:   []string{"*"},
				},
			},
			Members: []string{fmt.Sprintf("token:%s", response.Id)},
		})
	}
	if err != nil && status.Convert(err).Code() != codes.AlreadyExists {
		// Attempt to be transactional
		_, deleteTokenError := authnClient.DeleteToken(ctx, &authn.DeleteTokenReq{
			Id: response.Id,
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

func isUseV2Error(err error) bool {
	for _, detail := range status.Convert(err).Details() {
		if _, ok := detail.(*common.ErrorShouldUseV2); ok {
			return true
		}
	}
	return false
}
