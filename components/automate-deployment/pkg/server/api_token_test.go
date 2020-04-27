package server

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authn"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestGenerateAdminToken(t *testing.T) {
	ctx := context.Background()

	// https://github.com/chef/automate/blob/master/components/automate-grpc/protoc-gen-grpc-mock/README.md
	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	mockAuthN := authn.NewTokensMgmtServerMock()
	connFactory := secureconn.NewFactory(*serviceCerts)
	g := connFactory.NewServer()
	authn.RegisterTokensMgmtServer(g, mockAuthN)
	authnServer := grpctest.NewServer(g)
	defer authnServer.Close()

	serviceCerts = helpers.LoadDevCerts(t, "authz-service")
	mockV2PolicyServer := authz_v2.NewPoliciesServerMock()
	connFactory = secureconn.NewFactory(*serviceCerts)
	g = connFactory.NewServer()
	authz_v2.RegisterPoliciesServer(g, mockV2PolicyServer)
	authzServer := grpctest.NewServer(g)
	defer authzServer.Close()

	devCerts := helpers.LoadDevCerts(t, "deployment-service")
	connFactory = secureconn.NewFactory(*devCerts)

	testTokenString := "some-token"
	testID := "some-guid"
	testName := "some name of our admin token"

	t.Run("when API token and v2 policy creation succeed", func(t *testing.T) {
		mockAuthN.CreateTokenFunc = func(
			_ context.Context, req *authn.CreateTokenReq) (*authn.Token, error) {

			assert.True(t, req.Active)
			assert.Equal(t, testName, req.Name)

			return &authn.Token{
				Value: testTokenString,
				Id:    testID,
			}, nil
		}

		mockV2PolicyServer.AddPolicyMembersFunc = func(
			_ context.Context, req *authz_v2.AddPolicyMembersReq) (*authz_v2.AddPolicyMembersResp, error) {

			assert.Equal(t, "administrator-access", req.Id)

			return &authz_v2.AddPolicyMembersResp{
				Members: []string{"team:local:admins", "token:" + testID},
			}, nil
		}

		req := &api.GenerateAdminTokenRequest{Name: testName}
		resp, err := generateAdminToken(ctx, req, connFactory, authnServer.URL, authzServer.URL)
		require.NoError(t, err)
		require.NotNil(t, resp)
		assert.Equal(t, testID, resp.TokenId)
	})

	t.Run("when API token succeeds but policy creation fails and rollback fails", func(t *testing.T) {
		mockAuthN.CreateTokenFunc = func(
			_ context.Context, req *authn.CreateTokenReq) (*authn.Token, error) {

			assert.True(t, req.Active)
			assert.Equal(t, testName, req.Name)

			return &authn.Token{
				Value: testTokenString,
				Id:    testID,
			}, nil
		}

		mockV2PolicyServer.AddPolicyMembersFunc = func(
			_ context.Context, req *authz_v2.AddPolicyMembersReq) (*authz_v2.AddPolicyMembersResp, error) {

			assert.Equal(t, "administrator-access", req.Id)

			return nil, status.Error(codes.Internal, "unexpected error")
		}

		mockAuthN.DeleteTokenFunc = func(
			_ context.Context, req *authn.DeleteTokenReq) (*authn.DeleteTokenResp, error) {

			assert.Equal(t, testID, req.Id)

			return nil, status.Error(codes.Internal, "unexpected error")
		}

		req := &api.GenerateAdminTokenRequest{
			Name: testName,
		}

		resp, err := generateAdminToken(ctx, req, connFactory, authnServer.URL, authzServer.URL)
		require.Nil(t, resp)
		require.Error(t, err)
	})
}
