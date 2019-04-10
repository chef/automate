package server

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
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
	mockAuthZ := authz.NewAuthorizationServerMock()
	connFactory = secureconn.NewFactory(*serviceCerts)
	g = connFactory.NewServer()
	authz.RegisterAuthorizationServer(g, mockAuthZ)
	authzServer := grpctest.NewServer(g)
	defer authzServer.Close()

	devCerts := helpers.LoadDevCerts(t, "deployment-service")
	connFactory = secureconn.NewFactory(*devCerts)

	testTokenString := "some-token"
	testID := "some-guid"
	testSubjectString := "token:some-guid"
	testDescription := "some description of our admin token"

	t.Run("when API token and policy creation succeed", func(t *testing.T) {
		mockAuthN.CreateTokenFunc = func(
			_ context.Context, req *authn.CreateTokenReq) (*authn.Token, error) {

			assert.True(t, req.Active)
			assert.Equal(t, testDescription, req.Description)

			return &authn.Token{
				Value: testTokenString,
				Id:    testID,
			}, nil
		}

		mockAuthZ.CreatePolicyFunc = func(
			_ context.Context, req *authz.CreatePolicyReq) (*authz.CreatePolicyResp, error) {

			assert.Equal(t, []string{testSubjectString}, req.Subjects)
			assert.Equal(t, "*", req.Action)
			assert.Equal(t, "*", req.Resource)

			return &authz.CreatePolicyResp{}, nil
		}

		mockAuthN.DeleteTokenFunc = func(context.Context, *authn.DeleteTokenReq) (*authn.DeleteTokenResp, error) {
			return nil, status.Error(codes.FailedPrecondition,
				"DeleteToken should never be called because we aren't rolling back.")
		}

		req := &api.GenerateAdminTokenRequest{Description: testDescription}
		resp, err := generateAdminToken(ctx, req, connFactory, authnServer.URL, authzServer.URL)
		require.NoError(t, err)
		require.NotNil(t, resp)
		assert.Equal(t, testTokenString, resp.ApiToken)
	})

	t.Run("when API token fails", func(t *testing.T) {
		mockAuthN.CreateTokenFunc = func(
			_ context.Context, req *authn.CreateTokenReq) (*authn.Token, error) {

			assert.True(t, req.Active)
			assert.Equal(t, testDescription, req.Description)

			return nil, status.Error(codes.Internal, "unexpected error")
		}

		mockAuthZ.CreatePolicyFunc = func(
			context.Context, *authz.CreatePolicyReq) (*authz.CreatePolicyResp, error) {
			return nil, status.Error(codes.FailedPrecondition,
				"CreatePolicy should never be called because we are expecting CreateToken to fail.")
		}

		mockAuthN.DeleteTokenFunc = func(context.Context, *authn.DeleteTokenReq) (*authn.DeleteTokenResp, error) {
			return nil, status.Error(codes.FailedPrecondition,
				"DeleteToken should never be called because we aren't rolling back.")
		}

		req := &api.GenerateAdminTokenRequest{
			Description: testDescription,
		}

		resp, err := generateAdminToken(ctx, req, connFactory, authnServer.URL, authzServer.URL)
		require.Nil(t, resp)
		require.Error(t, err)
	})

	t.Run("when API token succeeds but policy creation fails but rollback succeeds", func(t *testing.T) {
		mockAuthN.CreateTokenFunc = func(
			_ context.Context, req *authn.CreateTokenReq) (*authn.Token, error) {

			assert.True(t, req.Active)
			assert.Equal(t, testDescription, req.Description)

			return &authn.Token{
				Value: testTokenString,
				Id:    testID,
			}, nil
		}

		mockAuthZ.CreatePolicyFunc = func(
			_ context.Context, req *authz.CreatePolicyReq) (*authz.CreatePolicyResp, error) {

			assert.Equal(t, []string{testSubjectString}, req.Subjects)
			assert.Equal(t, "*", req.Action)
			assert.Equal(t, "*", req.Resource)

			return nil, status.Error(codes.Internal, "unexpected error")
		}

		mockAuthN.DeleteTokenFunc = func(
			_ context.Context, req *authn.DeleteTokenReq) (*authn.DeleteTokenResp, error) {

			assert.Equal(t, testID, req.Id)

			return &authn.DeleteTokenResp{}, nil
		}

		req := &api.GenerateAdminTokenRequest{Description: testDescription}
		resp, err := generateAdminToken(ctx, req, connFactory, authnServer.URL, authzServer.URL)
		require.Nil(t, resp)
		require.Error(t, err)
	})

	t.Run("when API token succeeds but policy creation fails and rollback fails", func(t *testing.T) {
		mockAuthN.CreateTokenFunc = func(
			_ context.Context, req *authn.CreateTokenReq) (*authn.Token, error) {

			assert.True(t, req.Active)
			assert.Equal(t, testDescription, req.Description)

			return &authn.Token{
				Value: testTokenString,
				Id:    testID,
			}, nil
		}

		mockAuthZ.CreatePolicyFunc = func(
			_ context.Context, req *authz.CreatePolicyReq) (*authz.CreatePolicyResp, error) {

			assert.Equal(t, []string{testSubjectString}, req.Subjects)
			assert.Equal(t, "*", req.Action)
			assert.Equal(t, "*", req.Resource)

			return nil, status.Error(codes.Internal, "unexpected error")
		}

		mockAuthN.DeleteTokenFunc = func(
			_ context.Context, req *authn.DeleteTokenReq) (*authn.DeleteTokenResp, error) {

			assert.Equal(t, testID, req.Id)

			return nil, status.Error(codes.Internal, "unexpected error")
		}

		req := &api.GenerateAdminTokenRequest{
			Description: testDescription,
		}

		resp, err := generateAdminToken(ctx, req, connFactory, authnServer.URL, authzServer.URL)
		require.Nil(t, resp)
		require.Error(t, err)
	})
}
