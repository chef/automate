package middleware

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/pem"
	"errors"
	"fmt"
	"net/url"
	"testing"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/peer"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/tls/test/helpers"
)

type testAuthnClient struct {
	err error
}

func (t testAuthnClient) Authenticate(ctx context.Context, in *authn.AuthenticateRequest, opts ...grpc.CallOption) (*authn.AuthenticateResponse, error) {
	if t.err != nil {
		return nil, t.err
	}
	return &authn.AuthenticateResponse{Teams: []string{"sample-auth-response-team"}, Subject: "sample-auth-response-subject"}, nil
}

type testAuthzHandler struct {
	err error
}

func (t testAuthzHandler) Handle(ctx context.Context, subjects []string, projects []string, req interface{}) (context.Context, error) {
	if t.err != nil {
		return nil, t.err
	}
	// Notes:
	// * The real implementation of `Handle` is currently in
	// gateway/middleware/authz/authz.go; it uses this same
	// auth_context.NewContext() to create the context it returns.
	// * This test implementation is just hard coding everything because I didn't
	// need anything more complicated than that. Feel free to modify if that
	// changes.
	resource := "test resource"
	action := "test action"
	return auth_context.NewContext(ctx, subjects, projects, resource, action), nil
}

func TestHeaderAuthValidForClientAndPeer(t *testing.T) {
	albCert, _ := devCertToEncodedAndPeer(t, "automate-load-balancer")
	_, agPeer := devCertToEncodedAndPeer(t, "automate-gateway")
	otherServiceCert, otherServicePeer := devCertToEncodedAndPeer(t, "deployment-service")
	hash := "5fa1b62dea3204dcf87e57627c3742dc81d96a3603665a27afc7b1b122eec6fa"

	// "5fa1b62dea3204dcf87e57627c3742dc81d96a3603665a27afc7b1b122eec6fa"
	cases := map[string]struct {
		ctx           context.Context
		expectFailure bool
	}{
		"directly from metadata": {
			ctx: metadata.NewIncomingContext(
				context.Background(),
				metadata.Pairs("x-client-cert", otherServiceCert))},

		"from gateway-provided metadata if coming in through ALB": {
			ctx: metadata.NewIncomingContext(
				peer.NewContext(context.Background(), agPeer),
				metadata.Pairs("x-client-cert", albCert, "grpcgateway-x-client-cert", otherServiceCert))},

		"direct request NOT from automate-gateway": {
			ctx: peer.NewContext(context.Background(), otherServicePeer),
		},

		"from gateway-provided metadata and NOT coming in through ALB": {
			ctx: metadata.NewIncomingContext(
				context.Background(),
				metadata.Pairs("grpcgateway-x-client-cert", otherServiceCert)),
			expectFailure: true},

		"garbage data from metadata": {
			ctx: metadata.NewIncomingContext(
				context.Background(),
				metadata.Pairs("x-client-cert", "ThisIsNotACert")),
			expectFailure: true},

		"no metadata": {
			ctx:           context.Background(),
			expectFailure: true},

		"no information in  metadata": {
			ctx:           metadata.NewIncomingContext(context.Background(), metadata.Pairs()),
			expectFailure: true},
	}

	for desc, tc := range cases {
		t.Run(desc, func(t *testing.T) {
			name, ok := headerAuthValidForClientAndPeer(tc.ctx)

			require.Equal(t, !tc.expectFailure, ok, "expected operation result=>%v but actual=>%v", !tc.expectFailure, ok)
			if !tc.expectFailure {
				assert.Equal(t, fmt.Sprintf("tls:service:deployment-service:%s", hash), name)
			}
		})
	}
}

func TestComboAuth(t *testing.T) {
	albCert, _ := devCertToEncodedAndPeer(t, "automate-load-balancer")
	agCert, agPeer := devCertToEncodedAndPeer(t, "automate-gateway")
	otherServiceCert, _ := devCertToEncodedAndPeer(t, "deployment-service")

	reqFromALBCtx := metadata.NewIncomingContext(
		peer.NewContext(context.Background(), agPeer),
		metadata.Pairs("x-client-cert", albCert, "grpcgateway-x-client-cert", agCert,
			runtime.MetadataPrefix+"projects", "project1,project2",
		))

	reqDirectToGatewayCtx := metadata.NewIncomingContext(
		peer.NewContext(context.Background(), agPeer),
		metadata.Pairs("x-client-cert", albCert, "grpcgateway-x-client-cert", otherServiceCert,
			runtime.MetadataPrefix+"projects", "project1,project2",
		))

	t.Run("It returns error for authn failure", func(t *testing.T) {
		authnErr := errors.New("sample authn failure")
		a := authInterceptor{
			authn: testAuthnClient{err: authnErr},
			authz: testAuthzHandler{},
		}
		_, err := a.combinedAuth(reqFromALBCtx, nil)
		require.Error(t, err)
		// the upstream returns specific grpc error types for failures, they should
		// not be wrapped or modified
		assert.Equal(t, authnErr, err)
	})
	t.Run("It returns error for authz failure", func(t *testing.T) {
		authzErr := errors.New("sample authz failure")
		a := authInterceptor{
			authn: testAuthnClient{},
			authz: testAuthzHandler{err: authzErr},
		}
		_, err := a.combinedAuth(reqFromALBCtx, nil)
		require.Error(t, err)
		// the upstream returns specific grpc error types for failures, they should
		// not be wrapped or modified
		assert.Equal(t, authzErr, err)
	})
	t.Run("It forwards metadata correctly for successful auth for requests from Automate Load Balancer", func(t *testing.T) {
		a := authInterceptor{
			authn: testAuthnClient{},
			authz: testAuthzHandler{},
		}

		returnedCtx, err := a.combinedAuth(reqFromALBCtx, nil)
		require.NoError(t, err)

		authInfo := auth_context.FromContext(returnedCtx)

		expectedSubj := []string{"sample-auth-response-team", "sample-auth-response-subject"}
		expectedProjects := []string{"project1", "project2"}

		assert.Equal(t, expectedSubj, authInfo.Subjects)
		assert.Equal(t, expectedProjects, authInfo.Projects)
		assert.Equal(t, "test resource", authInfo.Resource)
		assert.Equal(t, "test action", authInfo.Action)
	})

	t.Run("It forwards metadata correctly for successful auth for direct requests using certificate auth", func(t *testing.T) {
		a := authInterceptor{
			authn: testAuthnClient{},
			authz: testAuthzHandler{},
		}

		returnedCtx, err := a.combinedAuth(reqDirectToGatewayCtx, nil)
		require.NoError(t, err)

		authInfo := auth_context.FromContext(returnedCtx)

		expectedSubj := []string{"tls:service:deployment-service:5fa1b62dea3204dcf87e57627c3742dc81d96a3603665a27afc7b1b122eec6fa"}
		expectedProjects := []string{"project1", "project2"}

		assert.Equal(t, expectedSubj, authInfo.Subjects)
		assert.Equal(t, expectedProjects, authInfo.Projects)
		assert.Equal(t, "test resource", authInfo.Resource)
		assert.Equal(t, "test action", authInfo.Action)
	})
}

func devCertToEncodedAndPeer(t *testing.T, service string) (string, *peer.Peer) {
	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, service)
	cert := serviceCerts.ServiceKeyPair.Certificate[0]
	x509Cert, err := x509.ParseCertificate(cert)
	require.NoError(t, err)
	block := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: x509Cert.Raw})
	require.NotNil(t, block)
	p := peer.Peer{
		AuthInfo: credentials.TLSInfo{
			State: tls.ConnectionState{
				VerifiedChains: [][]*x509.Certificate{
					{x509Cert},
				},
			},
		},
	}
	return url.QueryEscape(string(block)), &p
}
