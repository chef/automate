package server

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	secrets "github.com/chef/automate/api/external/secrets"
	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/test/helpers"
	"github.com/chef/automate/lib/tracing"

	v1 "github.com/chef/automate/components/infra-proxy-service/server/v1"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres/migration"
)

func TestHealthGRPC(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	_, _, conn, close, _ := setupInfraProxyService(ctx, t, l, nil)
	defer close()

	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		require.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}

func setupInfraProxyService(ctx context.Context,
	t *testing.T, l logger.Logger,
	migrationConfig *migration.Config) (*v1.Server, *service.Service, *grpc.ClientConn, func(), *authz.SubjectPurgeServerMock) {

	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, "infra-proxy-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()

	secretsCerts := helpers.LoadDevCerts(t, "secrets-service")
	secretsConnFactory := secureconn.NewFactory(*secretsCerts)
	grpcSecrets := secretsConnFactory.NewServer()

	mockCommon := authz.NewSubjectPurgeServerMock()
	mockCommon.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
	authz.RegisterSubjectPurgeServer(grpcAuthz, mockCommon)

	mockV2Authz := authz_v2.NewAuthorizationServerMock()
	mockV2Authz.ValidateProjectAssignmentFunc = defaultValidateProjectAssignmentFunc
	authz_v2.RegisterAuthorizationServer(grpcAuthz, mockV2Authz)

	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)

	authzClient := authz.NewSubjectPurgeClient(authzConn)
	authzV2AuthorizationClient := authz_v2.NewAuthorizationClient(authzConn)

	secretsServer := grpctest.NewServer(grpcSecrets)
	secretsConn, err := secretsConnFactory.Dial("secrets-service", secretsServer.URL)
	require.NoError(t, err)

	secretsClient := secrets.NewSecretsServiceClient(secretsConn)

	serviceRef, err := service.Start(l, *migrationConfig, connFactory, secretsClient,
		authzClient, authzV2AuthorizationClient)

	if err != nil {
		t.Fatalf("could not create server: %s", err)
	}
	grpcServ := serviceRef.ConnFactory.NewServer(tracing.GlobalServerInterceptor())
	server := v1.NewServer(serviceRef)
	infra_proxy.RegisterInfraProxyServer(grpcServ, server)

	resetState(ctx, t, serviceRef)

	g := grpctest.NewServer(grpcServ)

	conn, err := connFactory.Dial("infra-proxy-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	return server, serviceRef, conn, func() { g.Close(); authzServer.Close() }, mockCommon
}

func resetState(ctx context.Context, t *testing.T, serviceRef *service.Service) {
	t.Helper()

	if r, ok := serviceRef.Storage.(storage.Resetter); ok {
		err := r.Reset(ctx)
		require.NoError(t, err)
	}
}

func defaultMockPurgeFunc(context.Context,
	*authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	return &authz.PurgeSubjectFromPoliciesResp{}, nil
}

func defaultValidateProjectAssignmentFunc(context.Context,
	*authz_v2.ValidateProjectAssignmentReq) (*authz_v2.ValidateProjectAssignmentResp, error) {
	return &authz_v2.ValidateProjectAssignmentResp{}, nil
}

func insertProjectsIntoNewContext(projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(auth_context.NewContext(context.Background(),
		[]string{}, projects, "resource", "action", "pol"))
}
