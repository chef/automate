package test

import (
	"context"
	"errors"
	"net/url"
	"os"
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	secrets "github.com/chef/automate/api/external/secrets"
	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/test/helpers"
	"github.com/chef/automate/lib/tracing"

	server "github.com/chef/automate/components/infra-proxy-service/server"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres/migration"
)

// MigrationConfigIfPGTestsToBeRun either returns the pg migration config
// if PG_URL is set or we are in CI system, otherwise it returns nil, indicating
// postgres based tests shouldn't be run.
func MigrationConfigIfPGTestsToBeRun(l logger.Logger, migrationPath string) (*migration.Config, error) {
	customPGURL, pgURLPassed := os.LookupEnv("PG_URL")
	ciMode := os.Getenv("CI") == "true"

	// If in CI mode, use the default
	if ciMode {
		pgURL, err := url.Parse("postgres://postgres@127.0.0.1:5432/infra_proxy_test?sslmode=disable")
		if err != nil {
			return nil, err
		}
		return &migration.Config{
			Path:   migrationPath,
			Logger: l,
			PGURL:  pgURL,
		}, nil
	}

	// If PG_URL wasn't passed (and we aren't in CI)
	// we shouldn't run the postgres tests, return nil.
	if !pgURLPassed {
		return nil, errors.New("was not CI and PG_URL not passed")
	}

	pgURL, err := url.Parse(customPGURL)
	if err != nil {
		return nil, err
	}

	return &migration.Config{
		Path:   migrationPath,
		Logger: l,
		PGURL:  pgURL,
	}, nil
}

// SetupInfraProxyService provides the connection service client.
func SetupInfraProxyService(ctx context.Context,
	t *testing.T, l logger.Logger,
	migrationConfig migration.Config) (*server.Server, *service.Service, *grpc.ClientConn, func(), *authz.SubjectPurgeServerMock) {

	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, "infra-proxy-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()

	mockCommon := authz.NewSubjectPurgeServerMock()
	mockCommon.PurgeSubjectFromPoliciesFunc = DefaultMockPurgeFunc
	authz.RegisterSubjectPurgeServer(grpcAuthz, mockCommon)

	mockV2Authz := authz_v2.NewAuthorizationServerMock()
	mockV2Authz.ValidateProjectAssignmentFunc = defaultValidateProjectAssignmentFunc
	authz_v2.RegisterAuthorizationServer(grpcAuthz, mockV2Authz)

	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)

	authzClient := authz.NewSubjectPurgeClient(authzConn)
	authzV2AuthorizationClient := authz_v2.NewAuthorizationClient(authzConn)

	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	serviceRef, err := service.Start(l, migrationConfig, connFactory, secretsClient,
		authzClient, authzV2AuthorizationClient)

	if err != nil {
		t.Fatalf("could not create server: %s", err)
	}
	grpcServ := serviceRef.ConnFactory.NewServer(tracing.GlobalServerInterceptor())
	newServer := server.NewServer(serviceRef)
	infra_proxy.RegisterInfraProxyServer(grpcServ, newServer)
	health.RegisterHealthServer(grpcServ, health.NewService())

	ResetState(ctx, t, serviceRef)

	g := grpctest.NewServer(grpcServ)

	conn, err := connFactory.Dial("infra-proxy-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	return newServer, serviceRef, conn, func() { g.Close(); authzServer.Close() }, mockCommon
}

// ResetState reset the state
func ResetState(ctx context.Context, t *testing.T, serviceRef *service.Service) {
	t.Helper()

	if r, ok := serviceRef.Storage.(storage.Resetter); ok {
		err := r.Reset(ctx)
		require.NoError(t, err)
	}
}

// InsertProjectsIntoNewContext insert the projects into context.
func InsertProjectsIntoNewContext(projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(auth_context.NewContext(context.Background(),
		[]string{}, projects, "resource", "action", "pol"))
}

// DefaultMockPurgeFunc is the authz default mock purge function.
func DefaultMockPurgeFunc(context.Context,
	*authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	return &authz.PurgeSubjectFromPoliciesResp{}, nil
}

func defaultValidateProjectAssignmentFunc(context.Context,
	*authz_v2.ValidateProjectAssignmentReq) (*authz_v2.ValidateProjectAssignmentResp, error) {
	return &authz_v2.ValidateProjectAssignmentResp{}, nil
}
