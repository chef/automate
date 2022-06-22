package test

import (
	"context"
	"errors"
	"net/http"
	"net/url"
	"os"
	"testing"

	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authz"
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

type MockStatusChecker struct{}

func (s MockStatusChecker) GetInfraServerStatus(_ string) (*http.Response, error) {
	return &http.Response{
		StatusCode: 200,
	}, nil
}

type MockStatusFailedChecker struct{}

func (s MockStatusFailedChecker) GetInfraServerStatus(_ string) (*http.Response, error) {
	return nil, errors.New("Not able to connect to the server")
}

func SetMockStatusChecker(mockInfraServer *server.Server, checker server.StatusChecker) {
	mockInfraServer.SetAuthenticator(checker)
}

// migrationConfigIfPGTestsToBeRun either returns the pg migration config
// if PG_URL is set or we are in CI system, otherwise it returns nil, indicating
// postgres based tests shouldn't be run.
func migrationConfigIfPGTestsToBeRun(l logger.Logger, migrationPath string) (*migration.Config, error) {
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
	t *testing.T) (*server.Server, *service.Service, *grpc.ClientConn, func(), *authz.PoliciesServiceServerMock, *secrets.MockSecretsServiceClient) {

	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, "infra-proxy-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()

	mockAuthz := authz.NewAuthorizationServiceServerMock()
	mockAuthz.ValidateProjectAssignmentFunc = defaultValidateProjectAssignmentFunc
	authz.RegisterAuthorizationServiceServer(grpcAuthz, mockAuthz)

	mockPolicies := authz.NewPoliciesServiceServerMock()
	mockPolicies.PurgeSubjectFromPoliciesFunc = DefaultMockPurgeFunc
	authz.RegisterPoliciesServiceServer(grpcAuthz, mockPolicies)

	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)

	authzClient := authz.NewAuthorizationServiceClient(authzConn)

	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../storage/postgres/migration/sql")
	require.NoError(t, err)

	serviceRef, err := service.Start(l, *migrationConfig, connFactory, secretsClient, authzClient)

	if err != nil {
		t.Fatalf("could not create server: %s", err)
	}
	grpcServ := serviceRef.ConnFactory.NewServer(tracing.GlobalServerInterceptor())
	newServer := server.NewServer(serviceRef)
	newServer.SetAuthenticator(MockStatusChecker{})
	infra_proxy.RegisterInfraProxyServiceServer(grpcServ, newServer)
	health.RegisterHealthServer(grpcServ, health.NewService())

	ResetState(ctx, t, serviceRef)

	secretsMock := serviceRef.Secrets.(*secrets.MockSecretsServiceClient)

	g := grpctest.NewServer(grpcServ)

	conn, err := connFactory.Dial("infra-proxy-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	return newServer, serviceRef, conn, func() { g.Close(); authzServer.Close() }, mockPolicies, secretsMock
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
		[]string{}, projects, "resource", "action"))
}

// DefaultMockPurgeFunc is the authz default mock purge function.
func DefaultMockPurgeFunc(context.Context,
	*authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	return &authz.PurgeSubjectFromPoliciesResp{}, nil
}

func defaultValidateProjectAssignmentFunc(context.Context,
	*authz.ValidateProjectAssignmentReq) (*authz.ValidateProjectAssignmentResp, error) {
	return &authz.ValidateProjectAssignmentResp{}, nil
}
