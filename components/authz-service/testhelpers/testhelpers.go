package testhelpers

import (
	"context"
	"database/sql"
	"net/url"
	"os"
	"path"
	"runtime"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/engine/opa"
	"github.com/chef/automate/components/authz-service/prng"
	grpc_server "github.com/chef/automate/components/authz-service/server"
	server "github.com/chef/automate/components/authz-service/server/v2"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	postgres_v1 "github.com/chef/automate/components/authz-service/storage/v1/postgres"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/components/authz-service/storage/v2/postgres"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/test/helpers"
)

type TestFramework struct {
	Policy               api.PoliciesClient
	Authz                api.AuthorizationClient
	Projects             api.ProjectsClient
	TestDB               *TestDB
	Engine               engine.Engine
	Seed                 int64
	PolicyRefresher      server.PolicyRefresher
	GRPC                 *grpctest.Server
	ProjectUpdateManager server.ProjectUpdateMgr
}

type TestDB struct {
	*sql.DB
}

const resetDatabaseStatement = `DROP SCHEMA public CASCADE;
CREATE SCHEMA public;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;`

// NewTestFramework spins up authz-service with nothing mocked besides the event service.
// Due to lots of state (OPA cache that we aren't cleaning up properly, goroutines, etc)
// it should be used for a single test and Shutdown should be called on it at the end of
// that test. You should also be passing a cancel-able context.
func NewTestFramework(t *testing.T, ctx context.Context) *TestFramework {
	t.Helper()
	seed := prng.GenSeed(t)

	pg, testDB, opaInstance, _, migrationConfig := SetupTestDB(t)

	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for storage")

	pgV1, err := postgres_v1.New(ctx, l, *migrationConfig)
	require.NoError(t, err)

	vChan := make(chan api.Version, 1)
	vSwitch := server.NewSwitch(vChan)

	polRefresher, err := v2.NewPostgresPolicyRefresher(ctx, l, opaInstance)
	require.NoError(t, err)

	polSrv, err := server.NewPoliciesServer(ctx, l, polRefresher, pg, opaInstance, pgV1, vSwitch, vChan)
	require.NoError(t, err)

	projectUpdateManager := NewMockProjectUpdateManager()
	projectsSrv, err := server.NewProjectsServer(
		ctx, l, pg, opaInstance, projectUpdateManager, polRefresher)
	require.NoError(t, err)

	authzSrv, err := server.NewPostgresAuthzServer(l, opaInstance, vSwitch, projectsSrv)
	require.NoError(t, err)

	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	// TODO(sr): refactor our constructors. Having to maintain the middleware in
	// three places is tedious and error-prone.
	serv := connFactory.NewServer(grpc.UnaryInterceptor(
		grpc_server.InputValidationInterceptor(),
	))
	api.RegisterProjectsServer(serv, projectsSrv)
	api.RegisterAuthorizationServer(serv, authzSrv)
	api.RegisterPoliciesServer(serv, polSrv)

	grpcServ := grpctest.NewServer(serv)

	conn, err := connFactory.Dial("authz-service", grpcServ.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	tf := &TestFramework{
		Policy:               api.NewPoliciesClient(conn),
		Authz:                api.NewAuthorizationClient(conn),
		Projects:             api.NewProjectsClient(conn),
		TestDB:               testDB,
		Engine:               opaInstance,
		Seed:                 seed,
		PolicyRefresher:      polRefresher,
		ProjectUpdateManager: projectUpdateManager,
		GRPC:                 grpcServ,
	}

	return tf
}

// Shutdown must be called at the end of every test that uses TestFramework
// otherwise strange things might happen with the authz-service/config.Manager
// because other tests might accidentally get state to its config file
// if we don't kill the goroutine that writes to it.
func (tf *TestFramework) Shutdown(t *testing.T, ctx context.Context) {
	t.Helper()
	tf.TestDB.Flush(t)

	tf.GRPC.Close()
	// TODO (tc): Track down and kill literally every goroutine we start, otherwise
	// this TestFramework's authz instance could write some bad state while the next test is running.
}

// SetupProjectsAndRulesWithDB is a simplified test framework
// useful for integration tests with just the DB.
func SetupProjectsAndRulesWithDB(t *testing.T) (
	api.ProjectsClient, *TestDB, storage.Storage, int64) {
	t.Helper()
	ctx := context.Background()
	seed := prng.GenSeed(t)

	pg, testDB, _, _, _ := SetupTestDB(t)

	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for storage")
	projectUpdateManager := NewMockProjectUpdateManager()
	projectsSrv, err := server.NewProjectsServer(
		ctx, l, pg, &TestProjectRulesRetriever{}, projectUpdateManager, NewMockPolicyRefresher())
	require.NoError(t, err)

	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	// TODO(sr): refactor our constructors. Having to maintain the middleware in
	// three places is tedious and error-prone.
	serv := connFactory.NewServer(grpc.UnaryInterceptor(
		grpc_server.InputValidationInterceptor(),
	))
	api.RegisterProjectsServer(serv, projectsSrv)

	grpcServ := grpctest.NewServer(serv)

	conn, err := connFactory.Dial("authz-service", grpcServ.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	return api.NewProjectsClient(conn), testDB, pg, seed
}

func SetupTestDB(t *testing.T) (storage.Storage, *TestDB, *opa.State, *prng.Prng, *migration.Config) {
	t.Helper()

	ctx := context.Background()
	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for postgres storage")

	opaInstance, err := opa.New(ctx, l)
	require.NoError(t, err, "init OPA")

	migrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../storage/postgres/migration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	dataMigrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../storage/postgres/datamigration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	if migrationConfig == nil && dataMigrationConfig == nil {
		t.Skipf("start pg container and set PG_URL to run")
	}

	// reset database the hard way -- we do this to ensure that our comparison
	// between database content and hardcoded storage default policies actually
	// compares the migrated policies with the hardcoded ones (and NOT the
	// hardcoded policies with the hardcoded policies).
	db := openDB(t)
	_, err = db.ExecContext(ctx, resetDatabaseStatement)
	require.NoError(t, err, "error resetting database")
	_, err = db.Exec(`CREATE EXTENSION IF NOT EXISTS "uuid-ossp"`)
	require.NoError(t, err, "error creating extension")

	err = postgres.Initialize(ctx, opaInstance, l, *migrationConfig, datamigration.Config(*dataMigrationConfig))
	require.NoError(t, err)
	return postgres.GetInstance(), &TestDB{DB: db}, opaInstance, prng.Seed(t), migrationConfig
}

func (d *TestDB) Flush(t *testing.T) {
	_, err := d.Exec(`DELETE FROM iam_policies CASCADE;
		DELETE FROM iam_members CASCADE;
		DELETE FROM iam_roles CASCADE;
		DELETE FROM iam_projects CASCADE;`)
	require.NoError(t, err)
}

func (d *TestDB) CloseDB(t *testing.T) {
	t.Helper()
	require.NoError(t, d.Close())
}

// migrationConfigIfPGTestsToBeRun either returns the pg migration config
// if PG_URL is set or we are in CI, otherwise it returns nil, indicating
// postgres based tests shouldn't be run.
func migrationConfigIfPGTestsToBeRun(l logger.Logger, migrationFolder string) (*migration.Config, error) {
	customPGURL, pgURLPassed := os.LookupEnv("PG_URL")
	ciMode := os.Getenv("CI") == "true"

	_, filepath, _, _ := runtime.Caller(1)
	migrationPath := path.Join(path.Dir(filepath), migrationFolder)

	// If in CI mode, use the default
	if ciMode {
		pgURL, err := url.Parse("postgres://postgres@127.0.0.1:5432/authz_test?sslmode=disable")
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
		return nil, nil
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

func openDB(t *testing.T) *sql.DB {
	t.Helper()
	db, err := sql.Open("postgres", "postgres://postgres:postgres@127.0.0.1:5432/authz_test?sslmode=disable")
	require.NoError(t, err, "error opening db")
	err = db.Ping()
	require.NoError(t, err, "error pinging db")

	return db
}

type MockEventServiceClient struct {
	PublishedEvents       int
	LastestPublishedEvent *automate_event.EventMsg
}

func (t *MockEventServiceClient) Publish(ctx context.Context,
	in *automate_event.PublishRequest,
	opts ...grpc.CallOption) (*automate_event.PublishResponse, error) {
	t.PublishedEvents++
	t.LastestPublishedEvent = in.Msg
	return &automate_event.PublishResponse{}, nil
}

func (t *MockEventServiceClient) Subscribe(ctx context.Context,
	in *automate_event.SubscribeRequest,
	opts ...grpc.CallOption) (*automate_event.SubscribeResponse, error) {
	return &automate_event.SubscribeResponse{}, nil
}

func (t *MockEventServiceClient) Start(ctx context.Context,
	in *automate_event.StartRequest,
	opts ...grpc.CallOption) (*automate_event.StartResponse, error) {
	return &automate_event.StartResponse{}, nil
}

func (t *MockEventServiceClient) Stop(ctx context.Context,
	in *automate_event.StopRequest,
	opts ...grpc.CallOption) (*automate_event.StopResponse, error) {
	return &automate_event.StopResponse{}, nil
}

// TODO More testing
type TestProjectRulesRetriever struct{}

func (t *TestProjectRulesRetriever) ListProjectMappings(
	context.Context) (map[string][]storage.Rule, error) {
	return make(map[string][]storage.Rule, 0), nil
}

type mockPolicyRefresher struct{}

func NewMockPolicyRefresher() server.PolicyRefresher {
	return &mockPolicyRefresher{}
}

func (*mockPolicyRefresher) Refresh(context.Context) error {
	return nil
}

func (refresher *mockPolicyRefresher) RefreshAsync() error {
	return nil
}

func WaitForWithTimeout(t *testing.T, f func() bool, timeout time.Duration, message string) {
	expired := time.Now().Add(timeout)
	for {
		if f() {
			break
		}

		if expired.Before(time.Now()) {
			assert.Fail(t, message)
			break
		}
		time.Sleep(time.Millisecond * 10)
	}
}

type mockProjectUpdateManager struct{}

func NewMockProjectUpdateManager() *mockProjectUpdateManager {
	return &mockProjectUpdateManager{}
}

func (*mockProjectUpdateManager) Cancel() error {
	return nil
}

func (*mockProjectUpdateManager) Start() error {
	return nil
}

func (*mockProjectUpdateManager) Status() (server.ProjectUpdateStatus, error) {
	return &server.EmptyProjectUpdateStatus{}, nil
}
