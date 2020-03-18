// TODO (tc): These tests will be re-written or deleted by AUTOMATE-2950

package v1_test

// import (
// 	"context"
// 	"errors"
// 	"net/url"
// 	"os"
// 	"testing"

// 	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
// 	"github.com/stretchr/testify/assert"
// 	"github.com/stretchr/testify/require"
// 	"google.golang.org/grpc"
// 	"google.golang.org/grpc/codes"
// 	healthpb "google.golang.org/grpc/health/grpc_health_v1"
// 	"google.golang.org/grpc/reflection"

// 	"github.com/chef/automate/api/interservice/authz"
// 	"github.com/chef/automate/components/authz-service/engine"
// 	grpc_server "github.com/chef/automate/components/authz-service/server"
// 	server "github.com/chef/automate/components/authz-service/server/v1"
// 	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
// 	storage "github.com/chef/automate/components/authz-service/storage/v1"
// 	"github.com/chef/automate/components/authz-service/storage/v1/memstore"
// 	mockStore "github.com/chef/automate/components/authz-service/storage/v1/mockerr"
// 	"github.com/chef/automate/lib/grpc/grpctest"
// 	"github.com/chef/automate/lib/grpc/health"
// 	"github.com/chef/automate/lib/grpc/secureconn"
// 	"github.com/chef/automate/lib/logger"
// 	"github.com/chef/automate/lib/stringutils"
// 	"github.com/chef/automate/lib/tls/test/helpers"
// 	"github.com/chef/automate/lib/tracing"
// )

// var useDefaultEngine engine.V1Engine = nil

// const uuidLength = 36
// const defaultEffect = "allow"

// const resetDatabaseStatement = `DROP SCHEMA public CASCADE;
// CREATE SCHEMA public;
// GRANT ALL ON SCHEMA public TO postgres;
// GRANT ALL ON SCHEMA public TO public;`

// // ******************************** TEST SUITES ********************************

// func setup(t *testing.T) map[string]authz.AuthorizationClient {
// 	t.Helper()
// 	ctx, cancel := context.WithCancel(context.Background())
// 	defer cancel()

// 	l, err := logger.NewLogger("text", "warn")
// 	require.NoError(t, err, "init logger for storage")

// 	mem, err := memstore.New(l)
// 	require.NoError(t, err, "init memstore server")
// 	conn, _, _ := newGRPCServer(ctx, t, mem, useDefaultEngine, true)

// 	cls := map[string]authz.AuthorizationClient{
// 		"memstore": authz.NewAuthorizationClient(conn),
// 	}

// 	return cls
// }

// func TestIsAuthorized(t *testing.T) {
// 	ctx := context.Background()
// 	cls := setup(t)
// 	for desc, cl := range cls {
// 		t.Run(desc, func(t *testing.T) {
// 			t.Run("when the engine response is true, returns Authorized: true", func(t *testing.T) {
// 				resp, err := cl.IsAuthorized(ctx, &authz.IsAuthorizedReq{
// 					Subjects: []string{"user:local:admin"},
// 					Resource: "some:thing",
// 					Action:   "dothat",
// 				})
// 				require.NoError(t, err)
// 				assert.True(t, resp.Authorized)
// 			})

// 			t.Run("when the engine response is false, returns Authorized: false", func(t *testing.T) {
// 				resp, err := cl.IsAuthorized(ctx, &authz.IsAuthorizedReq{
// 					Subjects: []string{"user:local:not-admin"},
// 					Resource: "some:thing",
// 					Action:   "dothat",
// 				})
// 				require.NoError(t, err)
// 				assert.False(t, resp.Authorized)
// 			})
// 		})
// 	}
// }

// func TestFilterAuthorizedPairs(t *testing.T) {
// 	ctx := context.Background()
// 	cls := setup(t)
// 	for desc, cl := range cls {
// 		t.Run(desc, func(t *testing.T) {
// 			t.Run("if given an empty list to filter, it returns an empty list", func(t *testing.T) {
// 				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
// 					Subjects: []string{"user:local:admin"},
// 					Pairs:    []*authz.Pair{},
// 				})
// 				require.NoError(t, err)
// 				assert.Equal(t, 0, len(resp.Pairs))
// 			})

// 			t.Run("returns empty list if engine filters all pairs out", func(t *testing.T) {
// 				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
// 					Subjects: []string{"user:local:admin"},
// 					Pairs: []*authz.Pair{
// 						{
// 							Resource: "some:thing",
// 							Action:   "nopass",
// 						},
// 						{
// 							Resource: "some:thing:else",
// 							Action:   "nopass",
// 						},
// 					},
// 				})
// 				require.NoError(t, err)
// 				assert.Equal(t, 0, len(resp.Pairs))
// 			})

// 			t.Run("calls engine, and converts engine response to its response type", func(t *testing.T) {
// 				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
// 					Subjects: []string{"user:local:admin"},
// 					Pairs: []*authz.Pair{
// 						{
// 							Resource: "some:thing",
// 							Action:   "pass", // this one will be returned
// 						},
// 						{
// 							Resource: "some:thing",
// 							Action:   "nopass", // that one won't
// 						},
// 					},
// 				})
// 				require.NoError(t, err)
// 				require.Equal(t, 1, len(resp.Pairs))
// 				p := resp.Pairs[0]
// 				assert.Equal(t, "some:thing", p.Resource)
// 				assert.Equal(t, "pass", p.Action)
// 			})

// 			// This is a bit ridiculous, but who knows what could have gone wrong when
// 			// wrangling those input/output structs... Also, now the tests for this
// 			// method cover 0, 1, or 2 pairs.
// 			t.Run("returns more than one pair if the engine does that", func(t *testing.T) {
// 				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
// 					Subjects: []string{"user:local:admin"},
// 					Pairs: []*authz.Pair{
// 						{
// 							Resource: "some:thing",
// 							Action:   "pass", // this one will be returned
// 						},
// 						{
// 							Resource: "some:thing:else",
// 							Action:   "pass", // and that one, too
// 						},
// 					},
// 				})
// 				require.NoError(t, err)
// 				require.Equal(t, 2, len(resp.Pairs))
// 				assert.Contains(t, resp.Pairs, &authz.Pair{Resource: "some:thing", Action: "pass"})
// 				assert.Contains(t, resp.Pairs, &authz.Pair{Resource: "some:thing:else", Action: "pass"})
// 			})
// 		})
// 	}
// }

// func TestAuthzGRPCWithEngineError(t *testing.T) {
// 	ctx, cancel := context.WithCancel(context.Background())
// 	defer cancel()
// 	l, err := logger.NewLogger("text", "warn")
// 	require.NoError(t, err, "init logger for storage")
// 	mem, err := memstore.New(l)
// 	require.NoError(t, err, "init memstore server")
// 	conn, g, _ := newGRPCServer(ctx, t, mem, &testErrorEngine{}, true)
// 	defer g.Close()
// 	cl := authz.NewAuthorizationClient(conn)

// 	t.Run("IsAuthorized", func(t *testing.T) {
// 		t.Run("when the engine response is an error, returns an internal error code", func(t *testing.T) {
// 			_, err := cl.IsAuthorized(ctx, &authz.IsAuthorizedReq{
// 				Action:   "actionone",
// 				Subjects: []string{"user:local:subject1", "user:local:subject2"},
// 				Resource: "resource:1",
// 			})
// 			grpctest.AssertCode(t, codes.Internal, err)
// 		})
// 	})

// 	t.Run("FilterAuthorizedPairs", func(t *testing.T) {
// 		t.Run("when the engine response is an error, returns an internal error code", func(t *testing.T) {
// 			_, err := cl.FilterAuthorizedPairs(ctx,
// 				&authz.FilterAuthorizedPairsReq{
// 					Subjects: []string{"team:local:admins1"},
// 					Pairs:    []*authz.Pair{{Action: "read", Resource: "nodes:run_list"}},
// 				})
// 			grpctest.AssertCode(t, codes.Internal, err)
// 		})
// 	})

// 	// Note: PurgeSubjectFromPolicies has no engine interaction beyond
// 	// SetPolicies, and thus doesn't fit into this test suite.
// }

// // Note: the interesting tests around different policies, etc, happen in
// //   engine/conformance/conformance_test.go
// // The tests here are only concerned with the interplay between the various
// // functionalities (policy store, engine store, authorized API) exposed via
// // the GRPC API.

// // func TestAuthzGRPCInteractionWithEngine(t *testing.T) {
// // 	ctx, cancel := context.WithCancel(context.Background())
// // 	defer cancel()
// // 	l, err := logger.NewLogger("text", "warn")
// // 	if err != nil {
// // 		t.Fatalf("could not init logger: %s", err)
// // 	}

// // 	opa, err := opa.New(ctx, l)
// // 	if err != nil {
// // 		t.Fatalf("could not init opa engine: %s", err)
// // 	}
// // 	mem, err := memstore.New(l)
// // 	require.NoError(t, err, "init memstore server")
// // 	conn, g, _ := newGRPCServer(ctx, t, mem, opa, true)
// // 	defer g.Close()
// // 	cl := authz.NewAuthorizationClient(conn)

// // 	t.Run("IsAuthorized response changes when policies are changed", func(t *testing.T) {
// // 		reqQuery :=
// // 			&authz.IsAuthorizedReq{
// // 				Subjects: []string{"team:local:admins1"},
// // 				Action:   "read",
// // 				Resource: "auth:users",
// // 			}
// // 		// assert team is unauthorized prior to addition of policy
// // 		resp, err := cl.IsAuthorized(ctx, reqQuery)
// // 		require.NoError(t, err)
// // 		assert.False(t, resp.Authorized)

// // 		// arrange + act (store gets updated implicitly)
// // 		policyReq := &authz.CreatePolicyReq{
// // 			Action:   "read",
// // 			Subjects: []string{"team:local:admins1", "team:local:admins2"},
// // 			Resource: "auth:users",
// // 		}
// // 		testPolicies := []*authz.CreatePolicyReq{policyReq}
// // 		policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)

// // 		// assert team is now authorized
// // 		resp, err = cl.IsAuthorized(ctx, reqQuery)
// // 		require.NoError(t, err)
// // 		assert.True(t, resp.Authorized)
// // 		cleanupPolicies(ctx, t, cl, policyResponses)
// // 	})

// // 	t.Run("IsAuthorized response changes when a subject is purged from policies", func(t *testing.T) {
// // 		// create a policy including the subject we'll purge
// // 		policyReq := &authz.CreatePolicyReq{
// // 			Action:   "read",
// // 			Subjects: []string{"team:local:admins1", "team:local:admins2"},
// // 			Resource: "auth:users",
// // 		}
// // 		testPolicies := []*authz.CreatePolicyReq{policyReq}
// // 		policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)

// // 		// assert team is authorized prior to purging their subject
// // 		reqQuery := &authz.IsAuthorizedReq{
// // 			Subjects: []string{"team:local:admins1"},
// // 			Action:   "read",
// // 			Resource: "auth:users",
// // 		}
// // 		isAuthorizedResp, err := cl.IsAuthorized(ctx, reqQuery)
// // 		require.NoError(t, err)
// // 		assert.True(t, isAuthorizedResp.Authorized)

// // 		// arrange + act (store gets updated implicitly)
// // 		purgeReq := &authz.PurgeSubjectFromPoliciesReq{Subject: "team:local:admins1"}
// // 		resp, err := cl.PurgeSubjectFromPolicies(ctx, purgeReq)
// // 		require.NoError(t, err)
// // 		assert.Equal(t, policyResponses, resp.Ids)

// // 		// assert team is now unauthorized
// // 		isAuthorizedResp, err = cl.IsAuthorized(ctx, reqQuery)
// // 		require.NoError(t, err)
// // 		assert.False(t, isAuthorizedResp.Authorized)
// // 		cleanupPolicies(ctx, t, cl, policyResponses)
// // 	})

// // 	t.Run("FilterAuthorizedPairs response changes when policies are changed", func(t *testing.T) {

// // 		req := &authz.FilterAuthorizedPairsReq{
// // 			Subjects: []string{"team:local:admins1"},
// // 			Pairs:    []*authz.Pair{{Action: "read", Resource: "auth:users"}},
// // 		}
// // 		// assert team is unauthorized prior to addition of policy
// // 		resp, err := cl.FilterAuthorizedPairs(ctx, req)
// // 		require.NoError(t, err)
// // 		assert.Zero(t, len(resp.Pairs))

// // 		// arrange + act (store gets updated implicitly)
// // 		policyReq := &authz.CreatePolicyReq{
// // 			Action:   "read",
// // 			Subjects: []string{"team:local:admins1", "team:local:admins2"},
// // 			Resource: "auth:users",
// // 		}
// // 		testPolicies := []*authz.CreatePolicyReq{policyReq}
// // 		policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)

// // 		// assert team is now authorized
// // 		resp, err = cl.FilterAuthorizedPairs(ctx, req)
// // 		require.NoError(t, err)
// // 		assert.Equal(t, 1, len(resp.Pairs))
// // 		cleanupPolicies(ctx, t, cl, policyResponses)
// // 	})
// // }

// func TestHealthGRPC(t *testing.T) {
// 	ctx, cancel := context.WithCancel(context.Background())
// 	defer cancel()
// 	conn, g, _ := newGRPCServer(ctx, t, mockStore.New(), useDefaultEngine, false)
// 	defer g.Close()
// 	cl := healthpb.NewHealthClient(conn)

// 	t.Run("Check", func(t *testing.T) {
// 		// Note 2018/04/16 (sr): we don't supply a service name from the health
// 		// check hook (yet) -- this could be subsystems, e.g. database, engine,
// 		// ...
// 		t.Run("returns serving", func(t *testing.T) {
// 			actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
// 			require.NoError(t, err)
// 			assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
// 		})
// 	})
// }

// // ***************************** SUPPORT FUNCTIONS *****************************

// // default test engine: return authorized = true iff subjects contains "admin"
// type testEngine struct{}

// func (*testEngine) IsAuthorized(
// 	_ context.Context,
// 	subjects engine.Subjects,
// 	_ engine.Action,
// 	_ engine.Resource) (bool, error) {
// 	return stringutils.SliceContains(subjects, "user:local:admin"), nil
// }

// func (*testEngine) FilterAuthorizedPairs(
// 	_ context.Context,
// 	subjects engine.Subjects,
// 	pairs []engine.Pair) ([]engine.Pair, error) {
// 	filtered := []engine.Pair{}
// 	for _, p := range pairs {
// 		if p.Action == "pass" { // special action to control mock response in tests
// 			filtered = append(filtered, p)
// 		}
// 	}
// 	return filtered, nil
// }

// func (*testEngine) SetPolicies(context.Context, map[string]interface{}) error {
// 	return nil
// }

// type testErrorEngine struct{}

// func (*testErrorEngine) IsAuthorized(context.Context, engine.Subjects, engine.Action,
// 	engine.Resource) (bool, error) {
// 	return false, errors.New("engine failure")
// }

// func (*testErrorEngine) FilterAuthorizedPairs(context.Context, engine.Subjects,
// 	[]engine.Pair) ([]engine.Pair, error) {
// 	return nil, errors.New("engine failure")
// }

// func (*testErrorEngine) SetPolicies(context.Context, map[string]interface{}) error {
// 	return nil
// }

// type testRecordEngine struct {
// 	data map[string]interface{}
// }

// func (*testRecordEngine) IsAuthorized(context.Context, engine.Subjects, engine.Action,
// 	engine.Resource) (bool, error) {
// 	return false, errors.New("not implemented")
// }

// func (*testRecordEngine) FilterAuthorizedPairs(context.Context, engine.Subjects,
// 	[]engine.Pair) ([]engine.Pair, error) {
// 	return nil, errors.New("not implemented")
// }

// func (tre *testRecordEngine) SetPolicies(_ context.Context, data map[string]interface{}) error {
// 	tre.data = data
// 	return nil
// }

// func newGRPCServer(ctx context.Context,
// 	t *testing.T, store storage.Storage, engine engine.V1Engine, initPolicies bool) (
// 	*grpc.ClientConn, *grpctest.Server, *server.Server) {
// 	var serv *server.Server
// 	if store == nil {
// 		t.Fatal("must pass valid store to helper newGRPCServerAndClient")
// 	}

// 	l, err := logger.NewLogger("text", "warn")
// 	if err != nil {
// 		t.Fatal("could not init logger")
// 	}

// 	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
// 	connFactory := secureconn.NewFactory(*serviceCerts)

// 	if engine == nil {
// 		engine = &testEngine{}
// 	}

// 	serv, err = server.NewWithStore(ctx, l, engine, store, initPolicies, true, nil)
// 	if err != nil {
// 		t.Fatalf("could not init server: %s", err.Error())
// 	}

// 	g := connFactory.NewServer(grpc.UnaryInterceptor(
// 		grpc_middleware.ChainUnaryServer(
// 			tracing.ServerInterceptor(tracing.GlobalTracer()),
// 			grpc_server.InputValidationInterceptor(),
// 		),
// 	),
// 	)

// 	health.RegisterHealthServer(g, health.NewService())
// 	authz.RegisterAuthorizationServer(g, serv)

// 	reflection.Register(g)

// 	testServ := grpctest.NewServer(g)

// 	conn, err := connFactory.Dial("authz-service", testServ.URL)
// 	if err != nil {
// 		t.Fatalf("connecting to grpc endpoint: %s", err)
// 	}
// 	return conn, testServ, serv
// }

// // TODO: refactor (copied for now, from postgres_test)
// func migrationConfigIfPGTestsToBeRun(l logger.Logger, migrationPath string) (*migration.Config, error) {
// 	customPGURL, pgURLPassed := os.LookupEnv("PG_URL")
// 	ciMode := os.Getenv("CI") == "true"

// 	// If in CI mode, use the default
// 	if ciMode {
// 		pgURL, err := url.Parse("postgres://postgres@127.0.0.1:5432/authz_test?sslmode=disable")
// 		if err != nil {
// 			return nil, err
// 		}
// 		return &migration.Config{
// 			Path:   migrationPath,
// 			Logger: l,
// 			PGURL:  pgURL,
// 		}, nil
// 	}

// 	// If PG_URL wasn't passed (and we aren't in CI)
// 	// we shouldn't run the postgres tests, return nil.
// 	if !pgURLPassed {
// 		return nil, nil
// 	}

// 	pgURL, err := url.Parse(customPGURL)
// 	if err != nil {
// 		return nil, err
// 	}

// 	return &migration.Config{
// 		Path:   migrationPath,
// 		Logger: l,
// 		PGURL:  pgURL,
// 	}, nil
// }
