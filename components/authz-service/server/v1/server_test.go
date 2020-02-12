package v1_test

import (
	"context"
	"errors"
	"net/url"
	"os"
	"reflect"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"
	"google.golang.org/grpc/reflection"

	version "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/api/interservice/authz"
	constants "github.com/chef/automate/components/authz-service/constants/v1"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/engine/opa"
	grpc_server "github.com/chef/automate/components/authz-service/server"
	server "github.com/chef/automate/components/authz-service/server/v1"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage "github.com/chef/automate/components/authz-service/storage/v1"
	"github.com/chef/automate/components/authz-service/storage/v1/memstore"
	mockStore "github.com/chef/automate/components/authz-service/storage/v1/mockerr"
	"github.com/chef/automate/components/authz-service/storage/v1/postgres"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/tls/test/helpers"
	"github.com/chef/automate/lib/tracing"
)

var useDefaultEngine engine.V1Engine = nil

const uuidLength = 36
const defaultEffect = "allow"

// Some common, arbitrary policy definitions.
var req1 = &authz.CreatePolicyReq{
	Action:   "actionone",
	Subjects: []string{"user:local:subject1", "user:local:subject2"},
	Resource: "resource:1",
}
var req2 = &authz.CreatePolicyReq{
	Action:   "actiontwo",
	Subjects: []string{"team:local:team1"},
	Resource: "resource:two",
}
var req3 = &authz.CreatePolicyReq{
	Action:   "actionthree",
	Subjects: []string{"user:local:subject1", "user:local:subject2", "user:local:subject3", "user:local:subject4"},
	Resource: "resource:-3",
}

// ******************************** TEST SUITES ********************************

func setup(t *testing.T) map[string]authz.AuthorizationClient {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	l, err := logger.NewLogger("text", "warn")
	require.NoError(t, err, "init logger for storage")

	mem, err := memstore.New(l)
	require.NoError(t, err, "init memstore server")
	conn, _, _ := newGRPCServer(ctx, t, mem, useDefaultEngine, true)

	cls := map[string]authz.AuthorizationClient{
		"memstore": authz.NewAuthorizationClient(conn),
	}

	// optional PG setup
	migrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../../storage/postgres/migration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	datamigrationConfig := datamigration.Config{}
	if migrationConfig != nil { // So, we want PG...
		backend, err := postgres.New(ctx, l, *migrationConfig, datamigrationConfig)
		require.NoError(t, err)
		// reset
		require.NoError(t, backend.(storage.Resetter).Reset(ctx))

		conn, _, _ = newGRPCServer(ctx, t, backend, useDefaultEngine, true)
		cls["postgres"] = authz.NewAuthorizationClient(conn)
	}

	return cls
}

func TestGetVersion(t *testing.T) {
	ctx := context.Background()
	cls := setup(t)
	for desc, cl := range cls {
		t.Run(desc, func(t *testing.T) {
			t.Run("returns expected version", func(t *testing.T) {
				expectedVersion := "unknown" // Version is injected via linker flags that we don't set during tests
				resp, err := cl.GetVersion(ctx, &version.VersionInfoRequest{})
				require.NoError(t, err)
				assert.Equal(t, expectedVersion, resp.Version)
			})
		})
	}
}

func TestIsAuthorized(t *testing.T) {
	ctx := context.Background()
	cls := setup(t)
	for desc, cl := range cls {
		t.Run(desc, func(t *testing.T) {
			t.Run("when the engine response is true, returns Authorized: true", func(t *testing.T) {
				resp, err := cl.IsAuthorized(ctx, &authz.IsAuthorizedReq{
					Subjects: []string{"user:local:admin"},
					Resource: "some:thing",
					Action:   "dothat",
				})
				require.NoError(t, err)
				assert.True(t, resp.Authorized)
			})

			t.Run("when the engine response is false, returns Authorized: false", func(t *testing.T) {
				resp, err := cl.IsAuthorized(ctx, &authz.IsAuthorizedReq{
					Subjects: []string{"user:local:not-admin"},
					Resource: "some:thing",
					Action:   "dothat",
				})
				require.NoError(t, err)
				assert.False(t, resp.Authorized)
			})
		})
	}
}

func TestFilterAuthorizedPairs(t *testing.T) {
	ctx := context.Background()
	cls := setup(t)
	for desc, cl := range cls {
		t.Run(desc, func(t *testing.T) {
			t.Run("if given an empty list to filter, it returns an empty list", func(t *testing.T) {
				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
					Subjects: []string{"user:local:admin"},
					Pairs:    []*authz.Pair{},
				})
				require.NoError(t, err)
				assert.Equal(t, 0, len(resp.Pairs))
			})

			t.Run("returns empty list if engine filters all pairs out", func(t *testing.T) {
				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
					Subjects: []string{"user:local:admin"},
					Pairs: []*authz.Pair{
						{
							Resource: "some:thing",
							Action:   "nopass",
						},
						{
							Resource: "some:thing:else",
							Action:   "nopass",
						},
					},
				})
				require.NoError(t, err)
				assert.Equal(t, 0, len(resp.Pairs))
			})

			t.Run("calls engine, and converts engine response to its response type", func(t *testing.T) {
				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
					Subjects: []string{"user:local:admin"},
					Pairs: []*authz.Pair{
						{
							Resource: "some:thing",
							Action:   "pass", // this one will be returned
						},
						{
							Resource: "some:thing",
							Action:   "nopass", // that one won't
						},
					},
				})
				require.NoError(t, err)
				require.Equal(t, 1, len(resp.Pairs))
				p := resp.Pairs[0]
				assert.Equal(t, "some:thing", p.Resource)
				assert.Equal(t, "pass", p.Action)
			})

			// This is a bit ridiculous, but who knows what could have gone wrong when
			// wrangling those input/output structs... Also, now the tests for this
			// method cover 0, 1, or 2 pairs.
			t.Run("returns more than one pair if the engine does that", func(t *testing.T) {
				resp, err := cl.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
					Subjects: []string{"user:local:admin"},
					Pairs: []*authz.Pair{
						{
							Resource: "some:thing",
							Action:   "pass", // this one will be returned
						},
						{
							Resource: "some:thing:else",
							Action:   "pass", // and that one, too
						},
					},
				})
				require.NoError(t, err)
				require.Equal(t, 2, len(resp.Pairs))
				assert.Contains(t, resp.Pairs, &authz.Pair{Resource: "some:thing", Action: "pass"})
				assert.Contains(t, resp.Pairs, &authz.Pair{Resource: "some:thing:else", Action: "pass"})
			})
		})
	}
}

func TestCreatePolicy(t *testing.T) {
	cls := setup(t)
	for desc, cl := range cls {
		t.Run(desc, func(t *testing.T) {
			t.Run("successfully creates policy", func(t *testing.T) {
				subjTests := map[string][]string{
					"single subject":                  {"user:local:subject1"},
					"multiple subjects":               {"user:local:subject1", "user:local:subject2", "user:local:subject3"},
					"subject with long name":          {"user:local:long-long-long-long-long-long-long-long-long-name-here"},
					"one-character long subject name": {"user:local:a"},
				}
				for desc, tc := range subjTests {
					t.Run("with valid action and "+desc, expectSuccess(cl, authz.CreatePolicyReq{
						Action:   "action",
						Subjects: tc,
						Resource: "resource:1",
					}))
				}

				actTests := map[string]string{
					"single action":            "one",
					"action with long name":    "longlonglonglonglonglonglonglonglongactionhere",
					"shortest possible action": "a",
					"wildcard action":          "*",
				}
				for desc, tc := range actTests {
					t.Run("with valid subject and "+desc, expectSuccess(cl, authz.CreatePolicyReq{
						Action:   tc,
						Subjects: []string{"user:local:subject1", "user:local:subject2"},
						Resource: "resource:1",
					}))
				}

				resTests := map[string]string{
					"short resource":                   "one",
					"resource with long name":          "longlonglonglonglonglonglonglonglongresourcehere",
					"shortest possible resource":       "a",
					"wildcard resource":                "*",
					"wildcard resource with namespace": "foo:*",
					"resource with variable":           "foo:bar:${a2:username}",
					"wildcard resource with variable":  "foo:${a2:username}:*",
				}
				for desc, tc := range resTests {
					t.Run("with valid subject and "+desc, expectSuccess(cl, authz.CreatePolicyReq{
						Action:   "dothings",
						Subjects: []string{"user:local:subject1", "user:local:subject2"},
						Resource: tc,
					}))
				}
			})

			t.Run("returns InvalidArgument", func(t *testing.T) {
				tests := map[string]authz.CreatePolicyReq{
					"no subject submitted": {
						Action:   "action",
						Subjects: []string{},
						Resource: "resource:1",
					},
					"no action submitted": {
						Action:   "",
						Subjects: []string{"user:local:subject1", "user:local:subject2", "user:local:subject3"},
						Resource: "resource:1",
					},
					"no resource submitted": {
						Action:   "someaction",
						Subjects: []string{"user:local:subject1", "user:local:subject2", "user:local:subject3"},
						Resource: "",
					},
				}
				for desc, tc := range tests {
					t.Run(desc, expectFailureWithCode(cl, codes.InvalidArgument, tc))
				}
			})
		})
	}
}

func TestListPolicies(t *testing.T) {
	ctx := context.Background()
	cls := setup(t)
	for desc, cl := range cls {
		t.Run(desc, func(t *testing.T) {
			t.Run("returns a list of policies", func(t *testing.T) {
				tests := []struct {
					policies []*authz.CreatePolicyReq
					desc     string
				}{
					{[]*authz.CreatePolicyReq{req1, req2, req3}, "multiple policies"},
					{[]*authz.CreatePolicyReq{req3}, "a single policy"},
					{[]*authz.CreatePolicyReq{}, "no policies"},
				}
				for _, test := range tests {
					t.Run("for "+test.desc, func(t *testing.T) {
						assertPolicyStoreLength(ctx, t, cl, 0)
						policyResponses := generateTestPolicies(ctx, t, cl, test.policies)

						list, err := cl.ListPolicies(ctx, &authz.ListPoliciesReq{})

						require.NoError(t, err)
						require.NotNil(t, list)
						assert.Equal(t, len(test.policies)+len(constants.DefaultPolicyIDs), len(list.Policies))
						for _, policy := range test.policies {
							assert.True(t, listContainsPolicy(list.Policies, policy))
						}

						cleanupPolicies(ctx, t, cl, policyResponses)
					})
				}
			})
		})
	}
}

func TestDeletePolicy(t *testing.T) {
	ctx := context.Background()
	cls := setup(t)
	for desc, cl := range cls {
		t.Run(desc, func(t *testing.T) {
			t.Run("successfully deletes user created policies", func(t *testing.T) {
				tests := []struct {
					policies []*authz.CreatePolicyReq
					desc     string
				}{
					{[]*authz.CreatePolicyReq{req1, req2, req3}, "multiple policies"},
					{[]*authz.CreatePolicyReq{req3}, "a single policy"},
				}
				for _, test := range tests {
					t.Run("when store contains "+test.desc, func(t *testing.T) {
						assertPolicyStoreLength(ctx, t, cl, 0)
						policyResponses := generateTestPolicies(ctx, t, cl, test.policies)
						assertPolicyStoreLength(ctx, t, cl, len(test.policies))
						req := &authz.DeletePolicyReq{
							// TODO: nicer to pick a random one rather than always pick the first one
							Id: policyResponses[0],
						}

						resp, err := cl.DeletePolicy(ctx, req)

						assertPolicyStoreLength(ctx, t, cl, len(test.policies)-1)
						require.NoError(t, err)
						require.NotNil(t, resp)
						assert.Equal(t, resp.Policy.Action, test.policies[0].Action)
						assert.Equal(t, resp.Policy.Subjects, test.policies[0].Subjects)
						assert.Equal(t, resp.Policy.Resource, test.policies[0].Resource)

						cleanupPoliciesExceptOne(ctx, t, cl, policyResponses, req.Id)
					})
				}
			})

			t.Run("successfully deletes default policies", func(t *testing.T) {
				for _, id := range constants.DeletablePolicyIDs {

					assertPolicyStoreLength(ctx, t, cl, 0)

					resp, err := cl.DeletePolicy(ctx, &authz.DeletePolicyReq{Id: id})

					require.NoError(t, err)
					require.NotNil(t, resp)
					assertPolicyStoreLength(ctx, t, cl, -1)

					// Cleanup
					// TODO (tc) Not gonna have the correct UUID after test executes,
					// but is that ok for now?
					policies, err := storage.DefaultPolicies()
					require.NoError(t, err)
					policy := policies[id]
					_, err = cl.CreatePolicy(ctx, &authz.CreatePolicyReq{
						Action:   policy.Action,
						Subjects: policy.Subjects,
						Resource: policy.Resource,
					})
					require.NoError(t, err)
				}
			})

			t.Run("fails to delete non-deletable policies", func(t *testing.T) {
				for _, id := range constants.NonDeletablePolicyIDs {
					assertPolicyStoreLength(ctx, t, cl, 0)

					resp, err := cl.DeletePolicy(ctx, &authz.DeletePolicyReq{Id: id})

					require.Nil(t, resp)
					grpctest.AssertCode(t, codes.InvalidArgument, err)
					assertPolicyStoreLength(ctx, t, cl, 0)
				}
			})

			t.Run("fails to delete the policy when the ID does not exist", func(t *testing.T) {
				tests := []struct {
					policies []*authz.CreatePolicyReq
					desc     string
				}{
					{[]*authz.CreatePolicyReq{req1, req2, req3}, "multiple policies"},
					{[]*authz.CreatePolicyReq{req3}, "a single policy"},
					{[]*authz.CreatePolicyReq{}, "no policies"},
				}
				for _, test := range tests {
					t.Run("when store contains "+test.desc, func(t *testing.T) {
						assertPolicyStoreLength(ctx, t, cl, 0)
						testPolicies := []*authz.CreatePolicyReq{req1, req2, req3}
						policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)
						assertPolicyStoreLength(ctx, t, cl, len(testPolicies))
						req := &authz.DeletePolicyReq{
							// Not a GUID that exists
							Id: "97e01ea1-976e-4626-88c8-43345c5d934f",
						}

						resp, err := cl.DeletePolicy(ctx, req)

						require.Nil(t, resp)
						grpctest.AssertCode(t, codes.NotFound, err)
						cleanupPolicies(ctx, t, cl, policyResponses)
					})
				}
			})

			t.Run("returns InvalidArgument", func(t *testing.T) {
				tests := map[string]authz.DeletePolicyReq{
					"no ID submitted":           {},
					"submitted ID is no UUIDv4": {Id: "35bffbab-3a49-dd8a-94a1-9ea87ec5c3cc"},
				}
				for desc, tc := range tests {
					t.Run(desc, func(t *testing.T) {
						resp, err := cl.DeletePolicy(ctx, &tc)
						require.Nil(t, resp)
						grpctest.AssertCode(t, codes.InvalidArgument, err)
					})
				}
			})
		})
	}
}

func TestPurgeSubjectFromPolicies(t *testing.T) {
	ctx := context.Background()
	cls := setup(t)
	for desc, cl := range cls {
		t.Run(desc, func(t *testing.T) {
			// set the stage: create some background noise policies
			assertPolicyStoreLength(ctx, t, cl, 0)
			testPolicies := []*authz.CreatePolicyReq{req1, req2, req3}
			policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)
			defer cleanupPolicies(ctx, t, cl, policyResponses)
			assertPolicyStoreLength(ctx, t, cl, len(testPolicies))

			subjTests := map[string][][]string{
				"single subject":                      {{"user:local:purge"}},
				"multiple subjects (first)":           {{"user:local:purge", "user:local:subject2", "user:local:subject3"}},
				"multiple subjects (last)":            {{"user:local:subject0", "user:local:purge"}},
				"single subjects (multiple)":          {{"user:local:purge"}, {"user:local:purge"}},
				"multiple subjects (first, multiple)": {{"user:local:purge"}, {"user:local:purge", "user:local:subject0"}},
				"multiple subjects (last, multiple)":  {{"user:local:purge"}, {"user:local:subject0", "user:local:purge"}},
			}
			for desc, tc := range subjTests {
				t.Run("with existing matching policy having "+desc, func(t *testing.T) {
					// save list response to compare non-affected policies below
					listRespPre, err := cl.ListPolicies(ctx, &authz.ListPoliciesReq{})
					require.NoError(t, err)

					// arrange
					var ids []string
					for _, subs := range tc {
						createResp, err := cl.CreatePolicy(ctx, &authz.CreatePolicyReq{
							Subjects: subs,
							Action:   "read",
							Resource: "nodes:info",
						})
						require.NoError(t, err)
						ids = append(ids, createResp.Policy.Id)
					}

					// act
					resp, err := cl.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{Subject: "user:local:purge"})
					require.NoError(t, err)

					// assert
					assert.ElementsMatch(t, ids, resp.Ids)

					listResp, err := cl.ListPolicies(ctx, &authz.ListPoliciesReq{})
					require.NoError(t, err)
					for _, pol := range listResp.Policies {
						assert.NotContains(t, pol.Subjects, "user:local:purge")

						for i := range ids {
							if pol.Id == ids[i] {
								ts, err := ptypes.Timestamp(pol.GetUpdatedAt())
								require.NoError(t, err)
								assert.WithinDuration(t, time.Now(), ts, time.Second)
							}
						}
					}

					// Note: we don't allow creating unattached policies (yet), so to test
					// that subject purging works OK with policies that are already
					// unattached, we simply run it again. The "single subject" policy
					// will be unattached when we reach this test.
					resp, err = cl.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{Subject: "user:local:purge"})
					require.NoError(t, err)
					assert.Nil(t, resp.Ids)

					// compare pre-purge list response with post-purge list response
					// first, we drop the policies we've created for purging their subject
					cleanupPolicies(ctx, t, cl, ids)
					// listing again, to not include the one we've just deleted
					listResp, err = cl.ListPolicies(ctx, &authz.ListPoliciesReq{})
					require.NoError(t, err)
					assert.ElementsMatch(t, listRespPre.Policies, listResp.Policies)
				})
			}

			t.Run("non-deletable policies are skipped", func(t *testing.T) {
				// Note: We cannot create a non-deletable policy from the API, so this
				// test is using one of our non-deletable default policies.
				resp, err := cl.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{Subject: "team:local:admins"})
				require.NoError(t, err)
				assert.Nil(t, resp.Ids)
			})
		})
	}
}

func TestAuthzGRPCWithEngineError(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	l, err := logger.NewLogger("text", "warn")
	require.NoError(t, err, "init logger for storage")
	mem, err := memstore.New(l)
	require.NoError(t, err, "init memstore server")
	conn, g, _ := newGRPCServer(ctx, t, mem, &testErrorEngine{}, true)
	defer g.Close()
	cl := authz.NewAuthorizationClient(conn)

	t.Run("IsAuthorized", func(t *testing.T) {
		t.Run("when the engine response is an error, returns an internal error code", func(t *testing.T) {
			_, err := cl.IsAuthorized(ctx, &authz.IsAuthorizedReq{
				Action:   "actionone",
				Subjects: []string{"user:local:subject1", "user:local:subject2"},
				Resource: "resource:1",
			})
			grpctest.AssertCode(t, codes.Internal, err)
		})
	})

	t.Run("FilterAuthorizedPairs", func(t *testing.T) {
		t.Run("when the engine response is an error, returns an internal error code", func(t *testing.T) {
			_, err := cl.FilterAuthorizedPairs(ctx,
				&authz.FilterAuthorizedPairsReq{
					Subjects: []string{"team:local:admins1"},
					Pairs:    []*authz.Pair{{Action: "read", Resource: "nodes:run_list"}},
				})
			grpctest.AssertCode(t, codes.Internal, err)
		})
	})

	// Note: PurgeSubjectFromPolicies has no engine interaction beyond
	// SetPolicies, and thus doesn't fit into this test suite.
}

func TestAuthzGRPCWithStorageErrors(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	conn, g, _ := newGRPCServer(ctx, t, mockStore.New(), useDefaultEngine, false)
	defer g.Close()
	cl := authz.NewAuthorizationClient(conn)

	t.Run("CreatePolicy", func(t *testing.T) {
		req := &authz.CreatePolicyReq{
			Action:   "notrelevant",
			Subjects: []string{"user:local:not_relevant"},
			Resource: "foo:not_relevant",
		}

		resp, err := cl.CreatePolicy(ctx, req)
		require.Nil(t, resp)
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("DeletePolicy", func(t *testing.T) {
		req := &authz.DeletePolicyReq{
			Id: "bce6666f-c130-42db-8f56-f52f42ae7418", // valid UUIDv4
		}

		resp, err := cl.DeletePolicy(ctx, req)
		require.Nil(t, resp)
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("ListPolicies", func(t *testing.T) {
		req := &authz.ListPoliciesReq{}

		resp, err := cl.ListPolicies(ctx, req)
		require.Nil(t, resp)
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("PurgeSubjectFromPolicies", func(t *testing.T) {
		_, err := cl.PurgeSubjectFromPolicies(ctx,
			&authz.PurgeSubjectFromPoliciesReq{
				Subject: "team:local:admins1",
			})
		grpctest.AssertCode(t, codes.Internal, err)
	})
}

func TestAuthzGRPCInteractionWithTestEngineStore(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	testRecordEngine := &testRecordEngine{}
	l, err := logger.NewLogger("text", "warn")
	require.NoError(t, err, "init logger for storage")
	mem, err := memstore.New(l)
	require.NoError(t, err, "init memstore server")
	conn, g, _ := newGRPCServer(ctx, t, mem, testRecordEngine, true)
	defer g.Close()
	cl := authz.NewAuthorizationClient(conn)

	t.Run("CreatePolicy updates the engine", func(t *testing.T) {
		tests := map[string][]*authz.CreatePolicyReq{
			"multiple policies": {req1, req2, req3},
			"a single policy":   {req3},
			"no policies":       {},
		}
		for desc, testPolicies := range tests {
			t.Run(desc, func(t *testing.T) {
				// arrange + act: the action we're testing happens implicitly
				policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)

				// assert
				// Note: The response and its qualities have been tested above -- here,
				// we're interested in the interaction with the engine's store.
				assertInterfaceMapLength(t, testRecordEngine.data, len(testPolicies),
					"the numbers of both stores should match")
				for _, req := range testPolicies {
					assertInterfaceMapContainsPolicy(t, testRecordEngine.data, req)
				}
				cleanupPolicies(ctx, t, cl, policyResponses)
			})
		}
	})

	t.Run("DeletePolicy updates the engine", func(t *testing.T) {

		testPolicies := []*authz.CreatePolicyReq{req1, req2, req3}

		tests := map[string][]int{
			"delete first policy":  {0},
			"delete second policy": {1},
			"delete third policy":  {2},
			"keep first policy":    {1, 2},
			"keep second policy":   {0, 2},
			"keep third policy":    {0, 1},
			"delete all policies":  {0, 1, 2},
		}
		for desc, nums := range tests {
			t.Run(desc, func(t *testing.T) {
				// arrange
				policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)
				targetIDs := []string{}

				// act
				for i := range nums {
					id := policyResponses[i]
					targetIDs = append(targetIDs, id)
					_, _ = cl.DeletePolicy(ctx, &authz.DeletePolicyReq{Id: id})
				}

				// assert
				assertInterfaceMapLength(t, testRecordEngine.data, 3-len(nums),
					"the numbers of both stores should match")

				// check that we have the policies that have not been deleted in the
				// engine store
				for i := 0; i < len(testPolicies); i++ {
					isTarget := false
					for n := range nums {
						if i == n {
							isTarget = true
						}
					}
					if !isTarget {
						assertInterfaceMapContainsPolicy(t, testRecordEngine.data, testPolicies[i])
					}
				}

				cleanupPoliciesExceptList(ctx, t, cl, policyResponses, targetIDs)
			})
		}
	})
}

// Note: the interesting tests around different policies, etc, happen in
//   engine/conformance/conformance_test.go
// The tests here are only concerned with the interplay between the various
// functionalities (policy store, engine store, authorized API) exposed via
// the GRPC API.
func TestAuthzGRPCInteractionWithEngine(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	l, err := logger.NewLogger("text", "warn")
	if err != nil {
		t.Fatalf("could not init logger: %s", err)
	}

	opa, err := opa.New(ctx, l)
	if err != nil {
		t.Fatalf("could not init opa engine: %s", err)
	}
	mem, err := memstore.New(l)
	require.NoError(t, err, "init memstore server")
	conn, g, _ := newGRPCServer(ctx, t, mem, opa, true)
	defer g.Close()
	cl := authz.NewAuthorizationClient(conn)

	t.Run("IsAuthorized response changes when policies are changed", func(t *testing.T) {
		reqQuery :=
			&authz.IsAuthorizedReq{
				Subjects: []string{"team:local:admins1"},
				Action:   "read",
				Resource: "auth:users",
			}
		// assert team is unauthorized prior to addition of policy
		resp, err := cl.IsAuthorized(ctx, reqQuery)
		require.NoError(t, err)
		assert.False(t, resp.Authorized)

		// arrange + act (store gets updated implicitly)
		policyReq := &authz.CreatePolicyReq{
			Action:   "read",
			Subjects: []string{"team:local:admins1", "team:local:admins2"},
			Resource: "auth:users",
		}
		testPolicies := []*authz.CreatePolicyReq{policyReq}
		policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)

		// assert team is now authorized
		resp, err = cl.IsAuthorized(ctx, reqQuery)
		require.NoError(t, err)
		assert.True(t, resp.Authorized)
		cleanupPolicies(ctx, t, cl, policyResponses)
	})

	t.Run("IsAuthorized response changes when a subject is purged from policies", func(t *testing.T) {
		// create a policy including the subject we'll purge
		policyReq := &authz.CreatePolicyReq{
			Action:   "read",
			Subjects: []string{"team:local:admins1", "team:local:admins2"},
			Resource: "auth:users",
		}
		testPolicies := []*authz.CreatePolicyReq{policyReq}
		policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)

		// assert team is authorized prior to purging their subject
		reqQuery := &authz.IsAuthorizedReq{
			Subjects: []string{"team:local:admins1"},
			Action:   "read",
			Resource: "auth:users",
		}
		isAuthorizedResp, err := cl.IsAuthorized(ctx, reqQuery)
		require.NoError(t, err)
		assert.True(t, isAuthorizedResp.Authorized)

		// arrange + act (store gets updated implicitly)
		purgeReq := &authz.PurgeSubjectFromPoliciesReq{Subject: "team:local:admins1"}
		resp, err := cl.PurgeSubjectFromPolicies(ctx, purgeReq)
		require.NoError(t, err)
		assert.Equal(t, policyResponses, resp.Ids)

		// assert team is now unauthorized
		isAuthorizedResp, err = cl.IsAuthorized(ctx, reqQuery)
		require.NoError(t, err)
		assert.False(t, isAuthorizedResp.Authorized)
		cleanupPolicies(ctx, t, cl, policyResponses)
	})

	t.Run("FilterAuthorizedPairs response changes when policies are changed", func(t *testing.T) {

		req := &authz.FilterAuthorizedPairsReq{
			Subjects: []string{"team:local:admins1"},
			Pairs:    []*authz.Pair{{Action: "read", Resource: "auth:users"}},
		}
		// assert team is unauthorized prior to addition of policy
		resp, err := cl.FilterAuthorizedPairs(ctx, req)
		require.NoError(t, err)
		assert.Zero(t, len(resp.Pairs))

		// arrange + act (store gets updated implicitly)
		policyReq := &authz.CreatePolicyReq{
			Action:   "read",
			Subjects: []string{"team:local:admins1", "team:local:admins2"},
			Resource: "auth:users",
		}
		testPolicies := []*authz.CreatePolicyReq{policyReq}
		policyResponses := generateTestPolicies(ctx, t, cl, testPolicies)

		// assert team is now authorized
		resp, err = cl.FilterAuthorizedPairs(ctx, req)
		require.NoError(t, err)
		assert.Equal(t, 1, len(resp.Pairs))
		cleanupPolicies(ctx, t, cl, policyResponses)
	})
}

func TestHealthGRPC(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	conn, g, _ := newGRPCServer(ctx, t, mockStore.New(), useDefaultEngine, false)
	defer g.Close()
	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		// Note 2018/04/16 (sr): we don't supply a service name from the health
		// check hook (yet) -- this could be subsystems, e.g. database, engine,
		// ...
		t.Run("returns serving", func(t *testing.T) {
			actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
			require.NoError(t, err)
			assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
		})
	})
}

// ***************************** SUPPORT FUNCTIONS *****************************

// default test engine: return authorized = true iff subjects contains "admin"
type testEngine struct{}

func (*testEngine) IsAuthorized(
	_ context.Context,
	subjects engine.Subjects,
	_ engine.Action,
	_ engine.Resource) (bool, error) {
	return stringutils.SliceContains(subjects, "user:local:admin"), nil
}

func (*testEngine) FilterAuthorizedPairs(
	_ context.Context,
	subjects engine.Subjects,
	pairs []engine.Pair) ([]engine.Pair, error) {
	filtered := []engine.Pair{}
	for _, p := range pairs {
		if p.Action == "pass" { // special action to control mock response in tests
			filtered = append(filtered, p)
		}
	}
	return filtered, nil
}

func (*testEngine) SetPolicies(context.Context, map[string]interface{}) error {
	return nil
}

type testErrorEngine struct{}

func (*testErrorEngine) IsAuthorized(context.Context, engine.Subjects, engine.Action,
	engine.Resource) (bool, error) {
	return false, errors.New("engine failure")
}

func (*testErrorEngine) FilterAuthorizedPairs(context.Context, engine.Subjects,
	[]engine.Pair) ([]engine.Pair, error) {
	return nil, errors.New("engine failure")
}

func (*testErrorEngine) SetPolicies(context.Context, map[string]interface{}) error {
	return nil
}

type testRecordEngine struct {
	data map[string]interface{}
}

func (*testRecordEngine) IsAuthorized(context.Context, engine.Subjects, engine.Action,
	engine.Resource) (bool, error) {
	return false, errors.New("not implemented")
}

func (*testRecordEngine) FilterAuthorizedPairs(context.Context, engine.Subjects,
	[]engine.Pair) ([]engine.Pair, error) {
	return nil, errors.New("not implemented")
}

func (tre *testRecordEngine) SetPolicies(_ context.Context, data map[string]interface{}) error {
	tre.data = data
	return nil
}

func cleanupPolicies(ctx context.Context, t *testing.T, cl authz.AuthorizationClient, ids []string) {
	for _, id := range ids {
		cleanupPolicy(ctx, t, cl, id)
	}
}

func cleanupPoliciesExceptOne(ctx context.Context, t *testing.T, cl authz.AuthorizationClient, ids []string, targetId string) {
	cleanupPoliciesExceptList(ctx, t, cl, ids, []string{targetId})
}

func cleanupPoliciesExceptList(ctx context.Context, t *testing.T, cl authz.AuthorizationClient, ids []string, targetIds []string) {
	for _, id := range ids {
		if !stringutils.SliceContains(targetIds, id) {
			cleanupPolicy(ctx, t, cl, id)
		}
	}
}

func cleanupPolicy(ctx context.Context, t *testing.T, cl authz.AuthorizationClient, id string) {
	t.Helper()
	policyID := authz.DeletePolicyReq{Id: id}
	_, err := cl.DeletePolicy(ctx, &policyID)
	assert.NoError(t, err)
}

func generateTestPolicies(ctx context.Context, t *testing.T,
	cl authz.AuthorizationClient, policies []*authz.CreatePolicyReq) []string {

	t.Helper()
	policyResponses := make([]string, len(policies))
	for i, req := range policies {
		resp, err := cl.CreatePolicy(ctx, req)
		if assert.NoError(t, err) {
			policyResponses[i] = resp.Policy.Id
		}
	}
	return policyResponses
}

func subjectsEqual(as, bs []string) bool {
	eq := true
	for i := range as {
		if as[i] != bs[i] {
			eq = false
		}
	}
	return eq
}

func listContainsPolicy(policies []*authz.Policy, target *authz.CreatePolicyReq) bool {
	for _, policy := range policies {
		if target.Action == policy.Action &&
			subjectsEqual(target.Subjects, policy.Subjects) &&
			target.Resource == policy.Resource {
			return true
		}
	}
	return false
}

// This is for checking the testRecordingEngine's data
func assertInterfaceMapContainsPolicy(t *testing.T,
	data map[string]interface{}, target *authz.CreatePolicyReq) bool {

	t.Helper()

	targetDatum := map[string]interface{}{
		"action":   target.Action,
		"subjects": target.Subjects,
		"resource": target.Resource,
	}
	for _, datum := range data {
		if policy, ok := datum.(map[string]interface{}); assert.True(t, ok) {
			if reflect.DeepEqual(policy, targetDatum) {
				return true
			}
		}
	}
	return false
}

// Note: `rest ...interface{}` allows passing extra arguments to the underlying
// assert.Equal call, for example a message (string), or a format string plus
// data (e.g. "should be bigger than 300: %d", 213)
func assertInterfaceMapLength(t *testing.T, data map[string]interface{}, length int, rest ...interface{}) {
	t.Helper()
	assert.Equal(t, length+len(constants.DefaultPolicyIDs)+len(server.SystemPolicies()), len(data), rest...)
}

// Would not need this empty check if there was a way to ensure
// a clean slate upon starting each test.
func assertPolicyStoreLength(
	ctx context.Context, t *testing.T, cl authz.AuthorizationClient, length int) {

	t.Helper()
	emptyList, err := cl.ListPolicies(ctx, &authz.ListPoliciesReq{})
	require.NoError(t, err)
	require.NotNil(t, emptyList)
	assert.Equal(t, length+len(constants.DefaultPolicyIDs), len(emptyList.Policies))
}

func newGRPCServer(ctx context.Context,
	t *testing.T, store storage.Storage, engine engine.V1Engine, initPolicies bool) (
	*grpc.ClientConn, *grpctest.Server, *server.Server) {
	var serv *server.Server
	if store == nil {
		t.Fatal("must pass valid store to helper newGRPCServerAndClient")
	}

	l, err := logger.NewLogger("text", "warn")
	if err != nil {
		t.Fatal("could not init logger")
	}

	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	if engine == nil {
		engine = &testEngine{}
	}

	serv, err = server.NewWithStore(ctx, l, engine, store, initPolicies, true, nil)
	if err != nil {
		t.Fatalf("could not init server: %s", err.Error())
	}

	g := connFactory.NewServer(grpc.UnaryInterceptor(
		grpc_middleware.ChainUnaryServer(
			tracing.ServerInterceptor(tracing.GlobalTracer()),
			grpc_server.InputValidationInterceptor(),
		),
	),
	)

	health.RegisterHealthServer(g, health.NewService())
	authz.RegisterAuthorizationServer(g, serv)

	reflection.Register(g)

	testServ := grpctest.NewServer(g)

	conn, err := connFactory.Dial("authz-service", testServ.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	return conn, testServ, serv
}

func expectSuccess(cl authz.AuthorizationClient, req authz.CreatePolicyReq) func(*testing.T) {
	return func(t *testing.T) {
		ctx := context.Background()
		resp, err := cl.CreatePolicy(ctx, &req)

		// assert
		require.NoError(t, err)
		require.NotNil(t, resp)
		assert.Equal(t, req.Action, resp.Policy.Action)
		assert.Equal(t, req.Subjects, resp.Policy.Subjects)
		assert.Equal(t, uuidLength, len(resp.Policy.Id))
		assert.Equal(t, req.Resource, resp.Policy.Resource)
		assert.Equal(t, defaultEffect, resp.Policy.Effect)
		ts, err := ptypes.Timestamp(resp.Policy.CreatedAt)
		require.NoError(t, err)
		assert.WithinDuration(t, time.Now(), ts, time.Second)

		cleanupPolicy(ctx, t, cl, resp.Policy.Id)
	}
}

func expectFailureWithCode(cl authz.AuthorizationClient,
	code codes.Code,
	req authz.CreatePolicyReq) func(*testing.T) {
	return func(t *testing.T) {
		ctx := context.Background()
		resp, err := cl.CreatePolicy(ctx, &req)
		if assert.Nil(t, resp) {
			grpctest.AssertCode(t, code, err)
			return
		}

		cleanupPolicy(ctx, t, cl, resp.Policy.Id)
	}
}

// TODO: refactor (copied for now, from postgres_test)
func migrationConfigIfPGTestsToBeRun(l logger.Logger, migrationPath string) (*migration.Config, error) {
	customPGURL, pgURLPassed := os.LookupEnv("PG_URL")
	ciMode := os.Getenv("CI") == "true"

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
