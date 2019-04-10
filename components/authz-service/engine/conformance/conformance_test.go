// conformance_test is testing (potentially various) decision engine
// implementations
package conformance_test

import (
	"context"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strconv"
	"testing"

	"github.com/open-policy-agent/opa/ast"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/logger"

	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/engine/opa"
)

func TestIsAuthorized(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res := "user:local:admin", "read", "auth:users:admin"

	// We're always passing the same arguments to IsAuthorized(). This allows for
	// not having to wrap sub/act/res in _every call_; instead, we pass args().
	args := func() (context.Context, engine.Subjects, engine.Action, engine.Resource) {
		return ctx, engine.Subject(sub), engine.Action(act), engine.Resource(res)
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {
			t.Run("when the store is empty, returns false", func(t *testing.T) {
				actual, err := e.IsAuthorized(args())
				require.NoError(t, err)
				assert.False(t, actual)
			})

			policies := map[string]map[string]interface{}{
				"single subject": {
					"subjects": engine.Subject(sub),
					"action":   act,
					"resource": res,
					"effect":   "allow",
				},
				"multiple subjects": {
					"subjects": engine.Subject("team:local:special-ops", sub),
					"action":   act,
					"resource": res,
					"effect":   "allow",
				},
				"single subject, resource with variable": {
					"subjects": engine.Subject(sub),
					"action":   act,
					"resource": "auth:users:${a2:username}",
					"effect":   "allow",
				},
			}

			for desc, pol := range policies {
				t.Run(fmt.Sprintf("with one policy and %s in store", desc), func(t *testing.T) {
					// set data in engine store
					setPolicies(t, e, pol)
					matchingSubject := sub
					matchingAction := act
					matchingResource := res

					negativeCases := map[string]struct {
						subs engine.Subjects
						act  engine.Action
						res  engine.Resource
					}{
						"when input SUBJECT does not match": {
							engine.Subject("team:local:unmatchable"),
							engine.Action(matchingAction),
							engine.Resource(matchingResource),
						},

						"when input ACTION does not match": {
							engine.Subject(matchingSubject),
							engine.Action("unmatchable"),
							engine.Resource(matchingResource),
						},

						"when input RESOURCE does not match": {
							engine.Subject(matchingSubject),
							engine.Action(matchingAction),
							engine.Resource("nodes:unmatchable"),
						},

						"when input SUBJECT does not match CASE": {
							engine.Subject("team:local:Admins"),
							engine.Action(matchingAction),
							engine.Resource(matchingResource),
						},

						"when input ACTION does not match CASE": {
							engine.Subject(matchingSubject),
							engine.Action("Read"),
							engine.Resource(matchingResource),
						},

						"when input RESOURCE does not match CASE": {
							engine.Subject(matchingSubject),
							engine.Action(matchingAction),
							engine.Resource("nodes:Property"),
						},

						"when none of the input subjects match": {
							engine.Subjects([]string{"team:local:unmatchable1", "team:local:unmatchable2"}),
							engine.Action(matchingAction),
							engine.Resource(matchingResource),
						},
					}

					for desc, tc := range negativeCases {
						t.Run(desc+", returns false", func(t *testing.T) {
							actual, err := e.IsAuthorized(ctx, tc.subs, tc.act, tc.res)
							require.NoError(t, err)
							assert.False(t, actual)
						})
					}

					positiveCases := map[string]struct {
						subs engine.Subjects
						act  engine.Action
						res  engine.Resource
					}{
						"when all inputs match": {
							engine.Subject(matchingSubject),
							engine.Action(matchingAction),
							engine.Resource(matchingResource),
						},

						"when one of the input subjects matches": {
							engine.Subject("team:local:developers", matchingSubject),
							engine.Action(matchingAction),
							engine.Resource(matchingResource),
						},

						"when duplicate subjects match": {
							engine.Subject(matchingSubject, "team:local:developers", matchingSubject),
							engine.Action(matchingAction),
							engine.Resource(matchingResource),
						},
					}

					for desc, tc := range positiveCases {
						t.Run(desc+", returns true", func(t *testing.T) {
							actual, err := e.IsAuthorized(ctx, tc.subs, tc.act, tc.res)
							require.NoError(t, err)
							assert.True(t, actual)
						})
					}
				})
			}

			t.Run("with two CONFLICTING policies in store", func(t *testing.T) {
				policyDataAllow := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act,
					"resource": res,
					"effect":   "allow",
				}
				policyDataDeny := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act,
					"resource": res,
					"effect":   "deny",
				}

				t.Run("when input matches multiple conflicting policies, returns false", func(t *testing.T) {
					setPolicies(t, e, policyDataAllow, policyDataDeny)

					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					assert.False(t, actual)
				})

				t.Run("when input matches multiple conflicting policies (stored in reverse order), returns false", func(t *testing.T) {
					// reverse order
					setPolicies(t, e, policyDataDeny, policyDataAllow)

					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					assert.False(t, actual)
				})
			})

			t.Run("with subjects-less (unattached) allow policy in store", func(t *testing.T) {
				policyDataAllow := map[string]interface{}{
					"subjects": engine.Subject(),
					"action":   act,
					"resource": res,
					"effect":   "allow",
				}

				t.Run("when input matches the policy (for action/resource), returns false", func(t *testing.T) {
					setPolicies(t, e, policyDataAllow)

					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					assert.False(t, actual)
				})
			})
		})
	}
}

func TestHierarchicalResourcePolicies(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res := "user:local:someid", "read", "compliance:scans:123"
	args := func() (context.Context, engine.Subjects, engine.Action, engine.Resource) {
		return ctx, engine.Subject(sub), engine.Action(act), engine.Resource(res)
	}

	// different hierarchical resources that each match the test resource
	hierarchicalResources := []string{
		"*",
		"compliance:*",
		"compliance:scans:*",
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {

			for _, hierarchicalResource := range hierarchicalResources {
				t.Run(fmt.Sprintf("one policy with hierarchical resource %q, decision 'allow'", hierarchicalResource),
					func(t *testing.T) {
						policy := map[string]interface{}{
							"subjects": engine.Subject(sub),
							"action":   act,
							"resource": hierarchicalResource,
							"effect":   "allow",
						}
						setPolicies(t, e, policy)

						actual, err := e.IsAuthorized(args())
						require.NoError(t, err)
						assert.True(t, actual)
					})

				t.Run(fmt.Sprintf("two policies, one with specified resource, decision 'allow'; hierarchical resource %q, decision 'deny'", hierarchicalResource),
					func(t *testing.T) {
						allowPolicy := map[string]interface{}{
							"subjects": engine.Subject(sub),
							"action":   act,
							"resource": res,
							"effect":   "allow",
						}
						setPolicies(t, e, allowPolicy)

						// check that we get an allow, so this non-hierarchical policy is matching
						actual, err := e.IsAuthorized(args())
						require.NoError(t, err)
						require.True(t, actual)

						denyPolicy := map[string]interface{}{
							"subjects": engine.Subject(sub),
							"action":   act,
							"resource": hierarchicalResource,
							"effect":   "deny",
						}
						setPolicies(t, e, allowPolicy, denyPolicy)

						// now, having both the matching allow-policy, and a new deny-policy
						// with a hierarchical resource, this should end in deny
						actual, err = e.IsAuthorized(args())
						require.NoError(t, err)
						assert.False(t, actual)
					})

				t.Run(fmt.Sprintf("two policies, one with specified resource, decision 'deny'; hierarchical resource %q, decision 'allow'", hierarchicalResource),
					func(t *testing.T) {
						allowPolicy := map[string]interface{}{
							"subjects": engine.Subject(sub),
							"action":   act,
							"resource": hierarchicalResource,
							"effect":   "allow",
						}
						setPolicies(t, e, allowPolicy)

						// check that we get an allow, so this wildcard policy is matching
						actual, err := e.IsAuthorized(args())
						require.NoError(t, err)
						assert.True(t, actual)

						denyPolicy := map[string]interface{}{
							"subjects": engine.Subject(sub),
							"action":   act,
							"resource": res,
							"effect":   "deny",
						}
						setPolicies(t, e, allowPolicy, denyPolicy)

						// now, having both the matching deny-policy for a specified resource,
						// and a new allow-policy with a hierarchical resource, this should
						// still end in deny
						actual, err = e.IsAuthorized(args())
						require.NoError(t, err)
						assert.False(t, actual)
					})
			}
		})
	}
}

func TestWildcardActionPolicies(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res := "user:local:someid", "read", "nodes:property"
	args := func() (context.Context, engine.Subjects, engine.Action, engine.Resource) {
		return ctx, engine.Subject(sub), engine.Action(act), engine.Resource(res)
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {

			t.Run("one policy with wildcard action, decision 'allow'",
				func(t *testing.T) {
					policy := map[string]interface{}{
						"subjects": engine.Subject(sub),
						"action":   "*",
						"resource": res,
						"effect":   "allow",
					}
					setPolicies(t, e, policy)

					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					assert.True(t, actual)
				})

			t.Run("two policies, one with specified action, decision 'allow'; one with wildcard action, decision 'deny'",
				func(t *testing.T) {
					allowPolicy := map[string]interface{}{
						"subjects": engine.Subject(sub),
						"action":   act,
						"resource": res,
						"effect":   "allow",
					}
					setPolicies(t, e, allowPolicy)

					// check that we get an allow, so this non-wildcard policy is matching
					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					require.True(t, actual)

					denyPolicy := map[string]interface{}{
						"subjects": engine.Subject(sub),
						"action":   "*",
						"resource": res,
						"effect":   "deny",
					}
					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching allow-policy, and a new deny-policy
					// with a wildcard subject, this should end in deny
					actual, err = e.IsAuthorized(args())
					require.NoError(t, err)
					assert.False(t, actual)
				})

			t.Run("two policies, one with specified action, decision 'deny'; one with wildcard action, decision 'allow'",
				func(t *testing.T) {
					allowPolicy := map[string]interface{}{
						"subjects": engine.Subject(sub),
						"action":   "*",
						"resource": res,
						"effect":   "allow",
					}
					setPolicies(t, e, allowPolicy)

					// check that we get a deny, so this non-wildcard policy is matching
					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					assert.True(t, actual)

					denyPolicy := map[string]interface{}{
						"subjects": engine.Subject(sub),
						"action":   act,
						"resource": res,
						"effect":   "deny",
					}
					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching deny-policy for a specified action,
					// and a new allow-policy with a wildcard action, this should still
					// end in deny
					actual, err = e.IsAuthorized(args())
					require.NoError(t, err)
					assert.False(t, actual)
				})
		})
	}
}

func TestWildcardSubjectsPolicies(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res := "user:local:someid", "read", "nodes:property"
	args := func() (context.Context, engine.Subjects, engine.Action, engine.Resource) {
		return ctx, engine.Subject(sub), engine.Action(act), engine.Resource(res)
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {
			matchingSubjects := []string{
				"*",
				"user:*",
				"user:local:*",
			}
			for _, matchingSub := range matchingSubjects {
				t.Run(fmt.Sprintf("one policy with wildcard subject %q, decision 'allow'", matchingSub), func(t *testing.T) {
					policy := map[string]interface{}{
						"subjects": engine.Subject(matchingSub),
						"action":   act,
						"resource": res,
						"effect":   "allow",
					}
					setPolicies(t, e, policy)

					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					assert.True(t, actual)
				})

				t.Run(fmt.Sprintf("two policies, one with specified subject, decision 'allow'; one with wildcard subject %q, decision 'deny'", matchingSub), func(t *testing.T) {
					allowPolicy := map[string]interface{}{
						"subjects": engine.Subject(sub),
						"action":   act,
						"resource": res,
						"effect":   "allow",
					}
					setPolicies(t, e, allowPolicy)

					// check that we get an allow, so this non-wildcard policy is matching
					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					require.True(t, actual)

					denyPolicy := map[string]interface{}{
						"subjects": engine.Subject(matchingSub),
						"action":   act,
						"resource": res,
						"effect":   "deny",
					}
					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching allow-policy, and a new deny-policy
					// with a wildcard subject, this should end in deny
					actual, err = e.IsAuthorized(args())
					require.NoError(t, err)
					assert.False(t, actual)
				})

				t.Run(fmt.Sprintf("two policies, one with specified subject, decision 'deny'; one with wildcard subject %q, decision 'allow'", matchingSub), func(t *testing.T) {
					allowPolicy := map[string]interface{}{
						"subjects": engine.Subject(matchingSub),
						"action":   act,
						"resource": res,
						"effect":   "allow",
					}
					setPolicies(t, e, allowPolicy)

					// check that we get a deny, so this non-wildcard policy is matching
					actual, err := e.IsAuthorized(args())
					require.NoError(t, err)
					assert.True(t, actual)

					denyPolicy := map[string]interface{}{
						"subjects": engine.Subject(sub),
						"action":   act,
						"resource": res,
						"effect":   "deny",
					}
					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching deny-policy for a specified subject,
					// and a new allow-policy with a wildcard subject, this should still
					// end in deny
					actual, err = e.IsAuthorized((args()))
					require.NoError(t, err)
					assert.False(t, actual)
				})
			}
		})
	}
}

func TestFilterAuthorizedPairs(t *testing.T) {
	ctx, engines := setup(t)
	sub, act0, res0, act1, res1 := "user:local:someid", "read", "nodes:someid", "delete", "compliance:profiles"
	pair0 := engine.Pair{Resource: engine.Resource(res0), Action: engine.Action(act0)}
	pair1 := engine.Pair{Resource: engine.Resource(res1), Action: engine.Action(act1)}
	args := func() (context.Context, engine.Subjects, []engine.Pair) {
		return ctx, engine.Subject(sub), []engine.Pair{pair0, pair1}
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {

			t.Run("no matching policies", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act0,
					"resource": "nodes:nomatch",
					"effect":   "allow",
				}
				policy1 := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   "nomatch",
					"resource": res1,
					"effect":   "allow",
				}
				setPolicies(t, e, policy0, policy1)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, filtered, []engine.Pair{})
			})

			t.Run("one policy with wildcard subject matching one of the pairs, decision 'allow'", func(t *testing.T) {
				policy := map[string]interface{}{
					"subjects": engine.Subject("*"),
					"action":   act0,
					"resource": res0,
					"effect":   "allow",
				}
				setPolicies(t, e, policy)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, filtered, []engine.Pair{pair0})
			})

			t.Run("two policies matching each one of the pairs, decision 'allow'", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act0,
					"resource": res0,
					"effect":   "allow",
				}
				policy1 := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act1,
					"resource": res1,
					"effect":   "allow",
				}
				setPolicies(t, e, policy0, policy1)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, filtered, []engine.Pair{pair0, pair1})
			})

			// Note: this test case only has value in conjunction with the preceding
			// one -- to avoid phantom tests, and false security in "deny" responses
			t.Run("two policies matching each one of the pairs, one 'allow', one 'deny'", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act0,
					"resource": res0,
					"effect":   "allow",
				}
				policy1 := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act1,
					"resource": res1,
					"effect":   "deny",
				}
				setPolicies(t, e, policy0, policy1)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, filtered, []engine.Pair{pair0})
			})

			t.Run("one policy with wildcard action matching one of the pairs, decision 'allow'", func(t *testing.T) {
				policy := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   "*",
					"resource": res0,
					"effect":   "allow",
				}
				setPolicies(t, e, policy)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, filtered, []engine.Pair{pair0})
			})

			t.Run("one policy with wildcard resource matching one of the pairs, decision 'allow'", func(t *testing.T) {
				policy := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act0,
					"resource": "nodes:*",
					"effect":   "allow",
				}
				setPolicies(t, e, policy)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, filtered, []engine.Pair{pair0})
			})

			t.Run("one policy with variable resource matching one of the pairs, decision 'allow'", func(t *testing.T) {
				policy := map[string]interface{}{
					"subjects": engine.Subject(sub),
					"action":   act0,
					"resource": "nodes:${a2:username}",
					"effect":   "allow",
				}
				setPolicies(t, e, policy)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, filtered, []engine.Pair{pair0})
			})
		})
	}
}

var isAuthResult bool

func BenchmarkIsAuthorized(b *testing.B) {
	ctx, engines := setup(b)
	subs := engine.Subject("token:66ea15b5-e799-4db6-929a-8cceb5bc044d")
	res, act := engine.Resource("ingest:legacy"), engine.Action("write")
	var r bool
	var err error
	for name, s := range engines {
		b.Run(name, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				r, err = s.IsAuthorized(ctx, subs, act, res)
				if err != nil {
					b.Error(err)
				}
			}
			isAuthResult = r
		})
	}
}

var filterResult []engine.Pair

func BenchmarkFilterAuthorizedPairs(b *testing.B) {
	ctx, engines := setup(b)
	subs := engine.Subject("token:66ea15b5-e799-4db6-929a-8cceb5bc044d")
	pairs := []engine.Pair{{Resource: "ingest:legacy", Action: "write"}}
	var r []engine.Pair
	var err error
	for name, s := range engines {
		b.Run(name, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				r, err = s.FilterAuthorizedPairs(ctx, subs, pairs)
				if err != nil {
					b.Error(err)
				}
			}
			filterResult = r
		})
	}
}

func setup(t testing.TB) (context.Context, map[string]engine.Engine) {
	t.Helper()
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger")

	mods := map[string]*ast.Module{}
	files, err := filepath.Glob("../opa/policy/*.rego")
	require.NoError(t, err, "could not glob *.rego files")

	for _, file := range files {
		data, err := ioutil.ReadFile(file)
		require.NoErrorf(t, err, "could not read rego file %q", file)
		parsed, err := ast.ParseModule(file, string(data))
		require.NoErrorf(t, err, "could not parse rego file %q", file)
		mods[file] = parsed
	}

	o, err := opa.New(ctx, l, opa.WithModules(mods))
	require.NoError(t, err, "could not initialize OPA")

	engines := map[string]engine.Engine{
		"opa": o,
	}
	return ctx, engines
}

func setPolicies(t testing.TB, e engine.Engine, policies ...interface{}) {
	t.Helper()
	ctx := context.Background()
	// Simulate GUID by using i (just has to be unique)
	policyMap := make(map[string]interface{})
	for i, pol := range policies {
		policyMap[strconv.Itoa(i)] = pol
	}
	err := e.SetPolicies(ctx, policyMap)
	require.NoError(t, err, "set policies")
}
