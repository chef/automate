package conformance_test

import (
	"context"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
)

/************ ************ ************ ************ ************ ************
 * NOTE: These tests are mini-integration tests for the OPA engine,          *
 * confirming that our engine infrastructure on top of OPA is doing its job. *
 *                                                                           *
 * For lower level OPA tests, see opa_v2_test.go.                            *
 * For the next-higher level up integration tests, see authz_test.go,        *
 * which mock the engine results.                                            *
 ************ ************ ************ ************ ************ ************/

func TestV2IsAuthorized(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res := "user:local:admin", "iam:users:create", "iam:users"

	// We're always passing the same arguments to IsAuthorized(). This allows for
	// not having to wrap sub/act/res in _every call_; instead, we pass args().
	args := func() (context.Context, engine.Subjects, engine.Action, engine.Resource) {
		return ctx, engine.Subject(sub), engine.Action(act), engine.Resource(res)
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {
			t.Run("when the store is empty, returns false", func(t *testing.T) {
				actual, err := e.V2IsAuthorized(args())
				require.NoError(t, err)
				assert.False(t, actual)
			})

			t.Run("when one policy with one allow statement (inline action) matches, returns true", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2IsAuthorized(args())
				require.NoError(t, err)
				assert.True(t, actual)
			})

			t.Run("when one policy with one allow statement (via role) matches, returns true", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"role":      "handyman",
							"resources": []string{res},
							"effect":    "allow",
						},
					},
				}
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act},
				}
				setPoliciesV2(t, e, pol, role)
				actual, err := e.V2IsAuthorized(args())
				require.NoError(t, err)
				assert.True(t, actual)
			})
		})
	}
}

func TestV2ProjectsAuthorized(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res, proj1, proj2 := "user:local:admin", "iam:users:create", "iam:users", "proj-1", "proj-2"

	// We're always passing the same arguments to ProjectsAuthorized(). This allows for
	// not having to wrap sub/act/res in _every call_; instead, we pass args().
	args := func(projects []string) (context.Context, engine.Subjects, engine.Action, engine.Resource, engine.Projects) {
		return ctx, engine.Subject(sub), engine.Action(act), engine.Resource(res), engine.Projects(projects)
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {
			t.Run("when the store is empty, returns empty list", func(t *testing.T) {
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.Equal(t, []string{}, actual)
			})

			t.Run("policy with one of the requested projects returns matched project", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{proj1},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.Equal(t, []string{proj1}, actual)
			})

			t.Run("policy with some of the requested projects returns matched projects", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"role":      "handyman",
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{proj1, "other-project", proj2},
						},
					},
				}
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act},
				}
				setPoliciesV2(t, e, pol, role)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1, proj2}, actual)
			})

			t.Run("policy with no matching projects returns empty list", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"role":      "handyman",
							"resources": []string{res},
							"projects":  []string{"other-project"},
							"effect":    "allow",
						},
					},
				}
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act},
				}
				setPoliciesV2(t, e, pol, role)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.Equal(t, []string{}, actual)
			})

			// including this case to demonstrate requests containing [] projects (aka All Projects)
			// match on any project in policy statements
			t.Run("policy with one matching project returns that project when request contains no projects", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"role":      "handyman",
							"resources": []string{res},
							"projects":  []string{proj1},
							"effect":    "allow",
						},
					},
				}
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act},
				}
				setPoliciesV2(t, e, pol, role)
				actual, err := e.V2ProjectsAuthorized(args([]string{}))
				require.NoError(t, err)
				assert.Equal(t, []string{proj1}, actual)
			})

			// including this case to demonstrate the All Projects ID cannot be passed in the request
			// in cases when no filter should be applied
			t.Run("policy with one matching project returns no matching projects when request contains *", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"role":      "handyman",
							"resources": []string{res},
							"projects":  []string{proj1},
							"effect":    "allow",
						},
					},
				}
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act},
				}
				setPoliciesV2(t, e, pol, role)
				actual, err := e.V2ProjectsAuthorized(args([]string{"*"}))
				require.NoError(t, err)
				assert.Equal(t, []string{}, actual)
			})

			t.Run("policy denying one of the requested projects returns no projects", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "deny",
							"projects":  []string{proj1},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.Equal(t, []string{}, actual)
			})

			t.Run("policy denying some of the requested projects returns no projects", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "deny",
							"projects":  []string{proj1, "other-project", proj2},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{}, actual)
			})

			t.Run("policy with allow and deny statements returns only allowed project", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "deny",
							"projects":  []string{proj1},
						},
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{proj2},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj2}, actual)
			})

			t.Run("policy that denies all projects and allows one project returns no projects", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "deny",
							"projects":  []string{constants.AllProjectsID},
						},
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{proj1},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{}, actual)
			})

			t.Run("policy that denies all projects returns no projects when request contains no projects", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "deny",
							"projects":  []string{constants.AllProjectsID},
						},
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{proj1},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{}, actual)
			})

			t.Run("policy that allows all projects and denies one project returns list of all projects minus denied project", func(t *testing.T) {
				proj3, proj4 := "proj-3", "proj-4"
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{constants.AllProjectsID},
						},
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "deny",
							"projects":  []string{proj1},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				// in the server, we fetch the list of all projects when the projects filter is empty
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2, proj3, proj4}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj2, proj3, proj4}, actual)
			})

			t.Run("policy that allows all projects returns all requested projects", func(t *testing.T) {
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{constants.AllProjectsID},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1, proj2}, actual)
			})

			t.Run("policy that allows all projects returns all when requested projects are empty", func(t *testing.T) {
				proj3, proj4 := "proj-3", "proj-4"
				pol := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{constants.AllProjectsID},
						},
					},
				}
				setPoliciesV2(t, e, pol)
				// in the server, we fetch the list of all projects when the projects filter is empty
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2, proj3, proj4}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1, proj2, proj3, proj4}, actual)
			})
		})
	}
}

func TestV2FilterAuthorizedPairs(t *testing.T) {
	ctx, engines := setup(t)
	sub, act0, res0, act1, res1 := "user:local:someid", "iam:users:create",
		"nodes:someid", "compliance:profiles:delete", "compliance:profiles"
	pair0 := engine.Pair{Resource: engine.Resource(res0), Action: engine.Action(act0)}
	pair1 := engine.Pair{Resource: engine.Resource(res1), Action: engine.Action(act1)}
	args := func() (context.Context, engine.Subjects, []engine.Pair) {
		return ctx, engine.Subject(sub), []engine.Pair{pair0, pair1}
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {

			t.Run("no matching policies", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act0},
							"resources": []string{"nodes:nomatch"},
							"effect":    "allow",
						},
					},
				}
				policy1 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{"iam:users:nomatch"},
							"resources": []string{res0},
							"effect":    "allow",
						},
					},
				}
				setPoliciesV2(t, e, policy0, policy1)

				filtered, err := e.V2FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, []engine.Pair{}, filtered)
			})

			t.Run("one policy with wildcard subject matching one of the pairs (inline actions), decision 'allow'", func(t *testing.T) {
				policy := map[string]interface{}{
					"members": engine.Subject("*"),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act0},
							"resources": []string{res0},
							"effect":    "allow",
						},
					},
				}
				setPoliciesV2(t, e, policy)

				filtered, err := e.V2FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, []engine.Pair{pair0}, filtered)
			})

			t.Run("one policy with wildcard subject matching one of the pairs (via role), decision 'allow'", func(t *testing.T) {
				policy := map[string]interface{}{
					"members": engine.Subject("*"),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"role":      "handyman",
							"resources": []string{res0},
							"effect":    "allow",
						},
					},
				}
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act0},
				}
				setPoliciesV2(t, e, policy, role)

				filtered, err := e.V2FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.Equal(t, []engine.Pair{pair0}, filtered)
			})

			t.Run("two policies matching each one of the pairs, decision 'allow'", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act0},
							"resources": []string{res0},
							"effect":    "allow",
						},
					},
				}
				policy1 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act1},
							"resources": []string{res1},
							"effect":    "allow",
						},
					},
				}
				setPoliciesV2(t, e, policy0, policy1)

				filtered, err := e.V2FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, []engine.Pair{pair0, pair1}, filtered)
			})
		})
	}
}

func TestV2FilterAuthorizedProjects(t *testing.T) {
	ctx, engines := setup(t)
	sub := "user:local:someid"
	act0, res0 := "iam:users:create", "nodes:someid"
	act1, res1 := "compliance:profiles:delete", "compliance:profiles"
	act2, res2 := "iam:users:delete", "nodes:someid"
	proj0, proj1, proj2, proj3, proj4 := "proj-0", "proj-1", "proj-2", "proj-3", "proj-4"
	pair0 := engine.Pair{Resource: engine.Resource(res0), Action: engine.Action(act0)}
	pair1 := engine.Pair{Resource: engine.Resource(res1), Action: engine.Action(act1)}
	pair2 := engine.Pair{Resource: engine.Resource(res2), Action: engine.Action(act2)}
	args := func() (context.Context, engine.Subjects, []engine.Pair) {
		return ctx, engine.Subject(sub), []engine.Pair{pair0, pair1, pair2}
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {

			t.Run("no matching policies", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act0},
							"resources": []string{"nodes:nomatch"},
							"effect":    "allow",
							"projects":  engine.ProjectList(proj0, proj2),
						},
					},
				}
				policy1 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{"iam:users:nomatch"},
							"resources": []string{res0},
							"effect":    "allow",
							"projects":  engine.ProjectList(proj0, proj1),
						},
					},
				}
				setPoliciesV2(t, e, policy0, policy1)
				expectedProjects := []string{}

				filtered, err := e.V2FilterAuthorizedProjects(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, expectedProjects, filtered)
			})

			t.Run("overlapping projects reduced to a unique set", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act0},
							"resources": []string{res0},
							"effect":    "allow",
							"projects":  engine.ProjectList(proj0, proj1),
						},
					},
				}
				policy1 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act1},
							"resources": []string{res1},
							"effect":    "allow",
							"projects":  engine.ProjectList(proj0, proj2),
						},
					},
				}
				setPoliciesV2(t, e, policy0, policy1)
				expectedProjects := []string{proj0, proj1, proj2}

				filtered, err := e.V2FilterAuthorizedProjects(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, expectedProjects, filtered)
			})

			t.Run("projects on denied policies do not reduce nor add to the result set", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act0},
							"resources": []string{res0},
							"effect":    "allow",
							"projects":  engine.ProjectList(proj0, proj1),
						},
					},
				}
				policy1 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{act1},
							"resources": []string{res1},
							"effect":    "allow",
							"projects":  engine.ProjectList(proj0, proj2),
						},
						"statement-id-2": map[string]interface{}{
							"actions":   []string{act2},
							"resources": []string{res2},
							"effect":    "deny",
							"projects":  engine.ProjectList(proj2),
						},
						"statement-id-3": map[string]interface{}{
							"actions":   []string{act2},
							"resources": []string{res2},
							"effect":    "deny",
							"projects":  engine.ProjectList(proj3, proj4),
						},
					},
				}
				setPoliciesV2(t, e, policy0, policy1)
				expectedProjects := []string{proj0, proj1, proj2}

				filtered, err := e.V2FilterAuthorizedProjects(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, expectedProjects, filtered)
			})
		})
	}
}

func setPoliciesV2(t testing.TB, e engine.Engine, policiesAndRoles ...interface{}) {
	t.Helper()
	ctx := context.Background()

	// differentiate what we've been given
	var policies, roles []map[string]interface{}
	for i := range policiesAndRoles {
		m, ok := policiesAndRoles[i].(map[string]interface{})
		require.True(t, ok)
		if m["actions"] != nil {
			// it's a role!
			roles = append(roles, m)
		} else {
			policies = append(policies, m)
		}
	}

	// Simulate GUID by using i (just has to be unique)
	policyMap := make(map[string]interface{})
	for i, pol := range policies {
		policyMap[strconv.Itoa(i)] = pol
	}
	roleMap := make(map[string]interface{})
	for _, role := range roles {
		roleMap[role["id"].(string)] = role
	}
	err := e.V2SetPolicies(ctx, policyMap, roleMap, make(map[string][]interface{}))
	require.NoError(t, err, "set policies(v2)")
}
