package conformance_test

import (
	"context"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
	v2 "github.com/chef/automate/components/authz-service/storage/v2"
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
	ctx, engines := setupV2(t)
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

func TestV2p1ProjectsAuthorized(t *testing.T) {
	ctx, engines := setupV2p1(t)
	sub, act, res := "user:local:admin", "iam:users:create", "iam:users"
	proj1, proj2, proj3, proj4, unassigned := "proj-1", "proj-2", "proj-3", "proj-4", constants.UnassignedProjectID
	allProjects := []string{proj1, proj2, proj3, proj4, unassigned}

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
				setPoliciesV2p1(t, e, nil, pol)
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
				setPoliciesV2p1(t, e, nil, pol, role)
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
				setPoliciesV2p1(t, e, nil, pol, role)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.Equal(t, []string{}, actual)
			})

			t.Run("policy with single project returns that project when all projects requested", func(t *testing.T) {
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
				setPoliciesV2p1(t, e, nil, pol, role)
				actual, err := e.V2ProjectsAuthorized(args(allProjects))
				require.NoError(t, err)
				assert.Equal(t, []string{proj1}, actual)
			})

			// including this case to demonstrate the All Projects ID cannot be passed in the request
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
				setPoliciesV2p1(t, e, nil, pol, role)
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
				setPoliciesV2p1(t, e, nil, pol)
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
				setPoliciesV2p1(t, e, nil, pol)
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
				setPoliciesV2p1(t, e, nil, pol)
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
				setPoliciesV2p1(t, e, nil, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{}, actual)
			})

			t.Run("policy that denies all projects returns no projects when all projects requested", func(t *testing.T) {
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
				setPoliciesV2p1(t, e, nil, pol)
				actual, err := e.V2ProjectsAuthorized(args(allProjects))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{}, actual)
			})

			t.Run("policy that allows all projects and denies one project returns list of all projects minus denied project", func(t *testing.T) {
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
				setPoliciesV2p1(t, e, nil, pol)
				// in the server, we fetch the list of all projects when the projects filter is empty
				actual, err := e.V2ProjectsAuthorized(args(allProjects))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj2, proj3, proj4, unassigned}, actual)
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
				setPoliciesV2p1(t, e, nil, pol)
				actual, err := e.V2ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1, proj2}, actual)
			})

			t.Run("policy that allows all projects returns all when all projects requested", func(t *testing.T) {
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
				setPoliciesV2p1(t, e, nil, pol)
				// in the server, we fetch the list of all projects when the projects filter is empty
				actual, err := e.V2ProjectsAuthorized(args(allProjects))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1, proj2, proj3, proj4, unassigned}, actual)
			})
		})
	}
}

func TestV2FilterAuthorizedPairs(t *testing.T) {
	ctx, engines := setupV2(t)
	sub, act0, res0, act1, res1 := "user:local:someid", "iam:users:create",
		"nodes:someid", "compliance:profiles:delete", "compliance:profiles"
	pair0 := engine.Pair{Resource: engine.Resource(res0), Action: engine.Action(act0)}
	pair1 := engine.Pair{Resource: engine.Resource(res1), Action: engine.Action(act1)}
	args := func() (context.Context, engine.Subjects, []engine.Pair, bool) {
		return ctx, engine.Subject(sub), []engine.Pair{pair0, pair1}, false
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
	ctx, engines := setupV2p1(t)
	sub := "user:local:someid"
	act0, res0 := "iam:users:create", "nodes:someid"
	act1, res1 := "compliance:profiles:delete", "compliance:profiles"
	act2, res2 := "iam:users:delete", "nodes:someid"
	proj0, proj1, proj2, proj3, proj4 := "proj-0", "proj-1", "proj-2", "proj-3", "proj-4"
	args := func() (context.Context, engine.Subjects) {
		return ctx, engine.Subject(sub)
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {

			t.Run("no matching policies", func(t *testing.T) {
				policy0 := map[string]interface{}{
					"members": engine.Subject("team:local:foo"),
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
					"members": engine.Subject("user:local:bar"),
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
				setPoliciesV2p1(t, e, nil, policy0, policy1)
				expectedProjects := []string{proj0, proj1, proj2}

				filtered, err := e.V2FilterAuthorizedProjects(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, expectedProjects, filtered)
			})

			t.Run("does not return projects permitted by system policies when projects do not overlap", func(t *testing.T) {
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
					"type":    "system",
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{"*"},
							"resources": []string{"*"},
							"effect":    "allow",
							"projects":  engine.ProjectList(constants.AllProjectsExternalID),
						},
					},
				}
				setPoliciesV2p1(t, e, nil, policy0, policy1)
				expectedProjects := []string{proj0, proj1}

				filtered, err := e.V2FilterAuthorizedProjects(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, expectedProjects, filtered)
			})

			t.Run("does return a project permitted by system policies when that project overlaps with custom policy", func(t *testing.T) {
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
					"type":    "system",
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-1": map[string]interface{}{
							"actions":   []string{"*"},
							"resources": []string{"*"},
							"effect":    "allow",
							"projects":  engine.ProjectList(constants.AllProjectsExternalID, proj0, proj3),
						},
					},
				}
				setPoliciesV2p1(t, e, nil, policy0, policy1)
				expectedProjects := []string{proj0, proj1}

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
				setPoliciesV2p1(t, e, nil, policy0, policy1)
				expectedProjects := []string{proj0, proj1, proj2}

				filtered, err := e.V2FilterAuthorizedProjects(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, expectedProjects, filtered)
			})
		})
	}
}

func TestListProjectMappings(t *testing.T) {
	t.Run("on v2.0", func(t *testing.T) {
		ctx, engines := setupV2(t)

		for desc, e := range engines {
			t.Run(desc, func(t *testing.T) {
				t.Run("returns empty map", func(t *testing.T) {
					setPoliciesV2(t, e, map[string]interface{}{})

					resp, err := e.ListProjectMappings(ctx)
					require.NoError(t, err)
					assert.Equal(t, map[string][]v2.Rule{}, resp)
				})
			})
		}
	})

	t.Run("on v2.1", func(t *testing.T) {
		ctx, engines := setupV2p1(t)

		for desc, e := range engines {
			t.Run(desc, func(t *testing.T) {
				t.Run("returns complete map of rules", func(t *testing.T) {
					proj1, proj2 := "project1", "project2"

					type1 := v2.Node
					conditions1 := []v2.Condition{
						{
							Type:      type1,
							Attribute: v2.Organization,
							Operator:  v2.MemberOf,
							Value:     []string{"opscode", "chef"},
						},
					}

					rule1, err := v2.NewRule("rule-1", proj1, "rule #1", type1, conditions1)
					require.NoError(t, err)

					type2 := v2.Event
					conditions2 := []v2.Condition{
						{
							Type:      type2,
							Attribute: v2.Organization,
							Operator:  v2.MemberOf,
							Value:     []string{"opscode", "chef"},
						},
					}

					rule2, err := v2.NewRule("rule-2", proj2, "rule #2", type2, conditions2)
					require.NoError(t, err)

					expectedMap := map[string][]v2.Rule{
						proj1: []v2.Rule{rule1},
						proj2: []v2.Rule{rule2},
					}

					setPoliciesV2p1(t, e, expectedMap, map[string]interface{}{})

					resp, err := e.ListProjectMappings(ctx)
					require.NoError(t, err)
					assert.Equal(t, expectedMap, resp)
				})
			})
		}
	})
}

func setPoliciesV2(t testing.TB, e engine.Engine, policiesAndRoles ...interface{}) {
	setPoliciesV2pX(t, false, e, nil, policiesAndRoles...)
}

func setPoliciesV2p1(t testing.TB, e engine.Engine, ruleMap map[string][]v2.Rule, policiesAndRoles ...interface{}) {
	setPoliciesV2pX(t, true, e, ruleMap, policiesAndRoles...)
}

func setPoliciesV2pX(t testing.TB, twoPointOne bool, e engine.Engine, ruleMap map[string][]v2.Rule, policiesAndRoles ...interface{}) {
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
	if twoPointOne {
		require.NoError(t, e.V2p1SetPolicies(ctx, policyMap, roleMap), "V2p1SetPolicies() [v2.1]")
		require.NoError(t, e.SetRules(ctx, ruleMap), "SetRules() [v2.1]")
	} else {
		require.NoError(t, e.V2SetPolicies(ctx, policyMap, roleMap), "V1SetPolicies() [v2]")
	}
}
