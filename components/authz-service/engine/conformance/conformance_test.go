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

	constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/engine/opa"
	"github.com/chef/automate/components/authz-service/server"
	"github.com/chef/automate/lib/logger"
)

/************ ************ ************ ************ ************ ************
 * NOTE: These tests are mini-integration tests for the OPA engine,          *
 * confirming that our engine infrastructure on top of OPA is doing its job. *
 *                                                                           *
 * For lower level OPA tests, see opa_test.go.                               *
 * For the next-higher level up integration tests, see authz_test.go,        *
 * which mock the engine results.                                            *
 ************ ************ ************ ************ ************ ************/

func TestProjectsAuthorized(t *testing.T) {
	ctx, engines := setup(t)
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
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.Equal(t, []string{}, actual)
			})
			t.Run("test system policies", func(t *testing.T) {
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.Equal(t, []string{}, actual)

				isAuthorized := func(subject, action, resource string) func(*testing.T) {
					return func(t *testing.T) {
						resp, err := e.ProjectsAuthorized(ctx, engine.Subject(subject), engine.Action(action), engine.Resource(resource), engine.Projects(allProjects))
						require.NoError(t, err)
						assert.ElementsMatch(t, allProjects, resp)
					}
				}

				// put system policies in engine
				genericSysPols := make([]interface{}, len(server.SystemPolicies()))
				for i, sysPol := range server.SystemPolicies() {
					genericSysMembers := make([]string, len(sysPol.Members))
					for _, member := range sysPol.Members {
						genericSysMembers = append(genericSysMembers, member.Name)
					}
					genericSysStatements := make(map[string]interface{})
					for j, statement := range sysPol.Statements {
						genericSysStatements[strconv.Itoa(i+j)] = map[string]interface{}{
							"actions":   statement.Actions,
							"resources": statement.Resources,
							"effect":    statement.Effect.String(),
							"projects":  statement.Projects,
						}
					}

					genericSysPols[i] = map[string]interface{}{
						"type":       "system",
						"members":    engine.Subject(genericSysMembers...),
						"statements": genericSysStatements,
					}
				}

				setPolicies(t, e, genericSysPols...)

				cases := map[string]func(t *testing.T){
					"service version":                       isAuthorized("user:ldap:alice", "system:serviceVersion:get", "system:service:version"),
					"introspect all":                        isAuthorized("user:ldap:alice", "iam:introspect:getAll", "iam:introspect"),
					"introspect some":                       isAuthorized("user:ldap:alice", "iam:introspect:getSome", "iam:introspect"),
					"introspect get":                        isAuthorized("user:ldap:alice", "iam:introspect:get", "iam:introspect"),
					"get user record":                       isAuthorized("user:local:alice", "iam:users:get", "iam:users:alice"),
					"list user record":                      isAuthorized("user:local:alice", "iam:users:list", "iam:users:alice"),
					"d-s can do allthethings":               isAuthorized("tls:service:deployment-service:cert-id", "iam:users:delete", "iam:users:alice"),
					"ingest run as provider oc-erchef":      isAuthorized("tls:service:automate-cs-oc-erchef:cert", "ingest:nodes:create", "ingest:nodes:nodeUUID:runs"),
					"ingest action as provider oc-erchef":   isAuthorized("tls:service:automate-cs-oc-erchef:cert", "ingest:actions:create", "ingest:actions"),
					"ingest delete as provider oc-erchef":   isAuthorized("tls:service:automate-cs-oc-erchef:cert", "ingest:nodes:delete", "ingest:nodes"),
					"ingest liveness as provider oc-erchef": isAuthorized("tls:service:automate-cs-oc-erchef:cert", "ingest:nodes:create", "ingest:nodes:nodeUUID:liveness"),
					"ingest run as provider cs-nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "ingest:nodes:create", "ingest:nodes:nodeUUID:runs"),
					"ingest action as provider nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "ingest:actions:create", "ingest:actions"),
					"ingest delete as provider nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "ingest:nodes:delete", "ingest:nodes"),
					"ingest liveness as provider nginx":     isAuthorized("tls:service:automate-cs-nginx:cert", "ingest:nodes:create", "ingest:nodes:nodeUUID:liveness"),
				}

				for desc, test := range cases {
					t.Run(desc, test)
				}
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
				setPolicies(t, e, pol)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol, role)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1, proj2}, actual)
			})

			t.Run("policy with an allowed project that overlaps will return that project", func(t *testing.T) {
				pol1 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"role":      "handyman",
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{proj1},
						},
					},
				}
				pol2 := map[string]interface{}{
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
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act},
				}
				setPolicies(t, e, pol1, pol2, role)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1}, actual)
			})

			t.Run("policies with some of the requested projects that contain duplicates returns matched projects without duplicates", func(t *testing.T) {
				pol1 := map[string]interface{}{
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
				pol2 := map[string]interface{}{
					"members": engine.Subject(sub),
					"statements": map[string]interface{}{
						"statement-id-0": map[string]interface{}{
							"actions":   []string{act},
							"resources": []string{res},
							"effect":    "allow",
							"projects":  []string{"other-project", proj2},
						},
					},
				}
				role := map[string]interface{}{
					"id":      "handyman",
					"actions": []string{act},
				}
				setPolicies(t, e, pol1, pol2, role)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol, role)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol, role)
				actual, err := e.ProjectsAuthorized(args(allProjects))
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
				setPolicies(t, e, pol, role)
				actual, err := e.ProjectsAuthorized(args([]string{"*"}))
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
				setPolicies(t, e, pol)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol)
				actual, err := e.ProjectsAuthorized(args(allProjects))
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
				setPolicies(t, e, pol)
				// in the server, we fetch the list of all projects when the projects filter is empty
				actual, err := e.ProjectsAuthorized(args(allProjects))
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
				setPolicies(t, e, pol)
				actual, err := e.ProjectsAuthorized(args([]string{proj1, proj2}))
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
				setPolicies(t, e, pol)
				// in the server, we fetch the list of all projects when the projects filter is empty
				actual, err := e.ProjectsAuthorized(args(allProjects))
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{proj1, proj2, proj3, proj4, unassigned}, actual)
			})
		})
	}
}

func TestFilterAuthorizedPairs(t *testing.T) {
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
				setPolicies(t, e, policy0, policy1)

				filtered, err := e.FilterAuthorizedPairs(args())
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
				setPolicies(t, e, policy)

				filtered, err := e.FilterAuthorizedPairs(args())
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
				setPolicies(t, e, policy, role)

				filtered, err := e.FilterAuthorizedPairs(args())
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
				setPolicies(t, e, policy0, policy1)

				filtered, err := e.FilterAuthorizedPairs(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, []engine.Pair{pair0, pair1}, filtered)
			})
		})
	}
}

func TestFilterAuthorizedProjects(t *testing.T) {
	ctx, engines := setup(t)
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
				setPolicies(t, e, policy0, policy1)
				expectedProjects := []string{}

				filtered, err := e.FilterAuthorizedProjects(args())
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
				setPolicies(t, e, policy0, policy1)
				expectedProjects := []string{proj0, proj1, proj2}

				filtered, err := e.FilterAuthorizedProjects(args())
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
				setPolicies(t, e, policy0, policy1)
				expectedProjects := []string{proj0, proj1}

				filtered, err := e.FilterAuthorizedProjects(args())
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
				setPolicies(t, e, policy0, policy1)
				expectedProjects := []string{proj0, proj1}

				filtered, err := e.FilterAuthorizedProjects(args())
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
				setPolicies(t, e, policy0, policy1)
				expectedProjects := []string{proj0, proj1, proj2}

				filtered, err := e.FilterAuthorizedProjects(args())
				require.NoError(t, err)
				assert.ElementsMatch(t, expectedProjects, filtered)
			})
		})
	}
}

func TestHierarchicalResourcePolicies(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res, proj := "user:local:someid", "compliance:scans:read", "compliance:scans:123", "proj"
	pair := engine.Pair{Resource: engine.Resource(res), Action: engine.Action(act)}
	args := func() (context.Context, engine.Subjects, []engine.Pair) {
		return ctx, engine.Subject(sub), []engine.Pair{pair}
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
							"members": engine.Subject(sub),
							"statements": map[string]interface{}{
								"statement-id-1": map[string]interface{}{
									"actions":   []string{act},
									"resources": []string{hierarchicalResource},
									"effect":    "allow",
									"projects":  engine.ProjectList(proj),
								},
							},
						}
						setPolicies(t, e, policy)

						filtered, err := e.FilterAuthorizedPairs(args())
						require.NoError(t, err)
						assert.Equal(t, []engine.Pair{pair}, filtered)
					})

				t.Run(fmt.Sprintf("two policies, one with specified resource, decision 'allow'; hierarchical resource %q, decision 'deny'", hierarchicalResource),
					func(t *testing.T) {
						allowPolicy := map[string]interface{}{
							"members": engine.Subject(sub),
							"statements": map[string]interface{}{
								"statement-id-1": map[string]interface{}{
									"actions":   []string{act},
									"resources": []string{res},
									"effect":    "allow",
									"projects":  engine.ProjectList(proj),
								},
							},
						}

						denyPolicy := map[string]interface{}{
							"members": engine.Subject(sub),
							"statements": map[string]interface{}{
								"statement-id-1": map[string]interface{}{
									"actions":   []string{act},
									"resources": []string{hierarchicalResource},
									"effect":    "deny",
									"projects":  engine.ProjectList(proj),
								},
							},
						}

						setPolicies(t, e, allowPolicy, denyPolicy)

						// now, having both the matching allow-policy, and a new deny-policy
						// with a hierarchical resource, this should end in deny (no pairs)
						filtered, err := e.FilterAuthorizedPairs(args())
						require.NoError(t, err)
						assert.Equal(t, []engine.Pair{}, filtered)
					})

				t.Run(fmt.Sprintf("two policies, one with specified resource, decision 'deny'; hierarchical resource %q, decision 'allow'", hierarchicalResource),
					func(t *testing.T) {
						allowPolicy := map[string]interface{}{
							"members": engine.Subject(sub),
							"statements": map[string]interface{}{
								"statement-id-1": map[string]interface{}{
									"actions":   []string{act},
									"resources": []string{hierarchicalResource},
									"effect":    "allow",
									"projects":  engine.ProjectList(proj),
								},
							},
						}

						denyPolicy := map[string]interface{}{
							"members": engine.Subject(sub),
							"statements": map[string]interface{}{
								"statement-id-1": map[string]interface{}{
									"actions":   []string{act},
									"resources": []string{res},
									"effect":    "deny",
									"projects":  engine.ProjectList(proj),
								},
							},
						}

						setPolicies(t, e, allowPolicy, denyPolicy)

						// now, having both the matching deny-policy for a specified resource,
						// and a new allow-policy with a hierarchical resource, this should
						// still end in deny (no pairs)
						filtered, err := e.FilterAuthorizedPairs(args())
						require.NoError(t, err)
						assert.Equal(t, []engine.Pair{}, filtered)
					})
			}
		})
	}
}

func TestWildcardActionPolicies(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res, proj := "user:local:someid", "compliance:scans:read", "compliance:scans:123", "proj"
	pair := engine.Pair{Resource: engine.Resource(res), Action: engine.Action(act)}
	args := func() (context.Context, engine.Subjects, []engine.Pair) {
		return ctx, engine.Subject(sub), []engine.Pair{pair}
	}

	for desc, e := range engines {
		t.Run(desc, func(t *testing.T) {

			t.Run("one policy with wildcard action, decision 'allow'",
				func(t *testing.T) {
					policy := map[string]interface{}{
						"members": engine.Subject(sub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{"*"},
								"resources": []string{res},
								"effect":    "allow",
								"projects":  engine.ProjectList(proj),
							},
						},
					}
					setPolicies(t, e, policy)

					filtered, err := e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{pair}, filtered)
				})

			t.Run("two policies, one with specified action, decision 'allow'; one with wildcard action, decision 'deny'",
				func(t *testing.T) {
					allowPolicy := map[string]interface{}{
						"members": engine.Subject(sub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{act},
								"resources": []string{res},
								"effect":    "allow",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					// check that we get an allow, so this non-wildcard policy is matching
					setPolicies(t, e, allowPolicy)
					filtered, err := e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{pair}, filtered)

					denyPolicy := map[string]interface{}{
						"members": engine.Subject(sub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{"*"},
								"resources": []string{res},
								"effect":    "deny",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching allow-policy, and a new deny-policy
					// with a wildcard subject, this should end in deny (no pair)
					filtered, err = e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{}, filtered)
				})

			t.Run("two policies, one with specified action, decision 'deny'; one with wildcard action, decision 'allow'",
				func(t *testing.T) {
					allowPolicy := map[string]interface{}{
						"members": engine.Subject(sub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{"*"},
								"resources": []string{res},
								"effect":    "allow",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					// check that we get an allow, so this non-wildcard policy is matching
					setPolicies(t, e, allowPolicy)
					filtered, err := e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{pair}, filtered)

					denyPolicy := map[string]interface{}{
						"members": engine.Subject(sub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{act},
								"resources": []string{res},
								"effect":    "deny",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching deny-policy for a specified action,
					// and a new allow-policy with a wildcard action, this should still
					// end in deny (no pair)
					filtered, err = e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{}, filtered)
				})
		})
	}
}

func TestWildcardSubjectsPolicies(t *testing.T) {
	ctx, engines := setup(t)
	sub, act, res, proj := "user:local:someid", "compliance:scans:read", "compliance:scans:123", "proj"
	pair := engine.Pair{Resource: engine.Resource(res), Action: engine.Action(act)}
	args := func() (context.Context, engine.Subjects, []engine.Pair) {
		return ctx, engine.Subject(sub), []engine.Pair{pair}
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
						"members": engine.Subject(matchingSub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{act},
								"resources": []string{res},
								"effect":    "allow",
								"projects":  engine.ProjectList(proj),
							},
						},
					}
					setPolicies(t, e, policy)

					filtered, err := e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{pair}, filtered)
				})

				t.Run(fmt.Sprintf("two policies, one with specified subject, decision 'allow'; one with wildcard subject %q, decision 'deny'", matchingSub), func(t *testing.T) {
					allowPolicy := map[string]interface{}{
						"members": engine.Subject(sub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{act},
								"resources": []string{res},
								"effect":    "allow",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					// check that we get an allow
					setPolicies(t, e, allowPolicy)
					filtered, err := e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{pair}, filtered)

					denyPolicy := map[string]interface{}{
						"members": engine.Subject(matchingSub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{act},
								"resources": []string{res},
								"effect":    "deny",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching allow-policy, and a new deny-policy
					// with a wildcard subject, this should end in deny (no pair)
					filtered, err = e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{}, filtered)

				})

				t.Run(fmt.Sprintf("two policies, one with specified subject, decision 'deny'; one with wildcard subject %q, decision 'allow'", matchingSub), func(t *testing.T) {

					allowPolicy := map[string]interface{}{
						"members": engine.Subject(matchingSub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{act},
								"resources": []string{res},
								"effect":    "allow",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					// check that we get an allow, so this non-wildcard policy is matching
					setPolicies(t, e, allowPolicy)
					filtered, err := e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{pair}, filtered)

					denyPolicy := map[string]interface{}{
						"members": engine.Subject(sub),
						"statements": map[string]interface{}{
							"statement-id-1": map[string]interface{}{
								"actions":   []string{act},
								"resources": []string{res},
								"effect":    "deny",
								"projects":  engine.ProjectList(proj),
							},
						},
					}

					setPolicies(t, e, allowPolicy, denyPolicy)

					// now, having both the matching deny-policy for a specified subject,
					// and a new allow-policy with a wildcard subject, this should still
					// end in deny (no pair)
					filtered, err = e.FilterAuthorizedPairs(args())
					require.NoError(t, err)
					assert.Equal(t, []engine.Pair{}, filtered)
				})
			}
		})
	}
}

func setPolicies(t testing.TB, e engine.Engine, policiesAndRoles ...interface{}) {
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
	require.NoError(t, e.SetPolicies(ctx, policyMap, roleMap), "SetPolicies()")
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

	if o, ok := engines["opa"]; ok {
		o.SetPolicies(ctx, map[string]interface{}{}, map[string]interface{}{})
	}

	return ctx, engines
}
