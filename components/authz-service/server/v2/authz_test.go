package v2_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
)

/************ ************ ************ ************ ************ ************
 * NOTE: These tests are mini-integration tests at the service API level,    *
 * confirming that the wiring to our OPA engine infrastructure is correct.   *
 *                                                                           *
 * For lower level OPA tests, see opa_v2_test.go.                            *
 * For the next-lower down integration tests, see conformance_v2_test.go,    *
 * which actually use the OPA engine.                                        *
 ************ ************ ************ ************ ************ ************/

func TestIsAuthorized(t *testing.T) {
	eng := responderEngine{}
	ctx, ts := setupV2AuthTests(t, &eng)

	t.Run("when the engine response is true, returns Authorized: true", func(t *testing.T) {
		eng.authorized = true
		resp, err := ts.authz.IsAuthorized(ctx, &api_v2.IsAuthorizedReq{
			Subjects: []string{"user:local:admin"},
			Resource: "some:thing",
			Action:   "do:that:thing",
		})
		require.NoError(t, err)
		assert.True(t, resp.Authorized)
	})

	t.Run("when the engine response is false, returns Authorized: false", func(t *testing.T) {
		eng.authorized = false
		resp, err := ts.authz.IsAuthorized(ctx, &api_v2.IsAuthorizedReq{
			Subjects: []string{"user:local:not-admin"},
			Resource: "some:thing",
			Action:   "do:that:thing",
		})
		require.NoError(t, err)
		assert.False(t, resp.Authorized)
	})
}

func TestV2p1ProjectsAuthorized(t *testing.T) {
	eng := responderEngine{}
	ctx, ts := setupV2p1AuthTests(t, &eng)

	t.Run("authorized", func(t *testing.T) {
		cases := map[string]struct {
			allowedProjects []string
			result          []string
		}{
			"when engine response is SOME projects, returns the engine response verbatim": {
				[]string{"p1", "p2"},
				[]string{"p1", "p2"},
			},
			"when engine response is NO projects, returns no projects": {
				[]string{}, // no projects
				nil,
			},
			"when engine response is list of all projects + unassigned, returns external ALL projects": {
				[]string{"p1", "p2", "p3", "(unassigned)"},
				[]string{constants.AllProjectsExternalID},
			},
		}
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				eng.projects = tc.allowedProjects
				addProjectToStore(t, ts.projectCache, "p1", "Numero 1", storage.Custom)
				addProjectToStore(t, ts.projectCache, "p2", "Numero 2", storage.Custom)
				addProjectToStore(t, ts.projectCache, "p3", "Numero 3", storage.Custom)
				resp, err := ts.authz.ProjectsAuthorized(ctx, &api_v2.ProjectsAuthorizedReq{
					Subjects:       []string{"user:local:admin"},
					Resource:       "some:thing",
					Action:         "do:that:thing",
					ProjectsFilter: []string{},
				})
				require.NoError(t, err)
				assert.Equal(t, tc.result, resp.Projects)
			})
		}
	})
}

func TestV2ProjectsAuthorized(t *testing.T) {
	eng := responderEngine{}
	ctx, ts := setupV2AuthTests(t, &eng)

	t.Run("authorized", func(t *testing.T) {
		cases := map[string]struct {
			allowed bool
			result  []string
		}{
			"when engine response is true, returns external ALL projects": {
				true,
				[]string{constants.AllProjectsExternalID},
			},
			"when engine response is false, returns NO projects": {
				false,
				nil,
			},
		}
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				eng.authorized = tc.allowed
				resp, err := ts.authz.ProjectsAuthorized(ctx, &api_v2.ProjectsAuthorizedReq{
					Subjects:       []string{"user:local:admin"},
					Resource:       "some:thing",
					Action:         "do:that:thing",
					ProjectsFilter: []string{},
				})
				require.NoError(t, err)
				assert.Equal(t, tc.result, resp.Projects)
			})
		}
	})
}

func TestFilterAuthorizedPairs(t *testing.T) {
	eng := responderEngine{
		pairs: []engine.Pair{
			{Action: "iam:users:create", Resource: "iam:users"},
		}}
	ctx, ts := setupV2AuthTests(t, &eng)

	t.Run("returns engine response", func(t *testing.T) {
		resp, err := ts.authz.FilterAuthorizedPairs(ctx, &api_v2.FilterAuthorizedPairsReq{
			Subjects: []string{"user:local:admin"},
			Pairs:    []*api_v2.Pair{},
		})
		require.NoError(t, err)
		assert.Equal(t, []*api_v2.Pair{{Resource: "iam:users", Action: "iam:users:create"}}, resp.Pairs)
	})
}

func TestFilterAuthorizedProjects(t *testing.T) {
	var expProjects []string
	var eng responderEngine

	t.Run("returns engine response", func(t *testing.T) {
		expProjects = []string{"project-1", "project-2", "project-3"}
		eng = responderEngine{projects: expProjects}
		ctx, ts := setupV2AuthTests(t, &eng)

		resp, err := ts.authz.FilterAuthorizedProjects(ctx,
			&api_v2.FilterAuthorizedProjectsReq{
				Subjects: []string{"user:local:admin"},
			})
		require.NoError(t, err)
		assert.Equal(t, expProjects, resp.Projects)
	})

	t.Run("if engine returns all projects, returns list of all projects and unassigned", func(t *testing.T) {
		expProjects = []string{constants.AllProjectsID}
		eng = responderEngine{projects: expProjects}
		ctx, ts := setupV2AuthTests(t, &eng)
		addProjectToStore(t, ts.projectCache, "project-1", "Numero 1", storage.Custom)
		addProjectToStore(t, ts.projectCache, "project-2", "Numero 2", storage.Custom)
		allProjects := []string{"project-1", "project-2", "(unassigned)"}

		resp, err := ts.authz.FilterAuthorizedProjects(ctx,
			&api_v2.FilterAuthorizedProjectsReq{
				Subjects: []string{"user:local:admin"},
			})
		require.NoError(t, err)
		assert.ElementsMatch(t, allProjects, resp.Projects)
	})
}

func TestVersionSwitch(t *testing.T) {
	eng := responderEngine{}
	ctx, ts := setupV2AuthTests(t, &eng)

	t.Run("ProjectsAuthorized reacts to version switching", func(t *testing.T) {
		// setupV2AuthTests sets the version to v2.0
		// filtering should be ignored
		requestedProjects := []string{"p1", "p2", "p3"}
		v2ExpectedProjects := []string{constants.AllProjectsExternalID}
		v2p1ExpectedProjects := []string{"p1", "p2"}
		eng.projects = v2p1ExpectedProjects
		eng.authorized = true

		resp1, err := ts.authz.ProjectsAuthorized(ctx, &api_v2.ProjectsAuthorizedReq{
			Subjects:       []string{"user:local:admin"},
			Resource:       "some:thing",
			Action:         "do:that:thing",
			ProjectsFilter: requestedProjects,
		})
		require.NoError(t, err)
		assert.Equal(t, v2ExpectedProjects, resp1.Projects)

		// we upgrade to v2.1
		_, err = ts.policy.MigrateToV2(ctx, &api_v2.MigrateToV2Req{Flag: api_v2.Flag_VERSION_2_1})
		require.NoError(t, err)

		resp2, err := ts.authz.ProjectsAuthorized(ctx, &api_v2.ProjectsAuthorizedReq{
			Subjects:       []string{"user:local:admin"},
			Resource:       "some:thing",
			Action:         "do:that:thing",
			ProjectsFilter: requestedProjects,
		})
		require.NoError(t, err)
		assert.Equal(t, v2p1ExpectedProjects, resp2.Projects)
	})
}

func setupV2AuthTests(t *testing.T, eng *responderEngine) (context.Context, testSetup) {
	ctx := context.Background()
	vChan := make(chan api_v2.Version, 1)
	emptyV1List := v1Lister{}
	ts := setupV2(t, eng, nil, &emptyV1List, vChan)
	_, err := ts.policy.MigrateToV2(ctx, &api_v2.MigrateToV2Req{Flag: api_v2.Flag_VERSION_2_0})
	require.NoError(t, err)
	return ctx, ts
}

func setupV2p1AuthTests(t *testing.T, eng *responderEngine) (context.Context, testSetup) {
	ctx := context.Background()
	vChan := make(chan api_v2.Version, 1)
	emptyV1List := v1Lister{}
	ts := setupV2(t, eng, nil, &emptyV1List, vChan)
	_, err := ts.policy.MigrateToV2(ctx, &api_v2.MigrateToV2Req{Flag: api_v2.Flag_VERSION_2_1})
	require.NoError(t, err)
	return ctx, ts
}

type responderEngine struct {
	authorized bool
	pairs      []engine.Pair
	projects   []string
}

func (e *responderEngine) V2IsAuthorized(
	context.Context,
	engine.Subjects,
	engine.Action,
	engine.Resource) (bool, error) {
	return e.authorized, nil
}

func (e *responderEngine) V2ProjectsAuthorized(
	context.Context,
	engine.Subjects,
	engine.Action,
	engine.Resource,
	engine.Projects) ([]string, error) {
	return e.projects, nil
}

func (e *responderEngine) V2FilterAuthorizedPairs(
	context.Context,
	engine.Subjects,
	[]engine.Pair) ([]engine.Pair, error) {
	return e.pairs, nil
}

func (e *responderEngine) V2FilterAuthorizedProjects(
	context.Context, engine.Subjects) ([]string, error) {
	return e.projects, nil
}
