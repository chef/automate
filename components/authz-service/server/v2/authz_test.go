package v2_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
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
	ctx, ts := setupAuthTests(t, &eng)

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

func TestProjectsAuthorized(t *testing.T) {
	eng := responderEngine{}
	ctx, ts := setupAuthTests(t, &eng)

	t.Run("authorized", func(t *testing.T) {
		cases := map[string]struct {
			requestedProjects []string
			allowedProjects   []string
			result            []string
		}{
			"request includes SOME projects and engine response is SOME projects, returns the engine response verbatim": {
				[]string{"p1", "p2", "p3"},
				[]string{"p1", "p2"},
				[]string{"p1", "p2"},
			},
			"request includes ALL projects and engine response is SOME projects, returns the engine response verbatim": {
				[]string{}, // all projects
				[]string{"p1", "p2"},
				[]string{"p1", "p2"},
			},
			"request includes SOME projects and engine response is NO projects, returns the engine response verbatim": {
				[]string{"p1", "p2", "p3"},
				[]string{},    // no projects
				[]string(nil), // well, almost verbatim
			},
			"when the request includes ALL projects and the engine response is NO projects, returns the engine response verbatim": {
				[]string{},    // all projects
				[]string{},    // no projects
				[]string(nil), // well, almost verbatim
			},
			"when the request includes SOME projects and the engine response is ALL projects, returns the requested projects": {
				[]string{"p1", "p2", "p3"},
				[]string{constants.AllProjectsID, "p3"},
				[]string{"p1", "p2", "p3"},
			},
			"when the request includes ALL projects and the engine response is ALL projects, returns external ALL projects": {
				[]string{},
				[]string{constants.AllProjectsID},
				[]string{constants.AllProjectsExternalID},
			},
		}
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				eng.projects = tc.allowedProjects
				resp, err := ts.authz.ProjectsAuthorized(ctx, &api_v2.ProjectsAuthorizedReq{
					Subjects:       []string{"user:local:admin"},
					Resource:       "some:thing",
					Action:         "do:that:thing",
					ProjectsFilter: tc.requestedProjects,
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
	ctx, ts := setupAuthTests(t, &eng)

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
	var expProjects = []string{"project-1", "project-2", "project-3"}
	eng := responderEngine{projects: expProjects}
	ctx, ts := setupAuthTests(t, &eng)

	t.Run("returns engine response", func(t *testing.T) {
		resp, err := ts.authz.FilterAuthorizedProjects(ctx,
			&api_v2.FilterAuthorizedPairsReq{
				Subjects: []string{"user:local:admin"},
				Pairs:    []*api_v2.Pair{},
			})
		require.NoError(t, err)
		assert.Equal(t, expProjects, resp.Projects)
	})
}

func setupAuthTests(t *testing.T, eng *responderEngine) (context.Context, testSetup) {
	ctx := context.Background()
	vChan := make(chan api_v2.Version, 1)
	emptyV1List := v1Lister{}
	ts := setupV2(t, eng, nil, &emptyV1List, vChan)
	_, err := ts.policy.MigrateToV2(ctx, &api_v2.MigrateToV2Req{})
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
	context.Context,
	engine.Subjects,
	[]engine.Pair) ([]string, error) {
	return e.projects, nil
}
