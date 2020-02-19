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

func TestFilterAuthorizedPairs(t *testing.T) {
	eng := responderEngine{
		pairs: []engine.Pair{
			{Action: "iam:users:create", Resource: "iam:users"},
		}}
	ctx, ts := setupV2p1AuthTests(t, &eng)

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
		ctx, ts := setupV2p1AuthTests(t, &eng)

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
		ctx, ts := setupV2p1AuthTests(t, &eng)
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

func setupV2p1AuthTests(t *testing.T, eng *responderEngine) (context.Context, testSetup) {
	ctx := context.Background()
	emptyV1List := v1Lister{}
	ts := setupV2(t, eng, nil, &emptyV1List)
	return ctx, ts
}

type responderEngine struct {
	authorized bool
	pairs      []engine.Pair
	projects   []string
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
