package postgres_test

import (
	"context"
	"math/rand"
	"testing"

	"github.com/lib/pq"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/testhelpers"
	"github.com/chef/automate/lib/projectassignment"
)

func TestGetPolicy(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.GetPolicy(ctx, genSimpleID(t, prngSeed))
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"policy with projects": func(t *testing.T) {
			ctx := context.Background()
			targetProject := "p1"
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")
			insertTestProject(t, db, targetProject, "test project 1", storage.Custom)
			// also test the invalid case  where a specified project does not exist
			insertPolicyProject(t, db, polID, targetProject)

			resp, err := store.GetPolicy(ctx, polID)
			require.NoError(t, err)

			pol := storage.Policy{
				ID:         polID,
				Name:       "testpolicy",
				Members:    []storage.Member{member},
				Statements: []storage.Statement{},
				Projects:   []string{targetProject},
			}
			assert.Equal(t, &pol, resp)
		},
		"policy with no statements": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resp, err := store.GetPolicy(ctx, polID)
			require.NoError(t, err)

			pol := storage.Policy{
				ID:         polID,
				Name:       "testpolicy",
				Members:    []storage.Member{member},
				Statements: []storage.Statement{},
				Projects:   []string{},
			}
			assert.Equal(t, &pol, resp)
		},
		"policy with two statements found": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")

			s0Actions := []string{"iam:users:delete", "iam:users:create"}
			s0Resources := []string{"iam:users"}
			insertTestStatement(t, db, polID, "allow", "", s0Actions, s0Resources)

			s1Actions := []string{"compliance:profiles:download", "compliance:profiles:delete"}
			s1Resources := []string{"compliance:profiles"}
			insertTestStatement(t, db, polID, "deny", "", s1Actions, s1Resources)

			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			resp, err := store.GetPolicy(ctx, polID)
			require.NoError(t, err)
			pol := storage.Policy{
				ID:      polID,
				Name:    "testpolicy",
				Members: []storage.Member{member},
				Type:    storage.Custom,
				Statements: []storage.Statement{
					{
						Effect:    storage.Allow,
						Resources: s0Resources,
						Actions:   s0Actions,
						Projects:  []string{},
					},
					{
						Effect:    storage.Deny,
						Resources: s1Resources,
						Actions:   s1Actions,
						Projects:  []string{},
					},
				},
			}
			assertPolicy(t, &pol, resp)
		},
		"policy with two statements and three members, among other policy": func(t *testing.T) {
			ctx := context.Background()
			s0Actions := []string{"iam:users:delete", "iam:users:create"}
			s0Resources := []string{"iam:users"}
			s1Actions := []string{"compliance:profiles:download", "compliance:profiles:delete"}
			s1Resources := []string{"compliance:profiles"}

			firstPolID := insertTestPolicy(t, db, "firstpolicy")
			insertTestStatement(t, db, firstPolID, "allow", "", s0Actions, s0Resources)
			insertTestStatement(t, db, firstPolID, "deny", "", s1Actions, s1Resources)

			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestStatement(t, db, polID, "allow", "", s0Actions, s0Resources)
			insertTestStatement(t, db, polID, "deny", "", s1Actions, s1Resources)

			member0 := insertTestPolicyMember(t, db, polID, "user:local:albertine")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:charmander")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:charizard")

			resp, err := store.GetPolicy(ctx, polID)
			require.NoError(t, err)
			pol := storage.Policy{
				ID:      polID,
				Name:    "testpolicy",
				Members: []storage.Member{member0, member1, member2},
				Type:    storage.Custom,
				Statements: []storage.Statement{
					{
						Effect:    storage.Allow,
						Resources: s0Resources,
						Actions:   s0Actions,
						Projects:  []string{},
					},
					{
						Effect:    storage.Deny,
						Resources: s1Resources,
						Actions:   s1Actions,
						Projects:  []string{},
					},
				},
			}
			assertPolicy(t, &pol, resp)
		},
		"policy with two statements, both with projects, and three members, among other policy": func(t *testing.T) {
			ctx := context.Background()

			projID0, projID1 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			project0 := insertTestProject(t, db, projID0, "test project 1", storage.Custom)
			project1 := insertTestProject(t, db, projID1, "test project 2", storage.Custom)

			polID := insertTestPolicy(t, db, "testpolicy")

			s0Actions := []string{"iam:users:delete", "iam:users:create"}
			s0Resources := []string{"iam:users"}
			sID0 := insertTestStatement(t, db, polID, "allow", "", s0Actions, s0Resources)

			s1Actions := []string{"infra:nodes:delete", "infra:nodes:rerun"}
			s1Resources := []string{"infra:nodes"}
			sID1 := insertTestStatement(t, db, polID, "deny", "", s1Actions, s1Resources)

			insertStatementProject(t, db, sID0, projID0)
			insertStatementProject(t, db, sID1, projID0)
			insertStatementProject(t, db, sID1, projID1)

			member0 := insertTestPolicyMember(t, db, polID, "user:local:charmander")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:charmeleon")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:charizard")

			resp, err := store.GetPolicy(ctx, polID)
			require.NoError(t, err)
			pol := storage.Policy{
				ID:      polID,
				Name:    "testpolicy",
				Members: []storage.Member{member0, member1, member2},
				Type:    storage.Custom,
				Statements: []storage.Statement{
					{
						Effect:    storage.Allow,
						Resources: s0Resources,
						Actions:   s0Actions,
						Projects:  []string{project0.ID},
					},
					{
						Effect:    storage.Deny,
						Resources: s1Resources,
						Actions:   s1Actions,
						Projects:  []string{project0.ID, project1.ID},
					},
				},
			}
			assertPolicy(t, &pol, resp)
		},
		"when the policy's projects and the project filter intersect, return policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			resp, err := store.GetPolicy(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, polID, resp.ID)
		},
		"when the * project filter is passed, return policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			resp, err := store.GetPolicy(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, polID, resp.ID)
		},
		"when the policy has no projects and (unassigned) is in the projects filter, return policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{projID1, constants.UnassignedProjectID})
			resp, err := store.GetPolicy(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, polID, resp.ID)
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			originalName := "blasting off again"
			polID := insertTestPolicy(t, db, originalName)
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{projID2, constants.UnassignedProjectID})
			resp, err := store.GetPolicy(ctx, polID)

			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListPolicies(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assert.Nil(t, resp)
			assert.Zero(t, len(resp))
		},
		"policy with no statements": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resp, err := store.ListPolicies(ctx)

			require.NoError(t, err)
			pols := []*storage.Policy{{
				ID:         polID,
				Name:       "testpolicy",
				Members:    []storage.Member{member},
				Type:       storage.Custom,
				Statements: []storage.Statement{},
				Projects:   []string{},
			}}

			assert.Equal(t, pols, resp)
		},
		"policy with two statements found": func(t *testing.T) {
			ctx := context.Background()

			polID := insertTestPolicy(t, db, "testpolicy")

			s0Actions := []string{"iam:users:delete', 'iam:users:create"}
			s0Resources := []string{"*"}
			insertTestStatement(t, db, polID, "allow", "", s0Actions, s0Resources)

			s1Actions := []string{"compliance:profiles:download", "compliance:profiles:delete"}
			s1Resources := []string{"compliance:profiles"}
			insertTestStatement(t, db, polID, "deny", "", s1Actions, s1Resources)

			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			expectedPolicies := []*storage.Policy{{
				ID:      polID,
				Name:    "testpolicy",
				Members: []storage.Member{member},
				Type:    storage.Custom,
				Statements: []storage.Statement{
					{
						Effect:    storage.Allow,
						Resources: s0Resources,
						Actions:   s0Actions,
						Projects:  []string{},
					},
					{
						Effect:    storage.Deny,
						Resources: s1Resources,
						Actions:   s1Actions,
						Projects:  []string{},
					},
				},
			}}
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies, with one statement each": func(t *testing.T) {
			ctx := context.Background()

			polID0 := insertTestPolicy(t, db, "01testpolicy")
			s0Actions := []string{"iam:users:delete', 'iam:users:create"}
			s0Resources := []string{"*"}
			insertTestStatement(t, db, polID0, "allow", "", s0Actions, s0Resources)

			polID1 := insertTestPolicy(t, db, "02testpolicy")
			s1Actions := []string{"compliance:profiles:update"}
			s1Resources := []string{"compliance:profiles"}
			insertTestStatement(t, db, polID1, "deny", "", s1Actions, s1Resources)

			member0 := insertTestPolicyMember(t, db, polID0, "user:local:albertine0")
			member1 := insertTestPolicyMember(t, db, polID1, "user:local:albertine1")

			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			expectedPolicies := []*storage.Policy{
				{
					ID:      polID0,
					Name:    "01testpolicy",
					Members: []storage.Member{member0},
					Type:    storage.Custom,
					Statements: []storage.Statement{
						{
							Effect:    storage.Allow,
							Resources: s0Resources,
							Actions:   s0Actions,
							Projects:  []string{},
						},
					},
				},
				{
					ID:      polID1,
					Name:    "02testpolicy",
					Members: []storage.Member{member1},
					Type:    storage.Custom,
					Statements: []storage.Statement{
						{
							Effect:    storage.Deny,
							Resources: s1Resources,
							Actions:   s1Actions,
							Projects:  []string{},
						},
					},
				},
			}
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies, each with one statement that contains 1+ projects": func(t *testing.T) {
			ctx := context.Background()

			// insert projects
			projID0, projID1 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			project0 := insertTestProject(t, db, projID0, "test project 1", storage.Custom)
			project1 := insertTestProject(t, db, projID1, "test project 2", storage.Custom)

			// insert first policy with statement
			polID0 := insertTestPolicy(t, db, "01testpolicy")
			s0Actions := []string{"iam:users:delete', 'iam:users:create"}
			s0Resources := []string{"iam:users"}
			sID0 := insertTestStatement(t, db, polID0, "allow", "", s0Actions, s0Resources)

			// associate statement with project
			insertStatementProject(t, db, sID0, projID0)

			// insert second policy with statement
			polID1 := insertTestPolicy(t, db, "02testpolicy")
			s1Actions := []string{"compliance:profiles:update"}
			s1Resources := []string{"compliance:profiles"}
			sID1 := insertTestStatement(t, db, polID1, "deny", "", s1Actions, s1Resources)

			// associate statement with projects
			insertStatementProject(t, db, sID1, projID0)
			insertStatementProject(t, db, sID1, projID1)

			member0 := insertTestPolicyMember(t, db, polID0, "user:local:albertine0")
			member1 := insertTestPolicyMember(t, db, polID1, "user:local:albertine1")

			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			expectedPolicies := []*storage.Policy{
				{
					ID:      polID0,
					Name:    "01testpolicy",
					Members: []storage.Member{member0},
					Type:    storage.Custom,
					Statements: []storage.Statement{
						{
							Effect:    storage.Allow,
							Resources: s0Resources,
							Actions:   s0Actions,
							Projects:  []string{project0.ID},
						},
					},
				},
				{
					ID:      polID1,
					Name:    "02testpolicy",
					Members: []storage.Member{member1},
					Type:    storage.Custom,
					Statements: []storage.Statement{
						{
							Effect:    storage.Deny,
							Resources: s1Resources,
							Actions:   s1Actions,
							Projects:  []string{project0.ID, project1.ID},
						},
					},
				},
			}
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies, one with projects, one without": func(t *testing.T) {
			ctx := context.Background()
			name1, name2 := "testPolicy", "anotherTestPolicy"
			polID1 := insertTestPolicy(t, db, name1)
			polID2 := insertTestPolicy(t, db, name2)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID1, projID)
			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "too ordinary", storage.Custom)
			insertPolicyProject(t, db, polID1, projID2)

			expectedPolicies := []*storage.Policy{
				{
					ID:         polID1,
					Name:       name1,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{projID, projID2},
				},
				{
					ID:         polID2,
					Name:       name2,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{},
				},
			}

			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies with projects": func(t *testing.T) {
			ctx := context.Background()
			name1, name2 := "testPolicy", "anotherTestPolicy"
			polID1 := insertTestPolicy(t, db, name1)
			polID2 := insertTestPolicy(t, db, name2)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID1, projID)
			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "too ordinary", storage.Custom)
			insertPolicyProject(t, db, polID2, projID2)

			expectedPolicies := []*storage.Policy{
				{
					ID:         polID1,
					Name:       name1,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{projID},
				},
				{
					ID:         polID2,
					Name:       name2,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{projID2},
				},
			}

			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a policy list, return intersection": func(t *testing.T) {
			ctx := context.Background()
			name1, name2 := "testPolicy", "anotherTestPolicy"
			polID1 := insertTestPolicy(t, db, name1)
			polID2 := insertTestPolicy(t, db, name2)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID1, projID)
			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "too ordinary", storage.Custom)
			insertPolicyProject(t, db, polID2, projID2)

			expectedPolicies := []*storage.Policy{
				{
					ID:         polID2,
					Name:       name2,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{projID2},
				},
			}

			ctx = insertProjectsIntoContext(ctx, []string{projID2})
			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a policy list of *, return everything": func(t *testing.T) {
			ctx := context.Background()
			name1, name2 := "testPolicy", "anotherTestPolicy"
			polID1 := insertTestPolicy(t, db, name1)
			polID2 := insertTestPolicy(t, db, name2)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID1, projID)

			expectedPolicies := []*storage.Policy{
				{
					ID:         polID1,
					Name:       name1,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{projID},
				},
				{
					ID:         polID2,
					Name:       name2,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{},
				},
			}

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a policy list of (unassigned), return policies with no projects": func(t *testing.T) {
			ctx := context.Background()
			name1, name2 := "testPolicy", "anotherTestPolicy"
			polID1 := insertTestPolicy(t, db, name1)
			polID2 := insertTestPolicy(t, db, name2)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID1, projID)

			expectedPolicies := []*storage.Policy{
				{
					ID:         polID2,
					Name:       name2,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{},
				},
			}

			ctx = insertProjectsIntoContext(ctx, []string{constants.UnassignedProjectID})
			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a project list of (unassigned) and another project, return matched policies": func(t *testing.T) {
			ctx := context.Background()
			name1, name2, name3 := "testPolicy", "anotherTestPolicy", "pika"
			polID1 := insertTestPolicy(t, db, name1)
			polID2 := insertTestPolicy(t, db, name2)
			polID3 := insertTestPolicy(t, db, name3)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID1, projID)

			projID2 := "team-rocket"
			insertTestProject(t, db, projID2, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID3, projID2)

			expectedPolicies := []*storage.Policy{
				{
					ID:         polID1,
					Name:       name1,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{projID},
				},
				{
					ID:         polID2,
					Name:       name2,
					Members:    []storage.Member{},
					Type:       storage.Custom,
					Statements: []storage.Statement{},
					Projects:   []string{},
				},
			}

			ctx = insertProjectsIntoContext(ctx, []string{constants.UnassignedProjectID, projID})
			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when there is no intersection between projects filter and projects, return empty list": func(t *testing.T) {
			ctx := context.Background()
			name1, name2, name3 := "testPolicy", "anotherTestPolicy", "pika"
			polID1 := insertTestPolicy(t, db, name1)
			insertTestPolicy(t, db, name2)
			polID3 := insertTestPolicy(t, db, name3)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID1, projID)

			projID2 := "team-rocket"
			insertTestProject(t, db, projID2, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID3, projID2)

			projID3 := "team-montag"
			insertTestProject(t, db, projID3, "we like dags", storage.Custom)

			expectedPolicies := []*storage.Policy{}

			ctx = insertProjectsIntoContext(ctx, []string{projID3})
			resp, err := store.ListPolicies(ctx)
			require.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestDeletePolicy(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			assertNoPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, genSimpleID(t, prngSeed))
				assert.Error(t, err)
				assert.Equal(t, storage.ErrNotFound, err)
			})
		},
		"policy not found with existing policies in store": func(t *testing.T) {
			ctx := context.Background()
			// Add a different policy
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestStatement(t, db, polID, "allow", "", []string{"iam:users:delete", "iam:users:create"}, []string{"iam:users"})

			assertNoPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, genSimpleID(t, prngSeed))
				assert.Equal(t, storage.ErrNotFound, err)
			})
		},
		"attached policy with no statements": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			assertPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID)
				require.NoError(t, err)
			})

			// assert that stuff happened to the database
			assertEmpty(t, db.QueryRow(policyWithID, polID))
		},
		"only one unattached policy with no statements": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")

			assertPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID)
				require.NoError(t, err)
			})

			// assert that stuff happened to the database
			assertEmpty(t, db.QueryRow(policyWithID, polID))
		},
		"only one unattached policy with two statements": func(t *testing.T) {
			ctx := context.Background()

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db,
				polID, "deny", "", []string{"infra:nodes:delete", "infra:nodes:rerun"}, []string{"infra:nodes"})

			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			assertPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1`, sID0))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND policy_id=policy_db_id($2)`, sID1, polID))
		},
		"unattached policy with two statements, next to other policy": func(t *testing.T) {
			ctx := context.Background()

			polID0 := insertTestPolicy(t, db, "firstpolicy")
			insertTestStatement(t, db,
				polID0, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			insertTestStatement(t, db,
				polID0, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertTestPolicyMember(t, db, polID0, "user:local:albertine")

			polID1 := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID1, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db,
				polID1, "deny", "", []string{"infra:nodes:delete'", "infra:nodes:rerun"}, []string{"infra:nodes"})
			member := insertTestPolicyMember(t, db, polID1, "user:local:charmander")

			assertPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID1)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(policyWithID, polID1))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))
			assertEmpty(t, db.QueryRow(statementWithID, sID1))
			// Members get left in the table for now.
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member.Name))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID0))
		},
		"only one unattached policy with one statement with projects": func(t *testing.T) {
			ctx := context.Background()

			projID0, projID1 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			insertTestProject(t, db, projID0, "let's go eevee - prod", storage.Custom)
			insertTestProject(t, db, projID1, "let's go eevee - dev", storage.Custom)

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})

			insertStatementProject(t, db, sID0, projID0)
			insertStatementProject(t, db, sID0, projID1)

			insertTestPolicyMember(t, db, polID, "user:local:eevee")

			assertPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND policy_id=policy_db_id($2)`, sID0, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE statement_id=$1`, sID0))
		},
		"when the policy overlaps with the project filter, delete the policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			assertPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID)
				require.NoError(t, err)
			})
			assertEmpty(t, db.QueryRow(policyWithID, polID))
		},
		"when the policy has no projects with a filter of (unassigned), delete the policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{constants.UnassignedProjectID})
			assertPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID)
				require.NoError(t, err)
			})
			assertEmpty(t, db.QueryRow(policyWithID, polID))
		},
		"when the the policy filter is *, delete the policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			err := store.DeletePolicy(ctx, polID)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(policyWithID, polID))
		},
		"when the policy does not overlap with the project filter, return not found": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{projID2})
			assertNoPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, polID)
				assert.Equal(t, storage.ErrNotFound, err)
			})
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestCreatePolicy(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})

	cases := map[string]func(*testing.T){
		"unattached policy with no statement": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)

			name, members, typeVal := "toBeCreated", []storage.Member{}, storage.Custom
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: members,
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
		},
		"unattached chef-managed policy with no statement": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)

			name, members, typeVal := "toBeCreated", []storage.Member{}, storage.ChefManaged
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: members,
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
		},
		"policy adding a role that doesn't exist should fail": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources := []string{"iam:users"}
			role := "notFound"
			member := genMember(t, "user:local:albertine")
			statement := storage.Statement{
				Effect:    storage.Deny,
				Role:      role,
				Resources: resources,
			}

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
			}

			resp, err := store.CreatePolicy(ctx, &pol, false)
			require.Error(t, err)
			assert.Nil(t, resp)

			_, ok := err.(*storage.ForeignKeyError)
			assert.True(t, ok, "expected foreign key error")
			assert.Equal(t, "role not found: "+role, err.Error())

			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
		},
		"policy that contains a non-existent project returns ProjectsMissingError": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom

			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			member := genMember(t, "user:local:albertine")
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
				Projects:   []string{"notfound"},
			}

			assertNoPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				assert.Error(t, err)
				_, correctError := err.(*projectassignment.ProjectsMissingError)
				assert.True(t, correctError)
				assert.Empty(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
		},
		"policy with one statement that contains resources and actions but no role": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom

			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			member := genMember(t, "user:local:albertine")
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE resources=$1 AND actions=$2 AND effect=$3 AND policy_id=policy_db_id($4) AND role_id IS NULL`,
				pq.Array(resources), pq.Array(actions), "deny", polID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"policy with one statement that contains resources, actions, and a role that exists": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal, projID := "toBeCreated", storage.Custom, "project1"
			resources, actions, role := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}, "my-fancy-role"

			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			insertTestRole(t, db, role, "role name", []string{"action1"}, []string{projID})

			member := genMember(t, "user:local:albertine")
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
				Role:      role,
			}

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(statementQueryWithRole,
				pq.Array(resources), pq.Array(actions), "deny", polID, role))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"policy with one statement that contains resources, a role that exists, but no actions": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal, projID := "toBeCreated", storage.Custom, "project1"
			resources, role := []string{"iam:users"}, "my-fancy-role"

			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			insertTestRole(t, db, role, "role name", []string{"action1"}, []string{projID})

			member := genMember(t, "user:local:albertine")
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Role:      role,
				Actions:   []string{},
			}

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE resources=$1 AND actions='{}' AND effect=$2 AND policy_id=policy_db_id($3) AND role_id=role_db_id($4)`,
				pq.Array(resources), "deny", polID, role))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"policy with one resources+actions statement and two members": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement0 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			member0 := genMember(t, "user:local:albertine")
			member1 := genMember(t, "user:local:newperson")

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member0, member1},
				Type:       typeVal,
				Statements: []storage.Statement{statement0},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(statementQueryFull,
				pq.Array(resources), pq.Array(actions), "deny", polID))
			assertMembers(t, db, polID, []storage.Member{member0, member1})
		},
		"policy with two resources+actions statements": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			resources1, actions1 := []string{"compliance:profiles"}, []string{"compliance:profiles:upload", "compliance:profiles:delete"}
			statement0 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
			}
			statement1 := storage.Statement{
				Effect:    storage.Allow,
				Resources: resources1,
				Actions:   actions1,
			}
			member := genMember(t, "user:local:albertine")

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement0, statement1},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(statementQueryFull,
				pq.Array(resources0), pq.Array(actions0), "deny", polID))
			assertOne(t, db.QueryRow(statementQueryFull,
				pq.Array(resources1), pq.Array(actions1), "allow", polID))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"policy with one resources+actions statement and two members with the same name ignores the duplicate": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement0 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
			}
			member0 := genMember(t, "user:local:albertine")

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member0, member0},
				Type:       typeVal,
				Statements: []storage.Statement{statement0},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(statementQueryFull,
				pq.Array(resources0), pq.Array(actions0), "deny", polID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=policy_db_id($1) AND member_id=member_db_id($2)`, polID, member0.Name))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member0.Name))
		},

		// Note(sr): This changed when we've dropped the iam_statements' id column.
		// I don't think it's a thing we need to fix, though.
		"policy with the same resources+actions statement passed twice": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement, statement},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.NotNil(t, resp)
			})
		},
		"policy with same resources+actions generates two different iam_statements": func(t *testing.T) {
			// Note (sr): I don't think this is wrong, just worth noting; so, here's
			// a test for this behaviour
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement0 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			statement1 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement0, statement1},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertCount(t, 2, db.QueryRow(statementQueryFull,
				pq.Array(resources), pq.Array(actions), "deny", polID))
			assertMembers(t, db, polID, []storage.Member{member})
		},

		"policy with two resources+actions+existing project statements": func(t *testing.T) {
			polID, projID := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:teams"}, []string{"iam:teams:create", "iam:teams:delete"}
			resources1, actions1 := []string{"infra:nodes"}, []string{"infra:nodes:delete", "infra:nodes:rerun"}

			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			statement0 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
				Projects:  []string{projID},
			}
			statement1 := storage.Statement{
				Effect:    storage.Allow,
				Resources: resources1,
				Actions:   actions1,
				Projects:  []string{projID},
			}
			member := genMember(t, "user:local:jigglypuff")

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement0, statement1},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(statementQueryFull,
				pq.Array(resources0), pq.Array(actions0), "deny", polID))
			assertOne(t, db.QueryRow(statementQueryFull,
				pq.Array(resources1), pq.Array(actions1), "allow", polID))
			assertMembers(t, db, polID, []storage.Member{member})
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1)`, projID))
		},
		"policy with two resources+actions+non-existent project statements fails": func(t *testing.T) {
			polID, projID := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:teams"}, []string{"iam:teams:create", "iam:teams:delete"}
			resources1, actions1 := []string{"infra:nodes"}, []string{"infra:nodes:delete", "infra:nodes:rerun"}

			statement0 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
				Projects:  []string{projID},
			}
			statement1 := storage.Statement{
				Effect:    storage.Allow,
				Resources: resources1,
				Actions:   actions1,
				Projects:  []string{projID},
			}
			member := genMember(t, "user:local:jigglypuff")

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement0, statement1},
			}

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, projID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1)`, projID))

			assertNoPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.Error(t, err)
				assert.Nil(t, resp)
			})
		},
		"policy with empty projects": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)

			name, members, typeVal := "toBeCreated", []storage.Member{}, storage.Custom
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Type:     typeVal,
				Members:  members,
				Projects: []string{},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertEmpty(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
		"policy with single project": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)

			name, members, typeVal := "toBeCreated", []storage.Member{}, storage.Custom
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Type:     typeVal,
				Members:  members,
				Projects: []string{projID},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
		"policy with multiple projects": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "so ordinary", storage.Custom)

			name, members, typeVal := "toBeCreated", []storage.Member{}, storage.Custom
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Type:     typeVal,
				Members:  members,
				Projects: []string{projID, projID2},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			projCount := db.QueryRow(policyProjectsByProjectID, polID)
			assertCount(t, 2, projCount)
		},
		"policy with only one non-existent project fails": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)

			projID := "not-real-project"

			name, members, typeVal := "toBeCreated", []storage.Member{}, storage.Custom
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Type:     typeVal,
				Members:  members,
				Projects: []string{projID},
			}

			assertNoPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.Error(t, err)
				assert.Nil(t, resp)
				_, ok := err.(*projectassignment.ProjectsMissingError)
				require.True(t, ok, "expected projectassignment.ProjectsMissingError")
			})

			assertEmpty(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertEmpty(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
		"policy with one existent, one non-existent project fails": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)

			projID0 := "special-project"
			insertTestProject(t, db, projID0, "too special", storage.Custom)
			projID1 := "not-real-project"

			name, members, typeVal := "toBeCreated", []storage.Member{}, storage.Custom
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Type:     typeVal,
				Members:  members,
				Projects: []string{projID0, projID1},
			}

			assertNoPolicyChange(t, store, func() {
				resp, err := store.CreatePolicy(ctx, &pol, false)
				require.Error(t, err)
				assert.Nil(t, resp)
				_, ok := err.(*projectassignment.ProjectsMissingError)
				require.True(t, ok, "expected projectassignment.ProjectsMissingError")
			})

			assertEmpty(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertEmpty(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestUpdatePolicy(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"policy not found": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "somename", storage.Custom
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{member},
			}
			assertNoPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				assert.Error(t, err)
				assert.Equal(t, storage.ErrNotFound, err)
				assert.Nil(t, resp)
			})
		},
		"policy not found with existing policies in store": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			// Add a different policy
			polID0 := insertTestPolicy(t, db, "testpolicy")
			insertTestStatement(t, db,
				polID0, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			insertTestPolicyMember(t, db, polID0, "user:local:albertine")

			name, typeVal := "somename", storage.Custom
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:      genSimpleID(t, prngSeed),
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{member},
			}
			assertNoPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				assert.Error(t, err)
				assert.Equal(t, storage.ErrNotFound, err)
				assert.Nil(t, resp)
			})
		},
		"policy with no statements, updating fields": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			name, typeVal := "new-name", storage.Custom
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{member},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(membersCount))
		},
		"policy that updates the project diff to contain a nonexisting project returns ProjectsMissingError": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			initPolName := "testpolicy"
			polID := insertTestPolicy(t, db, initPolName)
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			newPolName, typeVal := "new-name", storage.Custom
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:       polID,
				Name:     newPolName,
				Type:     typeVal,
				Members:  []storage.Member{member},
				Projects: []string{"notfound"},
			}

			assertNoPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				assert.Error(t, err)
				assert.Empty(t, resp)
				_, correctError := err.(*projectassignment.ProjectsMissingError)
				assert.True(t, correctError)
			})

			assertOne(t, db.QueryRow(policyFull, polID, initPolName, typeVal.String()))
			assertEmpty(t, db.QueryRow(policyFull, polID, newPolName, typeVal.String()))
		},
		"policy with no statements, changing the type": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			name, typeVal := "new-name", storage.ChefManaged
			member := genMember(t, "user:local:new_member")
			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member.Name))
		},
		"policy with no statements, adding two statements": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			member := genMember(t, "user:local:new_member")
			statement0 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			statement1 := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			name, typeVal := "new-name", storage.Custom
			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement0, statement1},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			// there's two, but we don't know their db_id
			assertCount(t, 2, db.QueryRow(statementQueryFull,
				pq.Array(resources), pq.Array(actions), "deny", polID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member.Name))
		},
		"policy with two statements, removing one statement": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete", "iam:users:create"}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db,
				polID, "deny", "", []string{"infra:nodes:delete", "infra:nodes:rerun"}, []string{"infra:nodes"})
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			name, typeVal := "new-name", storage.Custom
			member := genMember(t, "user:local:new_member")
			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			// old statements are removed
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND resources=$2 AND actions=$3 AND effect=$4 and policy_id=policy_db_id($5)`,
				sID0, pq.Array(resources), pq.Array(actions), "deny", polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND resources=$2 AND actions=$3 AND effect=$4 AND policy_id=policy_db_id($5)`,
				sID1, pq.Array(resources), pq.Array(actions), "deny", polID))
			// and the new statement is in place
			assertOne(t, db.QueryRow(statementQueryFull,
				pq.Array(resources), pq.Array(actions), "deny", polID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member.Name))
		},

		"policy with one statement, adding existing project to statement": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})

			polID := insertTestPolicy(t, db, "testpolicy")
			sID := insertTestStatement(t, db,
				polID, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})

			projID := genSimpleID(t, prngSeed)
			member := insertTestPolicyMember(t, db, polID, "user:local:totodile")
			insertTestProject(t, db, projID, "pokemon crystal", storage.Custom)

			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
				Projects:  []string{projID},
			}
			name, typeVal := "new-name", storage.Custom
			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})

			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1)`, projID))
			// removed in update
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND resources=$2 AND actions=$3 AND effect=$4 AND policy_id=policy_db_id($5)`,
				sID, pq.Array(resources), pq.Array(actions), "deny", polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE statement_id=$1 AND project_id=project_db_id($2)`, sID, projID))

			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member.Name))
		},
		"policy with one statement, adding non-existent project to statement fails": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			resources, actions := []string{"compliance:profiles"}, []string{"compliance:profiles:download"}
			assertEmpty(t, db.QueryRow("SELECT count(*) FROM iam_statements"))

			polID := insertTestPolicy(t, db, "testpolicy")
			sID := insertTestStatement(t, db, polID, "allow", "", actions, resources)

			projID := genSimpleID(t, prngSeed)
			member := insertTestPolicyMember(t, db, polID, "user:local:totodile")

			statement := storage.Statement{
				Effect:    storage.Allow,
				Resources: resources,
				Actions:   actions,
				Projects:  []string{projID},
			}
			newName, typeVal := "new-name", storage.Custom
			pol := storage.Policy{
				ID:         polID,
				Name:       newName,
				Members:    []storage.Member{member},
				Type:       typeVal,
				Statements: []storage.Statement{statement},
			}
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, projID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1)`, projID))

			assertNoPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.Error(t, err)
				assert.Nil(t, resp)
			})

			// no update to policy or members
			assertEmpty(t, db.QueryRow(policyFull, polID, newName, typeVal.String()))
			assertOne(t, db.QueryRow(policyFull, polID, "testpolicy", typeVal.String()))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member.Name))

			// no update to statement
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE statement_id=$1 AND project_id=project_db_id($2)`, sID, projID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND resources=$2 AND actions=$3 AND effect=$4 AND policy_id=policy_db_id($5)`,
				sID, pq.Array(resources), pq.Array(actions), "allow", polID))
		},
		"policy with no projects to some projects": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			name := "testPolicy"
			polID := insertTestPolicy(t, db, name)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)

			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: []string{projID},
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.ElementsMatch(t, []string{projID}, resp.Projects)
			})
			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
		"policy with project to no projects": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			name := "testPolicy"
			polID := insertTestPolicy(t, db, name)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))

			expProjs := []string{}
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: expProjs,
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.ElementsMatch(t, expProjs, resp.Projects)
			})
			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertEmpty(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
		"policy with projects to same projects": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			// TODO optimize/opt-out if they're the same?

			name := "testPolicy"
			polID := insertTestPolicy(t, db, name)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "too ordinary", storage.Custom)
			insertPolicyProject(t, db, polID, projID2)
			initPolProjCount := db.QueryRow(policyProjectsByProjectID, polID)
			assertCount(t, 2, initPolProjCount)

			expProjs := []string{projID, projID2}
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: expProjs,
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, expProjs, resp.Projects)
			})
			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			expPolProjCount := db.QueryRow(policyProjectsByProjectID, polID)
			assertCount(t, 2, expPolProjCount)
		},
		"policy with single project to diff project": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			name := "testPolicy"
			polID := insertTestPolicy(t, db, name)

			assertEmpty(t, db.QueryRow(policyProjectsByProjectID, polID))

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))

			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "too ordinary", storage.Custom)
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: []string{projID2},
			}

			expProjs := []string{projID2}
			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.ElementsMatch(t, expProjs, resp.Projects)
			})

			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
		"policy with one project to additional project": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			name := "testPolicy"
			polID := insertTestPolicy(t, db, name)
			assertEmpty(t, db.QueryRow(policyProjectsByProjectID, polID))

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))

			projID2 := "another-project"
			insertTestProject(t, db, projID2, "more", storage.Custom)

			expProjs := []string{projID, projID2}
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: expProjs,
			}

			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, expProjs, resp.Projects)
			})

			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			projCount := db.QueryRow(policyProjectsByProjectID, polID)
			assertCount(t, 2, projCount)
		},
		"policy with project to add non-existent project fails": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			name := "testPolicy"
			polID := insertTestPolicy(t, db, name)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))

			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: []string{projID, "not-real"},
			}

			assertNoPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				assert.Error(t, err)
				assert.Nil(t, resp)
			})

			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE policy_id=policy_db_id($1)`, polID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyProjectsByProjectID, polID))
		},
		"when the policy's projects and the project filter intersect, update policy": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)

			name, typeVal := "new-name", storage.Custom
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{},
			}
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{projID1}, []string{SuperuserSubject})
			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})
			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
		},
		"when the * project filter is passed, update policy": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{constants.AllProjectsExternalID}, []string{SuperuserSubject})

			name, typeVal := "new-name", storage.Custom
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})
			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, update policy": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")

			name, typeVal := "new-name", storage.Custom
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{},
			}
			projID1 := "team-rocket"
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{projID1, constants.UnassignedProjectID}, []string{SuperuserSubject})
			assertPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				require.NoError(t, err)
				assert.Equal(t, &pol, resp)
			})
			assertOne(t, db.QueryRow(policyFull, polID, name, typeVal.String()))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			originalName := "blasting off again"
			polID := insertTestPolicy(t, db, originalName)
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{projID2, constants.UnassignedProjectID})
			pol := storage.Policy{
				ID:      polID,
				Name:    "new-name",
				Type:    storage.Custom,
				Members: []storage.Member{},
			}
			ctx = insertProjectsIntoContext(ctx, []string{projID2, constants.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2`, polID, originalName))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestPurgeSubjectFromPolicies(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()
	subject := "users:local:albertine"

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"empty database, returns empty array", func(t *testing.T) {
			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.Empty(t, ids)
		}},
		{"one policy exists, but doesn't match, returns empty array", func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, "user:local:someone-else")

			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.Empty(t, ids)

			assertOne(t, db.QueryRow(policyMembersByMemberName, member.Name))
		}},
		{"one policy matches, returns this policy's ID", func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, subject)

			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.Equal(t, []string{polID}, ids)

			assertEmpty(t, db.QueryRow(policyMembersByMemberName, member.Name))
		}},
		{"two policies match, returns their IDs", func(t *testing.T) {
			polID0 := insertTestPolicy(t, db, "testpolicy0")
			polID1 := insertTestPolicy(t, db, "testpolicy1")
			member := insertTestPolicyMember(t, db, polID0, subject)
			_, err := db.Exec(`INSERT INTO iam_policy_members (policy_id, member_id) VALUES (policy_db_id($1), member_db_id($2))`, polID1, member.Name)
			require.NoError(t, err)

			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.ElementsMatch(t, []string{polID0, polID1}, ids)

			assertEmpty(t, db.QueryRow(policyMembersByMemberName, member.Name))
		}},
		{"one policy matches, with extra members, those are kept intact", func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			member0 := insertTestPolicyMember(t, db, polID, subject)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:member1")

			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.Equal(t, []string{polID}, ids)

			assertEmpty(t, db.QueryRow(policyMembersByMemberName, member0.Name))
			assertOne(t, db.QueryRow(policyMembersByMemberName, member1.Name))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.Flush(t)
	}
}
