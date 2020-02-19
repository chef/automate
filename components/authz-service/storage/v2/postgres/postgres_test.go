package postgres_test

import (
	"context"
	"database/sql"
	"math/rand"
	"sort"
	"strconv"
	"testing"

	"github.com/jaswdr/faker"
	"github.com/lib/pq"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	v2 "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/prng"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/components/authz-service/testhelpers"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/projectassignment"
)

const (
	Applied          = "applied"
	Staged           = "staged"
	SuperuserSubject = "tls:service:deployment-service:internal"
)

const (
	policyMembersByMemberName = "SELECT count(*) FROM iam_policy_members WHERE member_id=member_db_id($1)"
	policyMembersByPolicyID   = "SELECT count(*) FROM iam_policy_members WHERE policy_id=policy_db_id($1)"
	policyMembersFull         = "SELECT count(*) FROM iam_policy_members WHERE policy_id=policy_db_id($1) and member_id=member_db_id($2)"
	policyWithID              = "SELECT count(*) FROM iam_policies WHERE id=$1"
	statementWithID           = "SELECT count(*) FROM iam_statements WHERE db_id=$1"
	statementQueryWithRole    = "SELECT count(*) FROM iam_statements WHERE resources=$1 AND actions=$2 AND effect=$3 AND policy_id=policy_db_id($4) AND role_id=role_db_id($5)"
	statementQueryFull        = "SELECT count(*) FROM iam_statements WHERE resources=$1 AND actions=$2 AND effect=$3 AND policy_id=policy_db_id($4)"
	policyFull                = "SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3"
	membersCount              = "SELECT count(*) FROM iam_members"
	policyProjectsByProjectID = "SELECT count(*) FROM iam_policy_projects WHERE policy_id=policy_db_id($1)"
)

// Note: to set up PG locally for running these tests,
// run the following from your command line from the components/authz-service folder:
//
// To start postgres in a docker container:
//
// make setup_docker_pg
//
// To run the tests, you can run this multiple times:
//
// make test_with_db
//
// When you are done testing, spin down the postgres container:
//
// make kill_docker_pg

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
			assert.Equal(t, storage_errors.ErrNotFound, err)
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

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.GetPolicy(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, polID, resp.ID)
		},
		"when the policy has no projects and (unassigned) is in the projects filter, return policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
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

			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			resp, err := store.GetPolicy(ctx, polID)

			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListPolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			resp, err := store.ListPolicyMembers(ctx, genSimpleID(t, prngSeed))
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"contains policies but looking for a different one": func(t *testing.T) {
			insertTestPolicy(t, db, "testpolicy")

			resp, err := store.ListPolicyMembers(ctx, genSimpleID(t, prngSeed))

			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"policy with no members": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")

			members, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Empty(t, members)
		},
		"policy with a single member": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")

			members, err := store.ListPolicyMembers(ctx, polID)
			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member}, members)
		},
		"policy with multiple members and statements returns alphabetically by name": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			insertTestStatement(t, db,
				polID, "deny", "", []string{"infra:nodes:delete", "infra:nodes:rerun"}, []string{"infra:nodes"})

			member0 := insertTestPolicyMember(t, db, polID, "user:local:c")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:a")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:b")

			members, err := store.ListPolicyMembers(ctx, polID)
			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2, member0}, members)
		},
		"when the policy's projects and the project filter intersect, list members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			resp, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2}, resp)
		},
		"when the * project filter is passed, list members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2}, resp)
		},
		"when the policy has no projects and (unassigned) is in the projects filter, list members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
			resp, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2}, resp)
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			resp, err := store.ListPolicyMembers(ctx, polID)

			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
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

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
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

			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID})
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

			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID, projID})
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
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
		},
		"policy not found with existing policies in store": func(t *testing.T) {
			ctx := context.Background()
			// Add a different policy
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestStatement(t, db, polID, "allow", "", []string{"iam:users:delete", "iam:users:create"}, []string{"iam:users"})

			assertNoPolicyChange(t, store, func() {
				err := store.DeletePolicy(ctx, genSimpleID(t, prngSeed))
				assert.Equal(t, storage_errors.ErrNotFound, err)
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

			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID})
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

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
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
				assert.Equal(t, storage_errors.ErrNotFound, err)
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

			_, ok := err.(*storage_errors.ForeignKeyError)
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

func TestReplacePolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, wrongPolID, []storage.Member{member})
				assert.Error(t, err)
				assert.Empty(t, resp)
			})
		},
		"policy not found reports an error": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member1})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
		},
		"updating policy with NO members to SOME members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
		},
		"updating policy with SOME members to NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"updating policy with SOME members to NEW members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			polMember1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			polMember2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			member1 := genMember(t, "user:local:foo")
			member2 := genMember(t, "user:local:bar")
			member3 := genMember(t, "team:saml:gophers")
			member4 := genMember(t, "team:saml:editors")
			members := []storage.Member{member1, member2, member3, member4}

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, len(members), db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, len(members)+2, db.QueryRow(membersCount))

			// old members no longer associated with policy
			assertEmpty(t, db.QueryRow(policyMembersFull, polID, polMember1.Name))
			assertEmpty(t, db.QueryRow(policyMembersFull, polID, polMember2.Name))

			// new members
			assertMembers(t, db, polID, members)
		},
		"updating policy with SOME members to NEW members with some REUSED members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			polMember1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			polMember2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			polMember3 := insertTestPolicyMember(t, db, polID, "team:local:friends")
			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			member3 := genMember(t, "team:saml:gophers")
			member4 := genMember(t, "team:saml:editors")
			members := []storage.Member{member1, member2, member3, member4}

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 5, db.QueryRow(membersCount))

			// re-used members plus new members
			assertMembers(t, db, polID, []storage.Member{polMember1, polMember2, member3, member4})

			// member still exists but disassociated from policy
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, polMember3.Name))
			assertEmpty(t, db.QueryRow(policyMembersFull, polID, polMember3.Name))
		},
		"updating policy by ADDING member from ANOTHER policy": func(t *testing.T) {
			ctx := context.Background()
			otherPolID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, otherPolID, "user:local:originaluser")

			polID := insertTestPolicy(t, db, "otherTestPolicy")
			member := genMember(t, "user:local:originaluser")

			// baseline: member is in just one policy
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			// now still just one member but in two policies
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"updating policy by REMOVING member from ANOTHER policy": func(t *testing.T) {
			ctx := context.Background()
			otherPolID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, otherPolID, "user:local:originaluser")

			polID := insertTestPolicy(t, db, "otherTestPolicy")
			_, err := db.Query(`INSERT INTO iam_policy_members (policy_id, member_id) values(policy_db_id($1), member_db_id($2))`, polID, member.Name)
			require.NoError(t, err)

			// baseline: member is in two policies
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			// now member remains in just one policy
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy's projects and the project filter intersect, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"when the * project filter is passed, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestRemovePolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, wrongPolID, []storage.Member{member})
				assert.Error(t, err)
				assert.Empty(t, resp)
			})
		},
		"policy not found reports an error": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
		},
		"removing members from policy with NO members results in an empty member list": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Empty(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
		},
		"removing members from policy with SOME members to now have NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Empty(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"removing repeat members from policy with SOME members to now have LESS members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			remainingMember := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:fred")
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 1, len(resp))
				require.Contains(t, resp, remainingMember)
			})

			assertOne(t,
				db.QueryRow(policyMembersFull,
					polID, remainingMember.Name))
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"removing members from policy with ONE member to now have NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			assertCount(t, 1, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 1, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:fred")
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Empty(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 1, db.QueryRow(membersCount))
		},
		"removing only non-members from policy results in no change to policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 2, len(resp))
				require.Contains(t, resp, member1)
				require.Contains(t, resp, member2)
			})

			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"removing members from policy with SOME members to now have less members with some ignored": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			polMember3 := insertTestPolicyMember(t, db, polID, "user:local:charmander")
			polMember4 := insertTestPolicyMember(t, db, polID, "user:local:squirtle")
			polMember5 := insertTestPolicyMember(t, db, polID, "user:local:bulbasaur")
			assertCount(t, 5, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 5, db.QueryRow(membersCount))
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "team:saml:notfound1")
			member3 := genMember(t, "user:local:mary")
			member4 := genMember(t, "team:saml:notfound2")
			members := []storage.Member{member1, member2, member3, member4}

			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
				require.Contains(t, resp, polMember3)
				require.Contains(t, resp, polMember4)
				require.Contains(t, resp, polMember5)
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 5, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{polMember3, polMember4, polMember5})
		},
		"when the policy's projects and the project filter intersect, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			members := []storage.Member{member1, member2}

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the * project filter is passed, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestAddPolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"fails to add with ErrNotFound when the database is empty and members are added to a non-existent policy": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, wrongPolID, []storage.Member{member})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
		},
		"fails to add with ErrNotFound when a policy exists but members are added to a non-existent policy": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
		},
		"adding one member to a policy with NO members results in member being added": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 1, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 1, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"adding several members to a policy with NO members results in members being added": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			member3 := genMember(t, "user:local:max")
			member4 := genMember(t, "user:local:ellen")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member1, member2, member3, member4})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 4, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3, member4})
		},
		"adding one member to a policy with SOME members succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
			member3 := genMember(t, "user:local:max")

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding several members to a policy with SOME members succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
			member3 := genMember(t, "user:local:max")
			member4 := genMember(t, "user:local:ellen")
			member5 := genMember(t, "user:local:barry")
			member6 := genMember(t, "user:local:jack")

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3, member4, member5, member6})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 6, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 6, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3, member4, member5, member6})
		},
		"adding same member more than once w/ single request doesn't result in duplicate policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			member3 := genMember(t, "user:local:ellen")
			repeatMember3 := genMember(t, "user:local:ellen")

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3, repeatMember3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding same member more than once w/ duplicate request doesn't result in duplicate policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			member3 := genMember(t, "user:local:ellen")

			assertPolicyChange(t, store, func() {
				// add ellen to the policy
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})

			// This isn't quite right. It's better if we don't update the change id, but that requires
			// the transaction to fail
			assertPolicyChange(t, store, func() {
				// attempt to add ellen again in a duplicate request
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding subset of members that's already part of a different policy succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID1 := insertTestPolicy(t, db, "testpolicy1")
			polID2 := insertTestPolicy(t, db, "testpolicy2")
			member1 := insertTestPolicyMember(t, db, polID1, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID1, "user:local:mary")
			member3 := insertTestPolicyMember(t, db, polID1, "user:local:ellen")
			member4 := genMember(t, "user:local:max")
			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID1))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID2))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID2, []storage.Member{member1, member2, member3, member4})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 4, len(resp))
			})

			assertMembers(t, db, polID1, []storage.Member{member1, member2, member3})
			assertMembers(t, db, polID2, []storage.Member{member1, member2, member3, member4})
			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID1))
			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID2))
			assertCount(t, 4, db.QueryRow(membersCount))
		},
		"adding members where the members match the existing policy membership results in no new members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 2, len(resp))
			})

			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
		},
		"adding member that's already a member of a different policy doesn't result in additional iam_members entry": func(t *testing.T) {
			ctx := context.Background()
			polID1 := insertTestPolicy(t, db, "testpolicy1")
			polID2 := insertTestPolicy(t, db, "testpolicy2")
			member := insertTestPolicyMember(t, db, polID1, "user:local:fred")
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID1))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID2))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID2, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 1, len(resp))
			})

			assertMembers(t, db, polID1, []storage.Member{member})
			assertMembers(t, db, polID2, []storage.Member{member})
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersFull, polID1, member.Name))
			assertOne(t, db.QueryRow(policyMembersFull, polID2, member.Name))
		},
		"when the policy's projects and the project filter intersect, add members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			assertPolicyChange(t, store, func() {
				ctx = insertProjectsIntoContext(ctx, []string{projID1})
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the * project filter is passed, add members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, add members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
			})
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
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
				assert.Equal(t, storage_errors.ErrNotFound, err)
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
				assert.Equal(t, storage_errors.ErrNotFound, err)
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
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{v2.AllProjectsExternalID}, []string{SuperuserSubject})

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
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{projID1, v2.UnassignedProjectID}, []string{SuperuserSubject})
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

			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			pol := storage.Policy{
				ID:      polID,
				Name:    "new-name",
				Type:    storage.Custom,
				Members: []storage.Member{},
			}
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.UpdatePolicy(ctx, &pol)
				assert.Nil(t, resp)
				assert.Equal(t, storage_errors.ErrNotFound, err)
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

func TestCreateRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"when the project doesn't exist, return ForeignKeyError": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", "project-not-found", "name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)
			require.Error(t, err)
			assert.Nil(t, resp)
			_, ok := err.(*storage_errors.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when rule exists in the applied rules table, return error": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertAppliedRuleWithMultipleConditions(t, db, "copy", projID, storage.Node)

			resp, err := store.CreateRule(ctx, rule)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrConflict, err)
		},
		"when rule exists in the staging rules table, return error": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertStagedRuleWithMultipleConditions(t, db, "copy", projID, storage.Node, false)

			resp, err := store.CreateRule(ctx, rule)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrConflict, err)
		},
		"cannot use improper condition attributes for events": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefTag, storage.MemberOf)
			require.NoError(t, err)
			_, err = storage.NewRule("new-rule", projID, "name", storage.Event, []storage.Condition{condition})
			assert.Error(t, err)
		},
		"creating a rule with no conditions returns an error": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			_, err := storage.NewRule("new-id-1", projID, "name", storage.Node, []storage.Condition{})
			assert.Error(t, err)
		},
		"creating a condition with zero entries for the 'equals' operator returns an error": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{}, storage.ChefServer, storage.Equals)
			assert.Equal(t, storage.Condition{}, condition1)
			assert.Error(t, err)
		},
		"creating a condition with zero entries for the 'member-of' operator returns an error": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{}, storage.ChefServer, storage.MemberOf)
			assert.Equal(t, storage.Condition{}, condition1)
			assert.Error(t, err)
		},
		"creating an equals condition with multiple entries returns an error": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"chef-server-1", "chef-server-2"}, storage.ChefServer, storage.Equals)
			assert.Equal(t, storage.Condition{}, condition1)
			assert.Error(t, err)
		},
		"creating a condition with multiple entries for 'member-of' operator is allowed": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"1", "2", "3"}, storage.ChefServer, storage.MemberOf)
			assert.NotNil(t, condition1)
			require.NoError(t, err)
		},
		"creating a condition with a single entry for 'member-of' operator is allowed": func(t *testing.T) {
			condition1, err := storage.NewCondition([]string{"1"}, storage.ChefServer, storage.MemberOf)
			assert.NotNil(t, condition1)
			require.NoError(t, err)
		},
		"create node rule with multiple conditions": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Node
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			condition2, err := storage.NewCondition([]string{"org1", "org2", "org3"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			condition3, err := storage.NewCondition([]string{"role1"}, storage.ChefRole, storage.MemberOf)
			require.NoError(t, err)
			ruleID := "new-id-1"
			rule, err := storage.NewRule(ruleID, "project-1", "name", ruleType,
				[]storage.Condition{condition1, condition2, condition3})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)
			require.NoError(t, err)
			require.Equal(t, &rule, resp)
			assertCount(t, 3, db.QueryRow(
				`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleID))
		},
		"create event rule with multiple conditions": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Event
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			condition2, err := storage.NewCondition([]string{"org1", "org2", "org3"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			condition3, err := storage.NewCondition([]string{"chef-server-2", "chef-server-3"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleID := "new-id-1"
			rule, err := storage.NewRule(ruleID, "project-1", "name", ruleType,
				[]storage.Condition{condition1, condition2, condition3})
			require.NoError(t, err)

			resp, err := store.CreateRule(ctx, &rule)
			require.NoError(t, err)
			require.Equal(t, &rule, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND type=$2
				AND project_id=project_db_id($3) AND name=$4 AND deleted=$5`,
				rule.ID, rule.Type.String(), rule.ProjectID, rule.Name, false))
			assertCount(t, 3, db.QueryRow(
				`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListRules(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist, returns an empty list": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.Nil(t, resp)
		},
		"when only staged rules exist, returns an empty list": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			insertStagedRuleWithMultipleConditions(t, db, "staged-rule", projID, storage.Node, false)
			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.Nil(t, resp)
		},
		"when multiple rules exist with no project filter, returns the full list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Node)

			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1, rule2}, resp)
		},
		"when staged and applied rules exist with no project filter, returns applied rules": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule1", projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, "rule2", projID, storage.Event, false)

			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			require.NotZero(t, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule1}, resp)
		},
		"when multiple rules exist with a project filter, returns filtered list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", projID2})

			ruleType := storage.Node
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType)

			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, ruleType)

			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			require.NotZero(t, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2}, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListStagedAndAppliedRules(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist, returns an empty list": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListRules(ctx)
			require.NoError(t, err)
			assert.Nil(t, resp)
			assert.Zero(t, len(resp))
		},
		"when multiple staged and applied rules exist with no project filter, returns the full list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Event)

			rule3 := insertStagedRuleWithMultipleConditions(t, db, "rule-3", projID, storage.Node, false)
			rule4 := insertStagedRuleWithMultipleConditions(t, db, "rule-4", projID, storage.Node, false)

			resp, err := store.ListStagedAndAppliedRules(ctx)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1, rule2, rule3, rule4}, resp)
		},
		"when multiple staged and applied rules exist with a project filter, returns filtered list": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", projID2})

			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "applied-rule2", projID2, storage.Event)

			insertStagedRuleWithMultipleConditions(t, db, "staged-rule", projID, storage.Event, false)
			rule4 := insertStagedRuleWithMultipleConditions(t, db, "staged-rule4", projID2, storage.Node, false)

			resp, err := store.ListStagedAndAppliedRules(ctx)
			require.NoError(t, err)
			require.NotZero(t, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2, rule4}, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListRulesForProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when no rules exist and requested project does not exist, returns ForeignKeyError", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "some-project", "some other project", storage.Custom)

			resp, status, err := store.ListRulesForProject(ctx, "project-not-found")
			require.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.RulesStatusError, status)
			_, ok := err.(*storage_errors.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		}},
		{"when requested project does not exist, returns ForeignKeyError", func(t *testing.T) {
			ctx := context.Background()
			projID := "some-project"
			insertTestProject(t, db, projID, "some other project", storage.Custom)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, "project-not-found")
			require.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.RulesStatusError, status)
			_, ok := err.(*storage_errors.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		}},
		{"when project exists but no rules exist, returns an empty list", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.Nil(t, resp)
			assert.Zero(t, len(resp))
			assert.Equal(t, storage.NoRules, status)
		}},
		{"when rules exist but not for the project queried, returns an empty list", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID2, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.Nil(t, resp)
			assert.Zero(t, len(resp))
			assert.Equal(t, storage.NoRules, status)
		}},
		{"when multiple applied rules exist with no project filter, returns rules for the project", func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, storage.Node)
			rule3 := insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Event)

			resp, status, err := store.ListRulesForProject(ctx, projID2)
			require.NoError(t, err)
			assert.Equal(t, 2, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2, rule3}, resp)
			assert.Equal(t, storage.Applied, status)
		}},
		{"when the requested project is in the filter, returns the rules for the project", func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", projID2})

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, storage.Event)
			rule3 := insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Event)

			resp, status, err := store.ListRulesForProject(ctx, projID2)
			require.NoError(t, err)
			assert.Equal(t, 2, len(resp))
			assert.ElementsMatch(t, []*storage.Rule{rule2, rule3}, resp)
			assert.Equal(t, storage.Applied, status)
		}},
		{"when the requested project is not in the filter, returns ErrNotFound", func(t *testing.T) {
			ctx := context.Background()
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", "project-4"})
			ruleType := storage.Event
			insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, ruleType)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, ruleType)

			resp, status, err := store.ListRulesForProject(ctx, projID2)
			require.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.RulesStatusError, status)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		}},
		{"when there are only staged changes for the project's rules, returns the staged versions of the rules", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "first project", storage.Custom)

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("first-rule", projID, "the very first rule", storage.Node,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			updatedCondition, err := storage.NewCondition([]string{"new-chef-role"}, storage.ChefRole, storage.Equals)
			require.NoError(t, err)
			updatedRule, err := storage.NewRule(rule.ID, projID, "updated rule name", rule.Type,
				[]storage.Condition{updatedCondition})
			insertStagedRule(t, db, &updatedRule, false)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{&updatedRule}, resp)
			assert.Equal(t, storage.EditsPending, status)

		}},
		{"when there are staged changes for some of the project's rules, returns those staged versions and rules that have no staged changes", func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "first project", storage.Custom)

			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("first-rule", projID, "the very first rule", storage.Event,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			updatedCondition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.Equals)
			require.NoError(t, err)
			updatedRule, err := storage.NewRule(rule.ID, projID, "updated rule name", rule.Type,
				[]storage.Condition{updatedCondition})
			require.NoError(t, err)
			insertStagedRule(t, db, &updatedRule, false)

			appliedRule := insertAppliedRuleWithMultipleConditions(t, db, "applied", projID, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{&updatedRule, appliedRule}, resp)
			assert.Equal(t, storage.EditsPending, status)
		}},
		{"when a project has two applied rules and one is staged for deletion, returns only the non-deleted one", func(t *testing.T) {
			ctx := context.Background()
			projID := "foo-project"
			insertTestProject(t, db, projID, "first project", storage.Custom)

			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Event)
			insertDeletedStagedRule(t, db, rule2)

			resp, status, err := store.ListRulesForProject(ctx, projID)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1}, resp)
			assert.Equal(t, storage.Applied, status)
		}},
		{"when multiple projects exist, returns only the requested project's rules", func(t *testing.T) {
			ctx := context.Background()
			projID1 := "foo-project"
			insertTestProject(t, db, projID1, "first project", storage.Custom)
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID1, storage.Event)

			projID2 := "bar-project"
			insertTestProject(t, db, projID2, "second project", storage.Custom)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID2, storage.Node)

			resp, status, err := store.ListRulesForProject(ctx, projID1)
			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Rule{rule1}, resp)
			assert.NotContains(t, resp, rule2)
			assert.Equal(t, storage.Applied, status)
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

func TestUpdateRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when the project does not exist, return ForeignKeyError": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("rule1", projID, "name", storage.Node, []storage.Condition{condition1})
			insertAppliedRule(t, db, &rule)
			require.NoError(t, err)

			rule.ProjectID = "project-not-found"
			resp, err := store.UpdateRule(ctx, &rule)
			require.Error(t, err)
			assert.Nil(t, resp)
			_, ok := err.(*storage_errors.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when the rule doesn't exist in either applied or staged, return ErrNotFound": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("not-found", projID, "name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &rule)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"when the update attempts to change the project, throw an error": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleOriginal, err := storage.NewRule("new-id-1", "project-1", "name", ruleType,
				[]storage.Condition{condition1})
			require.NoError(t, err)
			insertAppliedRule(t, db, &ruleOriginal)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)

			ruleUpdated, err := storage.NewRule(ruleOriginal.ID, projID2, ruleOriginal.Name, ruleType,
				[]storage.Condition{condition1})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrChangeProjectForRule, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleUpdated.ID, ruleUpdated.Name, ruleUpdated.Type.String(), ruleUpdated.ProjectID))
		},
		"when the update attempts to change the type, throw an error": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "project name", storage.Custom)

			ruleType := storage.Node
			condition, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleOriginal, err := storage.NewRule("new-id-1", "project-1", "rule name", ruleType,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertAppliedRule(t, db, &ruleOriginal)

			ruleUpdated, err := storage.NewRule(ruleOriginal.ID, projID, ruleOriginal.Name, storage.Event,
				[]storage.Condition{condition})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrChangeTypeForRule, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleUpdated.ID, ruleUpdated.Name, ruleUpdated.Type.String(), ruleUpdated.ProjectID))
		},
		"when there is no project filter, update node rule with multiple conditions to have more conditions": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			rule := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType)

			condition4, err := storage.NewCondition(
				[]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition4}
			ruleUpdated, err := storage.NewRule(rule.ID, projID, "name", ruleType, append(conditions, rule.Conditions...))
			require.NoError(t, err)
			ruleUpdated.Status = Applied
			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			require.NoError(t, err)
			assert.Equal(t, &ruleUpdated, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, rule.ID))
		},
		"when the project filter matches, update node rule with multiple conditions to have fewer conditions, different name": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "some-other-project"})

			ruleType := storage.Node
			condition1, err := storage.NewCondition(
				[]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			condition2, err := storage.NewCondition(
				[]string{"org1", "org2", "org3"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			condition3, err := storage.NewCondition(
				[]string{"role1"}, storage.ChefRole, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "name", ruleType,
				[]storage.Condition{condition1, condition2, condition3})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			condition4, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleUpdated, err := storage.NewRule("new-id-1", projID, "updated", rule.Type,
				[]storage.Condition{condition4})
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			require.NoError(t, err)
			assert.Equal(t, &ruleUpdated, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3`,
				ruleUpdated.ID, ruleUpdated.Name, ruleUpdated.Type.String()))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleUpdated.ID))
		},
		"when the project filter does not match, return ErrNotFound": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"not-a-match", "some-other-project"})

			ruleType := storage.Node
			ruleOriginal := insertAppliedRuleWithMultipleConditions(t, db, "rule-original", projID, ruleType)

			condition4, err := storage.NewCondition(
				[]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition4}
			ruleUpdated, err := storage.NewRule(ruleOriginal.ID, projID, "name", ruleType, append(conditions, ruleOriginal.Conditions...))
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &ruleUpdated)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
		},
		"when the rule exists in applied but not staged, adds a new rule to staged": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			ruleOriginal := insertAppliedRuleWithMultipleConditions(t, db, "rule-original", projID, ruleType)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))

			condition4, err := storage.NewCondition(
				[]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition4}
			updatedRule, err := storage.NewRule(ruleOriginal.ID, projID, "new name", ruleType, append(conditions, ruleOriginal.Conditions...))
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &updatedRule)
			require.NoError(t, err)
			assert.Equal(t, &updatedRule, resp)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				ruleOriginal.ID, ruleOriginal.Name, ruleOriginal.Type.String(), ruleOriginal.ProjectID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
		"when the rule exists in staged but not applied, updates the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition}
			originalRule, err := storage.NewRule("foo-rule", projID, "foo", storage.Event, conditions)
			require.NoError(t, err)
			insertStagedRule(t, db, &originalRule, false)

			newCondition, err := storage.NewCondition([]string{"new-chef-server-2"}, storage.ChefServer, storage.Equals)
			updatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "foo bar", originalRule.Type, append(conditions, newCondition))

			resp, err := store.UpdateRule(ctx, &updatedRule)
			require.NoError(t, err)
			assert.Equal(t, &updatedRule, resp)

			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
		"when the rule exists in both staged and applied, updates the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition}
			originalRule, err := storage.NewRule("foo-rule", projID, "foo", storage.Event, conditions)
			require.NoError(t, err)
			insertAppliedRule(t, db, &originalRule)
			insertStagedRule(t, db, &originalRule, false)

			newCondition, err := storage.NewCondition([]string{"new-chef-server-2"}, storage.ChefServer, storage.Equals)
			require.NoError(t, err)
			updatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "foo bar", originalRule.Type, append(conditions, newCondition))
			require.NoError(t, err)

			resp, err := store.UpdateRule(ctx, &updatedRule)
			require.NoError(t, err)
			assert.Equal(t, &updatedRule, resp)

			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
		"when the rule exists in applied but is marked for deletion in staged, returns marked for deletion": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition, err := storage.NewCondition([]string{"new-chef-server"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			conditions := []storage.Condition{condition}
			originalRule, err := storage.NewRule("foo-rule", projID, "foo", storage.Event, conditions)
			require.NoError(t, err)
			insertAppliedRule(t, db, &originalRule)
			deletedUpdatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "foo bar", originalRule.Type, conditions)
			insertDeletedStagedRule(t, db, &deletedUpdatedRule)

			newCondition, err := storage.NewCondition([]string{"new-chef-server-2"}, storage.ChefServer, storage.Equals)
			updatedRule, err := storage.NewRule(originalRule.ID, originalRule.ProjectID, "this better not work", originalRule.Type, append(conditions, newCondition))

			resp, err := store.UpdateRule(ctx, &updatedRule)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrMarkedForDeletion, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
				updatedRule.ID, updatedRule.Name, updatedRule.Type.String(), updatedRule.ProjectID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`,
				updatedRule.ID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestGetStagedOrAppliedRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist and requested project does not exist, returns ForeignKeyError": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.GetStagedOrAppliedRule(ctx, "project-not-found", "some-rule")
			require.Error(t, err)
			assert.Nil(t, resp)
			_, ok := err.(*storage_errors.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when no rules exist in either staged or applied, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "testing", storage.Custom)
			resp, err := store.GetStagedOrAppliedRule(ctx, projID, "not-found")
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"when the rule doesn't exist in applied or staged, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, "not-found")
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"when multiple rules exists with no project filter, return correct rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleToGet := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "other-rule", projID, storage.Event)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, ruleToGet.ID)
			require.NoError(t, err)
			assert.Equal(t, ruleToGet, resp)
		},
		"when multiple rules exists with a matching project filter, return correct rule": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, projID2, "some-other-project"})

			ruleToGet := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "other-project-rule", projID2, storage.Event)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, ruleToGet.ID)
			require.NoError(t, err)
			assert.Equal(t, ruleToGet, resp)
		},
		"when multiple rules exists with a non-matching project filter, return NotFoundErr": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			projID2 := "project-2"
			insertTestProject(t, db, projID2, "pika p", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, "some-other-project"})

			ruleToGet := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			insertAppliedRuleWithMultipleConditions(t, db, "other-rule", projID2, storage.Node)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, ruleToGet.ID)
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"when the rule exists only in the staged table, returns the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)
			insertStagedRule(t, db, &rule, false)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, rule.ID)
			require.NoError(t, err)
			assert.NotNil(t, resp)
			expectedRule := storage.Rule{
				ID:         rule.ID,
				ProjectID:  rule.ProjectID,
				Name:       rule.Name,
				Type:       rule.Type,
				Conditions: rule.Conditions,
				Deleted:    false,
				Status:     "staged",
			}
			assert.Equal(t, &expectedRule, resp)
		},
		"when the rule exists only in the applied table, returns the applied rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "my new project", storage.Custom)

			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "name", storage.Event, []storage.Condition{condition1})
			insertAppliedRule(t, db, &rule)

			resp, err := store.GetStagedOrAppliedRule(ctx, projID, rule.ID)
			require.NoError(t, err)
			assert.NotNil(t, resp)
			expectedRule := storage.Rule{
				ID:         rule.ID,
				ProjectID:  rule.ProjectID,
				Name:       rule.Name,
				Type:       rule.Type,
				Conditions: rule.Conditions,
				Deleted:    false,
				Status:     Applied,
			}
			assert.Equal(t, &expectedRule, resp)
		},
		"when the rule exists in the staged and applied tables, returns the staged rule": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "my new project", storage.Custom)

			condition1, err := storage.NewCondition([]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule, err := storage.NewRule("new-id-1", projID, "applied name", storage.Node, []storage.Condition{condition1})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule)

			stagedRule, err := storage.NewRule(rule.ID, rule.ProjectID, "update: staged name", rule.Type, rule.Conditions)
			require.NoError(t, err)
			insertStagedRule(t, db, &stagedRule, false)
			resp, err := store.GetStagedOrAppliedRule(ctx, projID, rule.ID)
			require.NoError(t, err)
			assert.NotNil(t, resp)
			expectedRule := storage.Rule{
				ID:         stagedRule.ID,
				ProjectID:  stagedRule.ProjectID,
				Name:       stagedRule.Name,
				Type:       stagedRule.Type,
				Conditions: stagedRule.Conditions,
				Deleted:    false,
				Status:     "staged",
			}
			assert.Equal(t, &expectedRule, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestDeleteRule(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"when no rules exist and requested project does not exist, returns ForeignKeyError": func(t *testing.T) {
			ctx := context.Background()
			err := store.DeleteRule(ctx, "project-not-found", "rule1")
			require.Error(t, err)
			_, ok := err.(*storage_errors.ForeignKeyError)
			require.True(t, ok, "mismatches expected error type")
			assert.Equal(t, "project not found: project-not-found", err.Error())
		},
		"when no rules exist but requested project exists, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "foo-project"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"when an applied rule exists but the wrong id requested, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertAppliedRuleWithMultipleConditions(t, db, "some-rule", projID, storage.Event)

			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions`))
		},
		"when an applied and staged rule exists but the wrong id requested, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Event)
			insertStagedRuleWithMultipleConditions(t, db, rule.ID, rule.ProjectID, rule.Type, false)

			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions`))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when only staged rule exists but the wrong id requested, returns NotFoundErr": func(t *testing.T) {
			ctx := context.Background()
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			rule := insertStagedRuleWithMultipleConditions(t, db, "staged", projID, storage.Event, false)

			err := store.DeleteRule(ctx, projID, "not-found")
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, rule.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged rules exist with no project filter, delete rule and associated conditions": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			ruleToSave := insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToSave.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged rules exist with a matching project filter and no applied rules, delete rule and associated conditions": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged rules exists with a non-matching project filter, do not delete anything": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			ruleToSave := insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToSave.ID))
			assertCount(t, 6, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged and applied rules exist with a non-matching project filter, do not delete anything": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"project-3", "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertStagedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType, false)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType)

			condition4, err := storage.NewCondition(
				[]string{"chef-server-2"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			ruleToSave, err := storage.NewRule("new-id-2", projID, "name2", ruleType,
				[]storage.Condition{condition4})
			require.NoError(t, err)
			insertStagedRule(t, db, &ruleToSave, false)
			insertAppliedRule(t, db, &ruleToSave)

			err = store.DeleteRule(ctx, projID, ruleToDelete.ID)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToSave.ID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple staged and applied rules exist with a matching project filter, mark for delete": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "project-2"})

			ruleToDelete := insertAppliedRuleWithMultipleConditions(t, db, "delete-me", projID, storage.Node)
			ruleToSave := insertStagedRuleWithMultipleConditions(t, db, "save-me", projID, storage.Event, false)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=true`, ruleToDelete.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToSave.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, ruleToDelete.ID))
		},
		"when multiple applied rules exist with a matching project filter, mark for delete": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID, "project-2"})

			ruleType := storage.Node
			ruleToDelete := insertAppliedRuleWithMultipleConditions(t, db, "delete-me", projID, ruleType)
			ruleToSave := insertAppliedRuleWithMultipleConditions(t, db, "save-me", projID, ruleType)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			require.NoError(t, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=true`, ruleToDelete.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND deleted=false`, ruleToSave.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
		"when multiple applied rules exist with a non-matching project filter, do nothing and return NotFoundErr": func(t *testing.T) {
			ctx := context.Background()

			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"wrong-project", "project-2"})

			ruleToDelete := insertAppliedRuleWithMultipleConditions(t, db, "delete-me", projID, storage.Event)
			ruleToSave := insertAppliedRuleWithMultipleConditions(t, db, "save-me", projID, storage.Node)

			err := store.DeleteRule(ctx, projID, ruleToDelete.ID)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToDelete.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1`, ruleToSave.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestApplyStagedRules(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	// No project filter concerns in these tests so safe to re-use context.
	ctx := context.Background()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when there are no staged rules, applied rules are unchanged", func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			rule := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)

			err := store.ApplyStagedRules(ctx)
			require.NoError(t, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules
				WHERE id=$1 AND project_id=project_db_id($2) AND name=$3 AND type=$4`, rule.ID, rule.ProjectID, rule.Name, rule.Type.String()))
		}},
		{"when there are n staged rules marked for update but no applied rules, it creates n applied rules", func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Node
			rule1 := insertStagedRuleWithMultipleConditions(t, db, "rule-1", projID, ruleType, false)
			condition, err := storage.NewCondition(
				[]string{"chef-server-2"}, storage.ChefServer, storage.MemberOf)
			require.NoError(t, err)
			rule2, err := storage.NewRule("new-id-2", projID, "name2", ruleType,
				[]storage.Condition{condition})
			require.NoError(t, err)
			insertStagedRule(t, db, &rule2, false)

			err = store.ApplyStagedRules(ctx)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule1.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1`, rule2.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule1.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule2.ID))
		}},
		{"when all staged rules are marked for delete, there are no applied rules or conditions remaining", func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			ruleType := storage.Node
			ruleToDelete1 := insertAppliedRuleWithMultipleConditions(t, db, "delete-me-1", projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, ruleToDelete1.ID, projID, ruleType, true)

			ruleToDelete2 := insertAppliedRuleWithMultipleConditions(t, db, "delete-me-2", projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, ruleToDelete2.ID, projID, ruleType, true)

			err := store.ApplyStagedRules(ctx)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions`))
		}},
		{"when there are staged rules for update and delete that are a subset of applied rules, update or delete relevant rules", func(t *testing.T) {
			projID := "project-1"
			id1 := "project-1-rule"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			ruleType := storage.Node
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, id1, projID, storage.Node)
			insertStagedRuleWithMultipleConditions(t, db, id1, projID, ruleType, true)

			condition1, err := storage.NewCondition(
				[]string{"chef-server-2"}, storage.ChefServer, storage.Equals)
			require.NoError(t, err)
			rule2, err := storage.NewRule("new-id-2", projID, "name2", ruleType,
				[]storage.Condition{condition1})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule2)

			condition2, err := storage.NewCondition(
				[]string{"tag1", "tag2"}, storage.ChefTag, storage.MemberOf)
			require.NoError(t, err)
			rule2.Conditions = []storage.Condition{condition1, condition2}
			rule2UpdatedName := "this name has been updated"
			rule2UpdatedType := storage.Event
			rule2.Name = rule2UpdatedName
			rule2.Type = rule2UpdatedType
			insertStagedRule(t, db, &rule2, false)

			condition3, err := storage.NewCondition(
				[]string{"role1"}, storage.ChefRole, storage.Equals)
			require.NoError(t, err)
			condition4, err := storage.NewCondition(
				[]string{"Event"}, storage.Environment, storage.Equals)
			require.NoError(t, err)
			condition5, err := storage.NewCondition(
				[]string{"org1", "org2"}, storage.Organization, storage.MemberOf)
			require.NoError(t, err)
			rule3, err := storage.NewRule("new-id-3", projID, "name3", ruleType,
				[]storage.Condition{condition3, condition4, condition5})
			require.NoError(t, err)
			insertAppliedRule(t, db, &rule3)

			err = store.ApplyStagedRules(ctx)
			require.NoError(t, err)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions`))

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND project_id=project_db_id($3) AND type=$4`,
				rule1.ID, rule1.Name, rule1.ProjectID, rule1.Type.String()))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND project_id=project_db_id($3) AND type=$4`,
				rule2.ID, rule2UpdatedName, rule2.ProjectID, rule2UpdatedType.String()))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND project_id=project_db_id($3) AND type=$4`,
				rule3.ID, rule3.Name, rule3.ProjectID, rule3.Type.String()))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule2.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule3.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule1.ID))
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

func TestCreateProject(t *testing.T) {
	projectLimit := 7
	store, db, _, _, _ := testhelpers.SetupTestDBWithLimit(t, projectLimit)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"successfully creates chef-managed project": func(t *testing.T) {
			project := storage.Project{
				ID:     "my-id-1",
				Name:   "name1",
				Type:   storage.ChefManaged,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &project, false)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"successfully creates custom project without supporting policies": func(t *testing.T) {
			project := storage.Project{
				ID:     "my-id-1",
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &project, false)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
		},
		"successfully creates custom project with supporting policies": func(t *testing.T) {
			project := storage.Project{
				ID:     "my-id-1",
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			resp, err := store.CreateProject(ctx, &project, true)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
			assertCount(t, 3, db.QueryRow(
				`SELECT count(*) FROM iam_policies
				 WHERE id IN ('my-id-1-project-owners','my-id-1-project-viewers','my-id-1-project-editors')`))
		},
		"does not create project with duplicate ID": func(t *testing.T) {
			projectID := "my-id-1"
			projectOriginal := storage.Project{
				ID:     projectID,
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &projectOriginal, false)
			require.NoError(t, err)
			require.Equal(t, &projectOriginal, resp)

			projectConflict := storage.Project{
				ID:   projectID,
				Name: "Something Else",
				Type: storage.Custom,
			}
			resp, err = store.CreateProject(ctx, &projectConflict, false)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrConflict, err)
			assert.Nil(t, resp)
		},
		"does not create custom project if max number of custom projects allowed has been reached": func(t *testing.T) {
			for i := 1; i <= projectLimit; i++ {
				projectID := "my-id-" + strconv.Itoa(i)
				project := storage.Project{
					ID:     projectID,
					Name:   "name-" + strconv.Itoa(i),
					Type:   storage.Custom,
					Status: storage.NoRules.String(),
				}
				resp, err := store.CreateProject(ctx, &project, false)
				require.NoError(t, err)
				require.Equal(t, &project, resp)
			}

			oneProjectTooManyID := "my-id-" + strconv.Itoa(projectLimit+1)
			oneProjectTooMany := storage.Project{
				ID:     oneProjectTooManyID,
				Name:   "Something Else",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &oneProjectTooMany, false)
			assert.Nil(t, resp)
			assert.Error(t, err)
			_, correctError := err.(*storage_errors.MaxProjectsExceededError)
			assert.True(t, correctError)
		},
		"does create chef-managed project if max number of custom projects allowed has been reached": func(t *testing.T) {
			for i := 1; i <= projectLimit; i++ {
				projectID := "my-id-" + strconv.Itoa(i)
				project := storage.Project{
					ID:     projectID,
					Name:   "name-" + strconv.Itoa(i),
					Type:   storage.Custom,
					Status: storage.NoRules.String(),
				}
				resp, err := store.CreateProject(ctx, &project, false)
				require.NoError(t, err)
				require.Equal(t, &project, resp)
			}

			chefManagedProjectID := "my-id-" + strconv.Itoa(projectLimit+1)
			chefManagedProject := storage.Project{
				ID:     chefManagedProjectID,
				Name:   "Something Else",
				Type:   storage.ChefManaged,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &chefManagedProject, false)
			require.NoError(t, err)
			require.Equal(t, &chefManagedProject, resp)
		},
	}

	for name, test := range cases {
		insertTestRole(t, db, v2.ViewerRoleID, "viewer", []string{"any"}, []string{})
		insertTestRole(t, db, v2.EditorRoleID, "editor", []string{"any"}, []string{})
		insertTestRole(t, db, v2.ProjectOwnerRoleID, "project owner", []string{"any"}, []string{})
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestUpdateProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"successfully updates existing custom project": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.UpdateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"successfully updates existing custom project with a project filter": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})
			resp, err := store.UpdateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"successfully updates existing custom project with a project filter of *": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})

			resp, err := store.UpdateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"returns ErrNotFound if it doesn't exist": func(t *testing.T) {
			ctx := context.Background()
			project := storage.Project{
				ID:     "not-found",
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.UpdateProject(ctx, &project)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, resp)
		},
		"returns ErrNotFound if the project exists but does not have a project in the project filter list": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			ctx = insertProjectsIntoContext(ctx, []string{"wrong", "projects"})

			resp, err := store.UpdateProject(ctx, &project)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestGetProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when no project exists, returns not found error", func(t *testing.T) {
			ctx := context.Background()
			p, err := store.GetProject(ctx, "id-that-wont-be-found")
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, p)
		}},
		{"when a chef-managed project exists, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:     "foo",
				Name:   "my foo project",
				Type:   storage.ChefManaged,
				Status: storage.NoRules.String(),
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists with no rules, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:     "foo",
				Name:   "my foo project",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists with only staged rules, status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, false)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with only applied rules, status is applied", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p.ID, storage.Node)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.Applied.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with staged and applied rules (including some deleted rules), status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p.ID, storage.Node)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with deleted rules and applied rules, status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p.ID, storage.Node)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with only deleted rules, status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, true)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with a project filter of *, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:     "foo",
				Name:   "my foo project",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists but the project filter does not overlap, return NotFoundErr", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"wrong", "project"})

			p, err := store.GetProject(ctx, "foo")
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, p)
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

func TestDeleteProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"returns project not found error with empty database", func(t *testing.T) {
			ctx := context.Background()
			err := store.DeleteProject(ctx, "test-project")
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		}},
		{"returns project not found with several projects in database", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			err := store.DeleteProject(ctx, "test-project")
			assert.Equal(t, storage_errors.ErrNotFound, err)
		}},
		{"when a policy contains a single statement and that statement contains a single project, on project deletion, the statement and policy are deleted", func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			insertStatementProject(t, db, sID0, project1.ID)

			polIDOther := insertTestPolicy(t, db, "testpolicy2")
			sID0Other := insertTestStatement(t, db,
				polIDOther, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1Other := insertTestStatement(t, db,
				polIDOther, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0Other, project2.ID)
			insertStatementProject(t, db, sID1Other, project2.ID)

			assertPolicyChange(t, store, func() {
				err = store.DeleteProject(ctx, project1.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, project1.ID))
			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))

			assertOne(t, db.QueryRow(policyWithID, polIDOther))
			assertOne(t, db.QueryRow(statementWithID, sID0Other))
			assertOne(t, db.QueryRow(statementWithID, sID1Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID0Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1Other))
		}},
		{"when a policy contains multiple statements and those statements contain a single project, on project deletion, the statement and policy are deleted", func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db,
				polID, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0, project1.ID)
			insertStatementProject(t, db, sID1, project1.ID)

			polIDOther := insertTestPolicy(t, db, "testpolicy2")
			sID0Other := insertTestStatement(t, db,
				polIDOther, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1Other := insertTestStatement(t, db,
				polIDOther, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0Other, project2.ID)
			insertStatementProject(t, db, sID1Other, project2.ID)

			assertPolicyChange(t, store, func() {
				err = store.DeleteProject(ctx, project1.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, project1.ID))
			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))
			assertEmpty(t, db.QueryRow(statementWithID, sID1))

			assertOne(t, db.QueryRow(policyWithID, polIDOther))
			assertOne(t, db.QueryRow(statementWithID, sID0Other))
			assertOne(t, db.QueryRow(statementWithID, sID1Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID0Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1Other))
		}},
		{"when a policy contains multiple statements and those statements contain different projects, on one project deletion, one statement is deleted and the policy remains", func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db,
				polID, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0, project1.ID)
			insertStatementProject(t, db, sID1, project2.ID)

			polIDOther := insertTestPolicy(t, db, "testpolicy2")
			sID0Other := insertTestStatement(t, db,
				polIDOther, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1Other := insertTestStatement(t, db,
				polIDOther, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0Other, project2.ID)
			insertStatementProject(t, db, sID1Other, project2.ID)

			assertPolicyChange(t, store, func() {
				err = store.DeleteProject(ctx, project1.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, project1.ID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))

			assertOne(t, db.QueryRow(policyWithID, polID))
			assertOne(t, db.QueryRow(statementWithID, sID1))
			assertOne(t, db.QueryRow(statementWithID, sID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1))

			assertOne(t, db.QueryRow(policyWithID, polIDOther))
			assertOne(t, db.QueryRow(statementWithID, sID0Other))
			assertOne(t, db.QueryRow(statementWithID, sID1Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID0Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1Other))
		}},
		{"deletes project with one project in database", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
		}},
		{"when the project is already deleted but is in the graveyard, the function succeeds", func(t *testing.T) {
			ctx := context.Background()
			projID := "test-project"
			_, err := db.Exec(`INSERT INTO iam_projects_graveyard (id) values ($1)`, projID)
			require.NoError(t, err)

			err = store.DeleteProject(ctx, projID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, projID))
		}},
		{"deletes project with several projects in database", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		}},
		{"deletes project with several projects in database with a project filter", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"foo", "test-project"})

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		}},
		{"deletes project with several projects in database with a project filter of *", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		}},
		{"returns 'not found' when the project filter excludes the project in question", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"my-id-1", "my-id-2"})

			err := store.DeleteProject(ctx, "test-project")
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_projects`))
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

func TestListProjects(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when no projects exists, returns empty list", func(t *testing.T) {
			ctx := context.Background()
			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			assert.Empty(t, ps)
		}},
		{"when multiple projects exist, filter based on projects lists", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			p2 := insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertTestProject(t, db, "baz", "my baz project", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{&p1, &p2}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"when multiple projects exist, returns all projects when * filter passed", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			p2 := insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			p3 := insertTestProject(t, db, "baz", "my baz project", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{&p1, &p2, &p3}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status for projects for none, staged, and applied individually", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p1.ID, storage.Node, false)
			p2 := insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertAppliedRuleWithMultipleConditions(t, db, "staged-rule-2", p2.ID, storage.Node)
			p3 := insertTestProject(t, db, "baz", "my baz project", storage.Custom)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			p2.Status = storage.Applied.String()
			p3.Status = storage.NoRules.String()
			expectedProjects := []*storage.Project{&p1, &p2, &p3}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with both staged and applied rules", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p1.ID, storage.Node, false)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p1.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p1.ID, storage.Node)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with staged new rules, staged deletes, and applied rules", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p1.ID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p1.ID, storage.Node)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with both staged deletes applied rules", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", p1.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-2", p1.ID, storage.Node)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with staged deletes", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", p1.ID, storage.Node, true)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
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

func TestCreateRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})

	cases := map[string]func(*testing.T){
		"successfully creates role with NO actions": func(t *testing.T) {
			// While disallowed at a higher level, no actions are OK at the DB level
			role := storage.Role{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{},
				Projects: []string{},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"successfully creates role with SOME actions": func(t *testing.T) {
			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"successfully creates a role with a project": func(t *testing.T) {
			project := storage.Project{
				ID:   "my-id-1",
				Name: "name1",
				Type: storage.Custom,
			}

			_, err := store.CreateProject(ctx, &project, false)
			require.NoError(t, err)

			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{"my-id-1"},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"successfully creates role with multiple projects": func(t *testing.T) {
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{project1.ID, project2.ID},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"does not creates role with duplicate ID": func(t *testing.T) {
			roleID := "my-id-5"
			role := storage.Role{
				ID:      roleID,
				Name:    "name4a",
				Type:    storage.Custom,
				Actions: []string{"action1", "action2"},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			role2 := storage.Role{
				ID:      roleID,
				Name:    "name4b",
				Type:    storage.Custom,
				Actions: []string{"action3", "action4"},
			}
			assertNoPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role2, false)
				assert.Error(t, err)
				assert.Equal(t, storage_errors.ErrConflict, err)
				assert.Nil(t, resp)
			})
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListRoles(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"successfully returns empty list when the database is empty": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.Nil(t, resp)
		},
		"successfully returns list with role when the database has one role": func(t *testing.T) {
			ctx := context.Background()
			roles := []*storage.Role{{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1"},
				Projects: []string{},
			}}
			insertTestRole(t, db, roles[0].ID, roles[0].Name, roles[0].Actions, roles[0].Projects)

			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, roles, resp)
		},
		"successfully returns list with roles when the database has several roles": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-2",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-3",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, roles, resp)
		},
		"successfully returns filtered list when rows intersect with projects filter": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-0",
				Name:     "name0",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}, {
				ID:       "my-id-3",
				Name:     "name3",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project3.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name4",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID, project2.ID})
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			expected := []*storage.Role{
				roles[1],
				roles[2],
				roles[4],
			}
			assert.ElementsMatch(t, expected, resp)
		},
		"successfully returns all projects when filter is *": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-3",
				Name:     "name3",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name4",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project3.ID},
			}, {
				ID:       "my-id-5",
				Name:     "name5",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, roles, resp)
		},
		"successfully returns roles with unassigned projects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-2",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-3",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID})
			resp, err := store.ListRoles(ctx)

			expected := []*storage.Role{
				{
					ID:       "my-id-2",
					Name:     "name1",
					Type:     storage.Custom,
					Actions:  []string{"action1", "action2"},
					Projects: []string{},
				},
			}
			require.NoError(t, err)
			assert.ElementsMatch(t, expected, resp)
		},
		"returns empty list if projects filter excludes all objects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-2",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-3",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{"some-other-project"})
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Role{}, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestGetRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"returns policy not found error with empty database": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.GetRole(ctx, "fake-id")
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"returns policy not found error with database that has several roles": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.GetRole(ctx, "fake-id")
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"successfully returns appropriate role when the database has one role": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})

			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the database has several roles": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the project filter intersects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID, project2.ID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the project filter is *": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the project filter is * and role has no projects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the role has no projects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"returns NotFound when no project filter intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID})
			resp, err := store.GetRole(ctx, "my-id-1")

			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestReset(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			require.NoError(t, store.Reset(ctx))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		},
		"empty database, run twice": func(t *testing.T) {
			require.NoError(t, store.Reset(ctx))
			require.NoError(t, store.Reset(ctx))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		},
		"non-empty database, a policy with two statements and members": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "firstpolicy")
			insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			insertTestStatement(t, db,
				polID, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertTestPolicyMember(t, db, polID, "user:local:albertine")
			insertTestPolicyMember(t, db, polID, "user:local:othermember")

			require.NoError(t, store.Reset(ctx))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			assertEmpty(t, db.QueryRow(membersCount))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestDeleteRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"returns role not found error with empty database": func(t *testing.T) {
			ctx := context.Background()
			err := store.DeleteRole(ctx, "test-role")
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"returns role not found with several roles in database": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			err = store.DeleteRole(ctx, "test-role")
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"deletes role with one role in database": func(t *testing.T) {
			ctx := context.Background()
			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})

			err := store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
		},
		"deletes role with several roles in database": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"when statements contains a role and no actions and are the last statements in a policy, on role deletion the policy is deleted": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			roleDeleted := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			roleRemaining := insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db, polID, "allow", roleDeleted.ID, []string{}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db, polID, "deny", roleDeleted.ID, []string{}, []string{"compliance:profiles"})

			polIDWrongRole := insertTestPolicy(t, db, "testpolicy2")
			sID0WrongRole := insertTestStatement(t, db, polIDWrongRole, "allow", roleRemaining.ID, []string{}, []string{"iam:users"})
			sID1WrongRole := insertTestStatement(t, db, polIDWrongRole, "deny", roleRemaining.ID, []string{}, []string{"compliance:profiles"})

			assertPolicyChange(t, store, func() {
				err = store.DeleteRole(ctx, roleDeleted.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, roleDeleted.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_roles`))

			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))
			assertEmpty(t, db.QueryRow(statementWithID, sID1))

			assertOne(t, db.QueryRow(policyWithID, polIDWrongRole))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID0WrongRole, roleRemaining.ID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID1WrongRole, roleRemaining.ID))
		},
		"when one statement contains a role and as well as actions but one statement contains no actions, on role deletion there is only one modified statement remaining": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			roleDeleted := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			roleRemaining := insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db, polID, "allow", roleDeleted.ID, []string{}, []string{"iam:users"})
			actions := []string{"compliance:profiles:download"}
			sID1 := insertTestStatement(t, db, polID, "deny", roleDeleted.ID, actions, []string{"compliance:profiles"})

			polIDWrongRole := insertTestPolicy(t, db, "testpolicy2")
			sID0WrongRole := insertTestStatement(t, db, polIDWrongRole, "allow", roleRemaining.ID, []string{}, []string{"iam:users"})
			sID1WrongRole := insertTestStatement(t, db, polIDWrongRole, "deny", roleRemaining.ID, []string{}, []string{"compliance:profiles"})

			assertPolicyChange(t, store, func() {
				err = store.DeleteRole(ctx, roleDeleted.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, roleDeleted.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_roles`))

			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND actions=$2 AND role_id IS NULL`, sID1, pq.Array(actions)))

			assertOne(t, db.QueryRow(policyWithID, polIDWrongRole))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID0WrongRole, roleRemaining.ID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID1WrongRole, roleRemaining.ID))
		},
		"deletes role with several roles in database when projects filter has intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project3.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID, project2.ID})
			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"deletes role with no projects assigned when projects filter has intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID})
			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"deletes role with several roles in database when projects filter is *": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project3.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"returns NotFound when project filter has no intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project3.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project2.ID})
			err = store.DeleteRole(ctx, role.ID)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestUpdateRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	nonexistingRole := storage.Role{
		ID:      "nonexistant",
		Name:    "name",
		Actions: []string{"actionx"},
	}

	cases := map[string]func(*testing.T){
		"returns role not found error with empty database": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			role, err := store.UpdateRole(ctx, &nonexistingRole)

			assert.Nil(t, role)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"returns role not found with several roles in database": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			role, err := store.UpdateRole(ctx, &nonexistingRole)

			assert.Nil(t, role)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"updates name of a role": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     "new name",
				Actions:  dbRole.Actions,
				Projects: []string{},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, "new name", updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, dbRole.Actions, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"updates action of a role": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project1.ID},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project1.ID}, updatedRole.Projects)
		},
		"updates the projects of a role": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			project4 := storage.Project{
				ID:   "project-4",
				Name: "name4",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project4, false)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project4.ID})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project2.ID, project3.ID, project4.ID},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project2.ID, project3.ID, project4.ID}, updatedRole.Projects)
		},
		"updates the projects of a role to be empty": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project2.ID})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"fails to update when a project filter is specified with no intersection": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project2.ID},
			}
			ctx = insertProjectsIntoContext(ctx, []string{project1.ID})
			updatedRole, err := store.UpdateRole(ctx, &r)

			assert.Nil(t, updatedRole)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"updates successfully when a project filter is specified with an intersection": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, false)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID, project3.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project2.ID},
			}
			ctx = insertProjectsAndSubjectsIntoContext(context.Background(), []string{project2.ID, project1.ID}, []string{SuperuserSubject})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project2.ID}, updatedRole.Projects)
		},
		"updates successfully when a project filter is *": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project1.ID},
			}
			ctx = insertProjectsAndSubjectsIntoContext(context.Background(), []string{v2.AllProjectsExternalID}, []string{SuperuserSubject})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project1.ID}, updatedRole.Projects)
		},
		"updates successfully when a role has no projects": func(t *testing.T) {
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{},
			}
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{v2.UnassignedProjectID}, []string{SuperuserSubject})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"updates the projects of a role to contain projects from empty": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, false)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, false)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project1.ID, project2.ID},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project1.ID, project2.ID}, updatedRole.Projects)
		},
		"successfully runs even if nothing is actually changed": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:      dbRole.ID,
				Name:    dbRole.Name,
				Actions: dbRole.Actions,
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, dbRole.Actions, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"successfully updates multiple properties at once": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:      dbRole.ID,
				Name:    "new name",
				Actions: []string{"newaction"},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, "new name", updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
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

func TestEnsureNoProjectsMissing(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when there are no projects, returns ProjectsMissingError", func(t *testing.T) {
			err := store.EnsureNoProjectsMissing(ctx, []string{"missing"})
			assert.Error(t, err)
			_, correctError := err.(*projectassignment.ProjectsMissingError)
			assert.True(t, correctError)
		}},
		{"when some projects don't exist, returns ProjectsMissingError", func(t *testing.T) {
			insertTestProject(t, db, "proj0", "test project 0", storage.Custom)
			insertTestProject(t, db, "proj1", "test project 1", storage.Custom)
			err := store.EnsureNoProjectsMissing(ctx, []string{"proj0", "missing"})
			assert.Error(t, err)
			_, correctError := err.(*projectassignment.ProjectsMissingError)
			assert.True(t, correctError)
		}},
		{"when all projects exist, return nil", func(t *testing.T) {
			insertTestProject(t, db, "proj0", "test project 0", storage.Custom)
			insertTestProject(t, db, "proj1", "test project 1", storage.Custom)
			err := store.EnsureNoProjectsMissing(ctx, []string{"proj0", "proj1"})
			assert.NoError(t, err)
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

func TestFetchAppliedRulesByProjectIDs(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()
	cases := map[string]func(*testing.T){
		"when no rules or projects exist, returns an empty map": func(t *testing.T) {
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			assert.Equal(t, resp, map[string][]*storage.Rule{})
		},
		"when a project exists without rules, returns an empty map": func(t *testing.T) {
			insertTestProject(t, db, "project-1", "let's go jigglypuff - topsecret", storage.Custom)
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, resp, map[string][]*storage.Rule{})
		},
		"when a project exists with staged rules only, returns an empty map": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Node, false)
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, resp, map[string][]*storage.Rule{})
		},
		"when a project exists with applied rules only, returns a map of applied rules": func(t *testing.T) {
			projID := "project-1"
			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID, storage.Node)
			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, 1, len(resp))
			require.ElementsMatch(t, resp[projID], []*storage.Rule{rule1, rule2})
		},
		"when multiple projects exists with staged and applied rules, returns a map of applied rules": func(t *testing.T) {
			projID1 := "project-1"
			insertTestProject(t, db, projID1, "let's go jigglypuff", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", projID1, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", projID1, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-only", projID1, storage.Node, false)
			rule1 := insertAppliedRuleWithMultipleConditions(t, db, "rule-1", projID1, storage.Node)
			rule2 := insertAppliedRuleWithMultipleConditions(t, db, "rule-2", projID1, storage.Node)

			projID2 := "project-2"
			insertTestProject(t, db, projID2, "let's go pikachu", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-4", projID2, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-only-2", projID2, storage.Node, false)
			rule3 := insertAppliedRuleWithMultipleConditions(t, db, "rule-3", projID2, storage.Node)
			rule4 := insertAppliedRuleWithMultipleConditions(t, db, "rule-4", projID2, storage.Node)

			resp, err := store.FetchAppliedRulesByProjectIDs(ctx)
			require.NoError(t, err)
			require.Equal(t, 2, len(resp))
			require.ElementsMatch(t, resp[projID1], []*storage.Rule{rule1, rule2})
			require.ElementsMatch(t, resp[projID2], []*storage.Rule{rule3, rule4})
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func assertProjectsMatch(t *testing.T, db *testhelpers.TestDB, project storage.Project) {
	t.Helper()
	dbProject := storage.Project{}
	err := db.QueryRow(`SELECT query_project($1, '{}');`, project.ID).Scan(&dbProject)
	require.NoError(t, err)
	assert.Equal(t, project, dbProject)
}

func assertRolesMatch(t *testing.T, db *testhelpers.TestDB, role storage.Role) {
	t.Helper()
	dbRole := storage.Role{}
	require.NoError(t, db.QueryRow("SELECT query_role($1)", role.ID).Scan(&dbRole))
	assert.Equal(t, role, dbRole)
}

func assertEmpty(t *testing.T, row *sql.Row) {
	t.Helper()
	assertCount(t, 0, row)
}

func assertOne(t *testing.T, row *sql.Row) {
	t.Helper()
	assertCount(t, 1, row)
}

func assertCount(t *testing.T, expected int, row *sql.Row) {
	t.Helper()
	require.NotNil(t, row)
	var count int
	require.NoError(t, row.Scan(&count))
	assert.Equal(t, expected, count)
}

func assertMembers(t *testing.T, db *testhelpers.TestDB, policyID string, members []storage.Member) {
	t.Helper()
	for _, member := range members {
		assertOne(t, db.QueryRow("SELECT count(*) FROM iam_policy_members WHERE policy_id=policy_db_id($1) AND member_id=member_db_id($2)", policyID, member.Name))
	}
}

func assertNoMembers(t *testing.T, db *testhelpers.TestDB, policyID string, members []storage.Member) {
	t.Helper()
	assertEmpty(t, db.QueryRow(policyMembersByPolicyID, policyID))
	for _, member := range members {
		assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, member.Name))
	}
}

func assertPolicy(t *testing.T, expectedPolicy, returnedPolicy *storage.Policy) {
	assert.Equal(t, expectedPolicy.ID, returnedPolicy.ID)
	assert.Equal(t, expectedPolicy.Name, returnedPolicy.Name)
	assert.Equal(t, expectedPolicy.Type, returnedPolicy.Type)
	assert.ElementsMatch(t, expectedPolicy.Projects, returnedPolicy.Projects, "projects match")
	assert.ElementsMatch(t, expectedPolicy.Members, returnedPolicy.Members, "members match")
	assert.ElementsMatch(t, expectedPolicy.Statements, returnedPolicy.Statements, "statements match")
}

func assertPolicies(t *testing.T, expectedPolicies, returnedPolicies []*storage.Policy) {
	t.Helper()

	require.Equal(t, len(expectedPolicies), len(returnedPolicies))

	// ensure expected and returned policies are in the same order
	if len(returnedPolicies) > 1 {
		sort.Slice(expectedPolicies, func(i, j int) bool {
			return expectedPolicies[i].ID < expectedPolicies[j].ID
		})
		sort.Slice(returnedPolicies, func(i, j int) bool {
			return returnedPolicies[i].ID < returnedPolicies[j].ID
		})
	}

	for i := 0; i < len(returnedPolicies); i++ {
		// confirm statements of sorted policies match
		assert.ElementsMatch(t, expectedPolicies[i].Statements, returnedPolicies[i].Statements)
		// confirm projects of sorted policies match
		assert.ElementsMatch(t, expectedPolicies[i].Projects, returnedPolicies[i].Projects)

		// then, empty statements so their potentially mismatched order
		// doesn't cause ElementsMatch on policies to fail
		// see related issue: https://github.com/stretchr/testify/issues/676
		expectedPolicies[i].Statements = []storage.Statement{}
		returnedPolicies[i].Statements = []storage.Statement{}
		expectedPolicies[i].Projects = []string{}
		returnedPolicies[i].Projects = []string{}
	}

	assert.ElementsMatch(t, expectedPolicies, returnedPolicies)
}

func genSimpleID(t *testing.T, p *prng.Prng) string {
	t.Helper()
	faker := faker.NewWithSeed(p)
	return faker.Lorem().Word() + "-" + faker.Lorem().Word()
}

func genMember(t *testing.T, name string) storage.Member {
	t.Helper()
	member, err := storage.NewMember(name)
	require.NoError(t, err)
	return member
}

func genRole(t *testing.T, id string, name string, actions []string, projects []string) storage.Role {
	t.Helper()
	role, err := storage.NewRole(id, name, storage.Custom, actions, projects)
	require.NoError(t, err)
	return *role
}

func insertTestPolicy(t *testing.T, db *testhelpers.TestDB, policyName string) string {
	row := db.QueryRow(
		"INSERT INTO iam_policies (id, name) VALUES (uuid_generate_v4(), $1) RETURNING id",
		policyName)
	require.NotNil(t, row)
	var polID string
	require.NoError(t, row.Scan(&polID))
	return polID
}

// pass "" as roleID if you do not wish to populate a role
func insertTestStatement(t *testing.T, db *testhelpers.TestDB,
	policyID, effect, roleID string, actions, resources []string) int {

	var dbID int
	if roleID != "" {
		row := db.QueryRow(`
		INSERT INTO iam_statements (policy_id, effect, role_id, actions, resources)
			VALUES (policy_db_id($1), $2::iam_effect, role_db_id($3), $4, $5) RETURNING db_id`,
			policyID, effect, roleID, pq.Array(actions), pq.Array(resources))
		require.NoError(t, row.Scan(&dbID))

		assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE
			policy_id=policy_db_id($1) AND
			effect=$2 AND
			actions=$3 AND
			resources=$4 AND
			role_id=role_db_id($5)`,
			policyID, effect, pq.Array(actions), pq.Array(resources), roleID))
	} else {
		row := db.QueryRow(`
		INSERT INTO iam_statements (policy_id, effect, actions, resources)
			VALUES (policy_db_id($1), $2::iam_effect, $3, $4) RETURNING db_id`,
			policyID, effect, pq.Array(actions), pq.Array(resources))
		require.NoError(t, row.Scan(&dbID))

		assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE
			policy_id=policy_db_id($1) AND
			effect=$2 AND
			actions=$3 AND
			resources=$4 AND
			role_id IS NULL`,
			policyID, effect, pq.Array(actions), pq.Array(resources)))
	}
	return dbID
}

// Will fail on conflict with existing name.
func insertTestPolicyMember(t *testing.T, db *testhelpers.TestDB, polID string, memberName string) storage.Member {
	member := genMember(t, memberName)

	_, err := db.Exec(`INSERT INTO iam_members (name) VALUES ($1)`, member.Name)
	require.NoError(t, err)
	_, err = db.Exec(`INSERT INTO iam_policy_members (policy_id, member_id) VALUES (policy_db_id($1), member_db_id($2))`, polID, member.Name)
	require.NoError(t, err)

	return member
}

func insertTestRole(t *testing.T,
	db *testhelpers.TestDB, id string, name string, actions []string, projects []string) storage.Role {

	role := genRole(t, id, name, actions, projects)

	row := db.QueryRow(`INSERT INTO iam_roles (id, name, type, actions) VALUES ($1, $2, $3, $4)
	RETURNING db_id`,
		role.ID, role.Name, role.Type.String(), pq.Array(role.Actions))
	var dbID string
	require.NoError(t, row.Scan(&dbID))

	if len(projects) > 0 {
		_, err := db.Exec(`INSERT INTO iam_role_projects (role_id, project_id)
		SELECT $1, db_id FROM iam_projects WHERE id=ANY($2)`,
			dbID, pq.Array(role.Projects))
		require.NoError(t, err)
	}

	return role
}
func insertTestProject(
	t *testing.T,
	db *testhelpers.TestDB,
	id string,
	name string,
	projType storage.Type) storage.Project {
	t.Helper()
	// the status is not actually used. we are only using NewProject for the id
	// and name validation and to have a storage.Project object to return.
	proj, err := storage.NewProject(id, name, projType, storage.NoRules)
	require.NoError(t, err)

	_, err = db.Exec(`INSERT INTO iam_projects (id, name, type) values ($1, $2, $3)`, id, name, projType.String())
	require.NoError(t, err)
	return proj
}

func insertPolicyProject(t *testing.T, db *testhelpers.TestDB, policyID string, projectId string) {
	t.Helper()
	_, err := db.Exec("INSERT INTO iam_policy_projects (policy_id, project_id) VALUES (policy_db_id($1), project_db_id($2))",
		policyID, projectId)
	require.NoError(t, err)
}

func insertStatementProject(t *testing.T, db *testhelpers.TestDB, statementID int, projectId string) {
	t.Helper()
	_, err := db.Exec(`
			INSERT INTO iam_statement_projects (statement_id, project_id) VALUES ($1, project_db_id($2));`,
		statementID, projectId)
	require.NoError(t, err)
}

func insertAppliedRule(t *testing.T, db *testhelpers.TestDB, rule *storage.Rule) {
	t.Helper()
	row := db.QueryRow(`INSERT INTO iam_project_rules (id, project_id, name, type)
		VALUES ($1, project_db_id($2), $3, $4) RETURNING db_id`,
		rule.ID, rule.ProjectID, rule.Name, rule.Type.String())
	var dbID string
	require.NoError(t, row.Scan(&dbID))
	for _, c := range rule.Conditions {
		_, err := db.Exec(`
			INSERT INTO iam_rule_conditions (rule_db_id, value, attribute, operator) VALUES ($1, $2, $3, $4);`,
			dbID, pq.Array(c.Value), c.Attribute.String(), c.Operator.String())
		require.NoError(t, err)
	}

	assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_project_rules WHERE id=$1 AND name=$2 AND type=$3
		AND project_id=project_db_id($4)`,
		rule.ID, rule.Name, rule.Type.String(), rule.ProjectID))
	assertCount(t, len(rule.Conditions), db.QueryRow(`SELECT count(*) FROM iam_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_project_rules r WHERE r.id=$1)`, rule.ID))
	rule.Status = Applied
}

func insertStagedRule(t *testing.T, db *testhelpers.TestDB, rule *storage.Rule, deleted bool) {
	rule.Status = "staged"
	t.Helper()
	row := db.QueryRow(`INSERT INTO iam_staged_project_rules (id, project_id, name, type, deleted)
		(SELECT $1, db_id, $3, $4, $5 FROM iam_projects WHERE id=$2)
		RETURNING db_id`,
		rule.ID, rule.ProjectID, rule.Name, rule.Type.String(), deleted)
	var dbID string
	require.NoError(t, row.Scan(&dbID))
	for _, c := range rule.Conditions {
		_, err := db.Exec(`
			INSERT INTO iam_staged_rule_conditions (rule_db_id, value, attribute, operator) VALUES ($1, $2, $3, $4);`,
			dbID, pq.Array(c.Value), c.Attribute.String(), c.Operator.String())
		require.NoError(t, err)
	}

	assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_staged_project_rules WHERE id=$1 AND name=$2 AND type=$3 AND project_id=project_db_id($4)`,
		rule.ID, rule.Name, rule.Type.String(), rule.ProjectID))
	assertCount(t, len(rule.Conditions), db.QueryRow(`SELECT count(*) FROM iam_staged_rule_conditions WHERE rule_db_id=(SELECT r.db_id FROM iam_staged_project_rules r WHERE r.id=$1)`, rule.ID))
}

func insertDeletedStagedRule(t *testing.T, db *testhelpers.TestDB, rule *storage.Rule) {
	t.Helper()
	insertStagedRule(t, db, rule, true)
}

func insertAppliedRuleWithMultipleConditions(t *testing.T, db *testhelpers.TestDB, id, projID string, ruleType storage.RuleType) *storage.Rule {
	t.Helper()
	rule := createRuleObjectWithMultipleConditions(t, id, projID, ruleType, Applied, false)
	insertAppliedRule(t, db, &rule)
	return &rule
}

func insertStagedRuleWithMultipleConditions(t *testing.T, db *testhelpers.TestDB, id, projID string, ruleType storage.RuleType, deleted bool) *storage.Rule {
	t.Helper()
	rule := createRuleObjectWithMultipleConditions(t, id, projID, ruleType, Staged, deleted)
	insertStagedRule(t, db, &rule, deleted)
	return &rule
}

func createRuleObjectWithMultipleConditions(t *testing.T, id, projID string, ruleType storage.RuleType, status string, deleted bool) storage.Rule {
	t.Helper()
	condition1, err := storage.NewCondition(
		[]string{"chef-server-1"}, storage.ChefServer, storage.MemberOf)
	require.NoError(t, err)
	condition2, err := storage.NewCondition(
		[]string{"org1", "org2", "org3"}, storage.Organization, storage.MemberOf)
	require.NoError(t, err)
	condition3, err := storage.NewCondition(
		[]string{"chef-server-2"}, storage.ChefServer, storage.Equals)
	require.NoError(t, err)
	rule, err := storage.NewRule(id, projID, "name", ruleType,
		[]storage.Condition{condition1, condition2, condition3})
	require.NoError(t, err)
	rule.Status = status
	return rule
}

func insertProjectsAndSubjectsIntoContext(ctx context.Context, projects []string, subjects []string) context.Context {
	return auth_context.NewOutgoingContext(auth_context.NewContext(ctx, subjects, projects, "res", "act", "v2.1"))
}

func insertProjectsIntoContext(ctx context.Context, projects []string) context.Context {
	return insertProjectsAndSubjectsIntoContext(ctx, projects, []string{})
}

func assertPolicyChange(t *testing.T, store storage.Storage, f func()) {
	t.Helper()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	notifier, err := store.GetPolicyChangeNotifier(ctx)
	require.NoError(t, err)

	before, err := store.GetPolicyChangeID(context.Background())
	require.NoError(t, err)
	f()
	after, err := store.GetPolicyChangeID(context.Background())
	require.NoError(t, err)
	require.NotEqual(t, before, after)
	<-notifier.C()
}

func assertNoPolicyChange(t *testing.T, store storage.Storage, f func()) {
	t.Helper()

	before, err := store.GetPolicyChangeID(context.Background())
	require.NoError(t, err)
	f()
	after, err := store.GetPolicyChangeID(context.Background())
	require.NoError(t, err)
	assert.Equal(t, before, after)
}
