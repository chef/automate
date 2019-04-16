package postgres_test

import (
	"context"
	"database/sql"
	"fmt"
	"math/rand"
	"net/url"
	"os"
	"sort"
	"testing"

	"github.com/jaswdr/faker"
	"github.com/lib/pq"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	v2 "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/prng"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/components/authz-service/storage/v2/postgres"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
)

const resetDatabaseStatement = `DROP SCHEMA public CASCADE;
CREATE SCHEMA public;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;`

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

type testDB struct {
	*sql.DB
}

func TestGetPolicy(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()
	ctx := context.Background()

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			resp, err := store.GetPolicy(ctx, genSimpleID(t, prngSeed))
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"policy with projects": func(t *testing.T) {
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
			// Note (sr): Is it ugly to put the SQL statement in here, with no
			// variables or any helper methods? Sure! It also helps isolate this test
			// case from the others. Let's refactor this if can't bear it anymore, but
			// maybe roll with it for now...
			sID0 := genUUID(t)
			sID1 := genUUID(t)
			polID := genSimpleID(t, prngSeed)

			_, err := db.Exec(`
		  WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
		    VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
		           ($2, 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
		       policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
		  INSERT INTO iam_policy_statements (policy_id, statement_id)
		    (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)
			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resp, err := store.GetPolicy(ctx, polID)
			require.NoError(t, err)
			pol := storage.Policy{
				ID:      polID,
				Name:    "testpolicy",
				Members: []storage.Member{member},
				Type:    storage.Custom,
				Statements: []storage.Statement{
					{
						ID:        sID0,
						Effect:    storage.Allow,
						Resources: []string{"iam:users"},
						Actions:   []string{"iam:users:delete", "iam:users:create"},
						Projects:  []string{},
					},
					{
						ID:        sID1,
						Effect:    storage.Deny,
						Resources: []string{"compliance:profiles"},
						Actions:   []string{"compliance:profiles:download", "compliance:profiles:delete"},
						Projects:  []string{},
					},
				},
			}
			assertPolicy(t, &pol, resp)
		},
		"policy with two statements, among other policy": func(t *testing.T) {
			_, err := db.Exec(`
		  WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
		    VALUES (uuid_generate_v4(), 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
		           (uuid_generate_v4(), 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
		       policy AS (INSERT INTO iam_policies (id, name) VALUES (uuid_generate_v4(), 'otherpolicy') RETURNING id)
		  INSERT INTO iam_policy_statements (policy_id, statement_id)
		    (SELECT policy.id, statement.id FROM policy, statement)`)
			require.NoError(t, err)

			sID0 := genUUID(t)
			sID1 := genUUID(t)
			polID := genSimpleID(t, prngSeed)

			_, err = db.Exec(`
		  WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
		    VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
		           ($2, 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
		       policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
		  INSERT INTO iam_policy_statements (policy_id, statement_id)
		    (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)
			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resp, err := store.GetPolicy(ctx, polID)
			require.NoError(t, err)
			pol := storage.Policy{
				ID:      polID,
				Name:    "testpolicy",
				Members: []storage.Member{member},
				Type:    storage.Custom,
				Statements: []storage.Statement{
					{
						ID:        sID0,
						Effect:    storage.Allow,
						Resources: []string{"iam:users"},
						Actions:   []string{"iam:users:delete", "iam:users:create"},
						Projects:  []string{},
					},
					{
						ID:        sID1,
						Effect:    storage.Deny,
						Resources: []string{"compliance:profiles"},
						Actions:   []string{"compliance:profiles:download", "compliance:profiles:delete"},
						Projects:  []string{},
					},
				},
			}
			assertPolicy(t, &pol, resp)
		},
		"policy with two statements and three members, among other policy": func(t *testing.T) {
			_, err := db.Exec(`
		  WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
		    VALUES (uuid_generate_v4(), 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
		           (uuid_generate_v4(), 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
		       policy AS (INSERT INTO iam_policies (id, name) VALUES (uuid_generate_v4(), 'otherpolicy') RETURNING id)
		  INSERT INTO iam_policy_statements (policy_id, statement_id)
		    (SELECT policy.id, statement.id FROM policy, statement)`)
			require.NoError(t, err)

			sID0 := genUUID(t)
			sID1 := genUUID(t)
			polID := genSimpleID(t, prngSeed)
			_, err = db.Exec(`
		  WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
		    VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
		           ($2, 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
		       policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
		  INSERT INTO iam_policy_statements (policy_id, statement_id)
		    (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)
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
						ID:        sID0,
						Effect:    storage.Allow,
						Resources: []string{"iam:users"},
						Actions:   []string{"iam:users:delete", "iam:users:create"},
						Projects:  []string{},
					},
					{
						ID:        sID1,
						Effect:    storage.Deny,
						Resources: []string{"compliance:profiles"},
						Actions:   []string{"compliance:profiles:download", "compliance:profiles:delete"},
						Projects:  []string{},
					},
				},
			}
			assertPolicy(t, &pol, resp)
		},
		"policy with two statements, both with projects, and three members, among other policy": func(t *testing.T) {
			projID0, projID1 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			polID := genSimpleID(t, prngSeed)
			sID0, sID1 := genUUID(t), genUUID(t)

			// insert projects
			project0 := insertTestProject(t, db, projID0, "test project 1", storage.Custom)
			project1 := insertTestProject(t, db, projID1, "test project 2", storage.Custom)

			// insert policy with statements
			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'deny'::iam_effect, array['iam:teams:delete', 'iam:teams:create'], array['iam:teams']),
               ($2, 'allow'::iam_effect, array['infra:nodes:delete', 'infra:nodes:rerun'], array['infra:nodes']) RETURNING id),
					 policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
				(SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)

			// insert projects in statements
			insertStatementProject(t, db, sID0, projID0)
			insertStatementProject(t, db, sID1, projID0)
			insertStatementProject(t, db, sID1, projID1)

			// insert members
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
						ID:        sID0,
						Effect:    storage.Deny,
						Resources: []string{"iam:teams"},
						Actions:   []string{"iam:teams:delete", "iam:teams:create"},
						Projects:  []string{project0.ID},
					},
					{
						ID:        sID1,
						Effect:    storage.Allow,
						Resources: []string{"infra:nodes"},
						Actions:   []string{"infra:nodes:delete", "infra:nodes:rerun"},
						Projects:  []string{project0.ID, project1.ID},
					},
				},
			}
			assertPolicy(t, &pol, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestListPolicyMembers(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
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
			sID0 := genUUID(t)
			sID1 := genUUID(t)
			polID := genSimpleID(t, prngSeed)

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
               ($2, 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)
			member0 := insertTestPolicyMember(t, db, polID, "user:local:c")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:a")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:b")

			members, err := store.ListPolicyMembers(ctx, polID)
			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2, member0}, members)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestListPolicies(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListPolicies(ctx)
			assert.NoError(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, 0, len(resp))
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
			sID0 := genUUID(t)
			sID1 := genUUID(t)
			polID := genSimpleID(t, prngSeed)

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
               ($2, 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
					 policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
				(SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)
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
						ID:        sID0,
						Effect:    storage.Allow,
						Resources: []string{"iam:users"},
						Actions:   []string{"iam:users:delete", "iam:users:create"},
						Projects:  []string{},
					},
					{
						ID:        sID1,
						Effect:    storage.Deny,
						Resources: []string{"compliance:profiles"},
						Actions:   []string{"compliance:profiles:download", "compliance:profiles:delete"},
						Projects:  []string{},
					},
				},
			}}
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies, with one statement each": func(t *testing.T) {
			ctx := context.Background()
			sID0, sID1, polID0, polID1 := genUUID(t), genUUID(t), genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
             VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name)
             VALUES ($2, '01testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, polID0)
			require.NoError(t, err)
			_, err = db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
             VALUES ($1, 'deny'::iam_effect, array['compliance:profiles:update'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name)
             VALUES ($2, '02testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID1, polID1)
			require.NoError(t, err)
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
							ID:        sID0,
							Effect:    storage.Allow,
							Resources: []string{"iam:users"},
							Actions:   []string{"iam:users:delete", "iam:users:create"},
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
							ID:        sID1,
							Effect:    storage.Deny,
							Resources: []string{"compliance:profiles"},
							Actions:   []string{"compliance:profiles:update"},
							Projects:  []string{},
						},
					},
				},
			}
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies, each with one statement that contains 1+ projects": func(t *testing.T) {
			ctx := context.Background()
			sID0, sID1 := genUUID(t), genUUID(t)
			polID0, polID1 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			projID0, projID1 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)

			// insert projects
			project0 := insertTestProject(t, db, projID0, "test project 1", storage.Custom)
			project1 := insertTestProject(t, db, projID1, "test project 2", storage.Custom)

			// insert first policy with statement
			_, err := db.Exec(`
			WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
			       VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']) RETURNING id),
			     policy AS (INSERT INTO iam_policies (id, name)
			       VALUES ($2, '03testpolicy') RETURNING id)
			INSERT INTO iam_policy_statements (policy_id, statement_id)
			  (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, polID0)
			require.NoError(t, err)

			// associate statement with project
			insertStatementProject(t, db, sID0, projID0)

			// insert second policy with statement
			_, err = db.Exec(`
			WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
			       VALUES ($1, 'deny'::iam_effect, array['compliance:profiles:update'], array['compliance:profiles']) RETURNING id),
			     policy AS (INSERT INTO iam_policies (id, name)
			       VALUES ($2, '04testpolicy') RETURNING id)
			INSERT INTO iam_policy_statements (policy_id, statement_id)
			  (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID1, polID1)
			require.NoError(t, err)

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
					Name:    "03testpolicy",
					Members: []storage.Member{member0},
					Type:    storage.Custom,
					Statements: []storage.Statement{
						{
							ID:        sID0,
							Effect:    storage.Allow,
							Resources: []string{"iam:users"},
							Actions:   []string{"iam:users:delete", "iam:users:create"},
							Projects:  []string{project0.ID},
						},
					},
				},
				{
					ID:      polID1,
					Name:    "04testpolicy",
					Members: []storage.Member{member1},
					Type:    storage.Custom,
					Statements: []storage.Statement{
						{
							ID:        sID1,
							Effect:    storage.Deny,
							Resources: []string{"compliance:profiles"},
							Actions:   []string{"compliance:profiles:update"},
							Projects:  []string{project0.ID, project1.ID},
						},
					},
				},
			}
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies, one with projects, one without": func(t *testing.T) {
			ctx := context.Background()
			polID1, polID2 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name1, name2 := "testPolicy", "anotherTestPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2), ($3, $4)`,
				polID1, name1, polID2, name2)
			require.NoError(t, err)

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
			assert.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"two policies with projects": func(t *testing.T) {
			ctx := context.Background()
			polID1, polID2 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name1, name2 := "testPolicy", "anotherTestPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2), ($3, $4)`,
				polID1, name1, polID2, name2)
			require.NoError(t, err)

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
			assert.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a policy list, return intersection": func(t *testing.T) {
			ctx := context.Background()
			polID1, polID2 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name1, name2 := "testPolicy", "anotherTestPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2), ($3, $4)`,
				polID1, name1, polID2, name2)
			require.NoError(t, err)

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
			assert.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a policy list of *, return everything": func(t *testing.T) {
			ctx := context.Background()
			polID1, polID2 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name1, name2 := "testPolicy", "anotherTestPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2), ($3, $4)`,
				polID1, name1, polID2, name2)
			require.NoError(t, err)

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
			assert.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a policy list of (unassigned), return policies with no projects": func(t *testing.T) {
			ctx := context.Background()
			polID1, polID2 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name1, name2 := "testPolicy", "anotherTestPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2), ($3, $4)`,
				polID1, name1, polID2, name2)
			require.NoError(t, err)

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
			assert.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when the list is filtered by a policy list of (unassigned) and another project, return matched projects": func(t *testing.T) {
			ctx := context.Background()
			polID1, polID2, polID3 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name1, name2, name3 := "testPolicy", "anotherTestPolicy", "pika"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2), ($3, $4), ($5, $6)`,
				polID1, name1, polID2, name2, polID3, name3)
			require.NoError(t, err)

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
			assert.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
		"when there is no intersection between projects filter and projects, return empty list": func(t *testing.T) {
			ctx := context.Background()
			polID1, polID2, polID3 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			name1, name2, name3 := "testPolicy", "anotherTestPolicy", "pika"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2), ($3, $4), ($5, $6)`,
				polID1, name1, polID2, name2, polID3, name3)
			require.NoError(t, err)

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
			assert.NoError(t, err)
			assertPolicies(t, expectedPolicies, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestDeletePolicy(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			err := store.DeletePolicy(ctx, genSimpleID(t, prngSeed))
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"policy not found with existing policies in store": func(t *testing.T) {
			ctx := context.Background()
			// Add a different policy
			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
      VALUES ($1, 'deny'::iam_effect, array['iam:users:create', 'iam:users:delete'], array['iam:users']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES (uuid_generate_v4(), 'otherpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, genUUID(t))
			require.NoError(t, err)

			polID := genSimpleID(t, prngSeed)
			err = store.DeletePolicy(ctx, polID)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"attached policy with no statements": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			err := store.DeletePolicy(ctx, polID)
			require.NoError(t, err)

			// assert that stuff happened to the database
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
		},
		"only one unattached policy with no statements": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")

			err := store.DeletePolicy(ctx, polID)
			require.NoError(t, err)

			// assert that stuff happened to the database
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
		},
		"only one unattached policy with two statements": func(t *testing.T) {
			ctx := context.Background()
			sID0, sID1, polID := genUUID(t), genUUID(t), genSimpleID(t, prngSeed)

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
               ($2, 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			err = store.DeletePolicy(ctx, polID)
			require.NoError(t, err)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1`, sID0))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1`, sID1))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
		},
		"unattached policy with two statements, next to other policy": func(t *testing.T) {
			ctx := context.Background()
			polID0 := genSimpleID(t, prngSeed)

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES (uuid_generate_v4(), 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
               (uuid_generate_v4(), 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($1, 'otherpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement)`, polID0)
			require.NoError(t, err)
			insertTestPolicyMember(t, db, polID0, "user:local:albertine")

			sID0, sID1, polID1 := genUUID(t), genUUID(t), genSimpleID(t, prngSeed)

			_, err = db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
               ($2, 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID1)
			require.NoError(t, err)
			member := insertTestPolicyMember(t, db, polID1, "user:local:charmander")

			err = store.DeletePolicy(ctx, polID1)
			require.NoError(t, err)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID1))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1`, sID0))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1`, sID1))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID1))
			// Members get left in the table for now.
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1`, member.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_statements`))
		},
		"only one unattached policy with one statement with projects": func(t *testing.T) {
			ctx := context.Background()
			sID0, polID := genUUID(t), genSimpleID(t, prngSeed)
			projID0, projID1 := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)

			// insert projects
			insertTestProject(t, db, projID0, "let's go eevee - prod", storage.Custom)
			insertTestProject(t, db, projID1, "let's go eevee - dev", storage.Custom)

			// insert policy with statements
			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($2, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, polID)
			require.NoError(t, err)

			// insert project into statements
			insertStatementProject(t, db, sID0, projID0)
			insertStatementProject(t, db, sID0, projID1)

			insertTestPolicyMember(t, db, polID, "user:local:eevee")

			err = store.DeletePolicy(ctx, polID)
			require.NoError(t, err)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1`, sID0))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
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
			err := store.DeletePolicy(ctx, polID)
			assert.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
		},
		"when the policy is has no projects with a filter of (unassigned), delete the policy": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID})
			err := store.DeletePolicy(ctx, polID)
			assert.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
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
			assert.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
		},
		"when the policy does not overlaps with the project filter, return not found": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{projID2})
			err := store.DeletePolicy(ctx, polID)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestCreatePolicy(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()
	ctx := context.Background()

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
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
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
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"policy with one resources+actions statement": func(t *testing.T) {
			polID, sID := genSimpleID(t, prngSeed), genUUID(t)
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			member := genMember(t, "user:local:albertine")
			statement := storage.Statement{
				ID:        sID,
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
			resp, err := store.CreatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID, pq.Array(resources), pq.Array(actions), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"policy with one resources+actions statement and two members": func(t *testing.T) {
			polID, sID := genSimpleID(t, prngSeed), genUUID(t)
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement0 := storage.Statement{
				ID:        sID,
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
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID, pq.Array(resources), pq.Array(actions), "deny"))
			assertMembers(t, db, polID, []storage.Member{member0, member1})
		},
		"policy with two resources+actions statements": func(t *testing.T) {
			polID, sID0, sID1 := genSimpleID(t, prngSeed), genUUID(t), genUUID(t)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			resources1, actions1 := []string{"compliance:profiles"}, []string{"compliance:profiles:upload", "compliance:profiles:delete"}
			statement0 := storage.Statement{
				ID:        sID0,
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
			}
			statement1 := storage.Statement{
				ID:        sID1,
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
			resp, err := store.CreatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID0, pq.Array(resources0), pq.Array(actions0), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID1, pq.Array(resources1), pq.Array(actions1), "allow"))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"policy with one resources+actions statement and two members with the same name ignores the duplicate": func(t *testing.T) {
			polID, sID0 := genSimpleID(t, prngSeed), genUUID(t)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement0 := storage.Statement{
				ID:        sID0,
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
			}
			member0 := genMember(t, "user:local:albertine")
			member1 := genMember(t, "user:local:albertine")

			pol := storage.Policy{
				ID:         polID,
				Name:       name,
				Members:    []storage.Member{member0, member1},
				Type:       typeVal,
				Statements: []storage.Statement{statement0},
			}
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID0, pq.Array(resources0), pq.Array(actions0), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 AND member_id=$2`, polID, member0.ID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member0.ID, member0.Name))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 AND member_id=$2`, polID, member1.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member1.ID, member1.Name))
		},
		"policy with the same resources+actions statement passed twice": func(t *testing.T) {
			polID, sID := genSimpleID(t, prngSeed), genUUID(t)
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement := storage.Statement{
				ID:        sID,
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
			resp, err := store.CreatePolicy(ctx, &pol)
			require.Error(t, err)
			assert.Nil(t, resp)
		},
		"policy with same resources+actions, but different ID statements": func(t *testing.T) {
			// Note (sr): I don't think this is wrong, just worth noting; so, here's
			// a test for this behaviour
			polID, sID0, sID1 := genSimpleID(t, prngSeed), genUUID(t), genUUID(t)
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement0 := storage.Statement{
				ID:        sID0,
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			statement1 := storage.Statement{
				ID:        sID1,
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
			resp, err := store.CreatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID0, pq.Array(resources), pq.Array(actions), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID1, pq.Array(resources), pq.Array(actions), "deny"))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"policy with statement ID already used in other policy's statements": func(t *testing.T) {
			// Note(sr): implementation detail worth noting (reconsidering?)
			sID, polID, originalPolID := genUUID(t), genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)

			// add a policy using that statement
			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
      VALUES ($2, 'deny'::iam_effect, array['iam:users:create', 'iam:users:delete'], array['iam:users']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($1, 'otherpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, originalPolID, sID)
			require.NoError(t, err)
			insertTestPolicyMember(t, db, originalPolID, "user:local:albertine")

			// try to add another one using that statement
			name, typeVal := "toBeCreated", storage.Custom
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			member1 := genMember(t, "user:local:otheruser")
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Members: []storage.Member{member1},
				Type:    typeVal,
				Statements: []storage.Statement{{
					ID:        sID,
					Effect:    storage.Deny,
					Resources: resources,
					Actions:   actions,
				}},
			}
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.Equal(t, storage_errors.ErrConflict, err)
			assert.Nil(t, resp)

			// the second policy was NOT added,
			assertEmpty(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			// and it doesn't have any statements...
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID))
			// however, the statement is still there, so the existing policy is intact
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID, pq.Array(resources), pq.Array(actions), "deny"))
			// and the original member is NOT updated because our policy-create db interaction
			// is properly transactional.
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 AND member_id=$2`, polID, member1.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member1.ID, member1.Name))
		},
		"policy with two resources+actions+existing project statements": func(t *testing.T) {
			polID, sID0, sID1, projID := genSimpleID(t, prngSeed), genUUID(t), genUUID(t), genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:teams"}, []string{"iam:teams:create", "iam:teams:delete"}
			resources1, actions1 := []string{"infra:nodes"}, []string{"infra:nodes:delete", "infra:nodes:rerun"}

			insertTestProject(t, db, projID, "let's go jigglypuff - topsecret", storage.Custom)

			statement0 := storage.Statement{
				ID:        sID0,
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
				Projects:  []string{projID},
			}
			statement1 := storage.Statement{
				ID:        sID1,
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
			resp, err := store.CreatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID0, pq.Array(resources0), pq.Array(actions0), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID1, pq.Array(resources1), pq.Array(actions1), "allow"))
			assertMembers(t, db, polID, []storage.Member{member})
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE statement_id=$1 AND project_id=$2`, sID0, projID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE statement_id=$1 AND project_id=$2`, sID1, projID))
		},
		"policy with two resources+actions+non-existent project statements fails": func(t *testing.T) {
			polID, sID0, sID1, projID := genSimpleID(t, prngSeed), genUUID(t), genUUID(t), genSimpleID(t, prngSeed)
			name, typeVal := "toBeCreated", storage.Custom
			resources0, actions0 := []string{"iam:teams"}, []string{"iam:teams:create", "iam:teams:delete"}
			resources1, actions1 := []string{"infra:nodes"}, []string{"infra:nodes:delete", "infra:nodes:rerun"}

			statement0 := storage.Statement{
				ID:        sID0,
				Effect:    storage.Deny,
				Resources: resources0,
				Actions:   actions0,
				Projects:  []string{projID},
			}
			statement1 := storage.Statement{
				ID:        sID1,
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
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=$1`, projID))

			resp, err := store.CreatePolicy(ctx, &pol)
			require.Error(t, err)
			assert.Nil(t, resp)
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
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
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
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
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
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))

			projCount := db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID)
			assertCount(t, 2, projCount)
		},
		"policy with non-existent project fails": func(t *testing.T) {
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
			resp, err := store.CreatePolicy(ctx, &pol)
			assert.Error(t, err)
			assert.Nil(t, resp)

			assertEmpty(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestReplacePolicyMembers(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			resp, err := store.ReplacePolicyMembers(ctx, wrongPolID, []storage.Member{member})
			assert.Error(t, err)
			assert.Empty(t, resp)
		},
		"policy not found reports an error": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")

			resp, err := store.ReplacePolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member1})

			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"updating policy with NO members to SOME members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))

			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{member1, member2})
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
		},
		"updating policy with SOME members to NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"updating policy with SOME members to NEW members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			polMember1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			polMember2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			member1 := genMember(t, "user:local:foo")
			member2 := genMember(t, "user:local:bar")
			member3 := genMember(t, "team:saml:gophers")
			member4 := genMember(t, "team:saml:editors")
			members := []storage.Member{member1, member2, member3, member4}

			resp, err := store.ReplacePolicyMembers(ctx, polID, members)
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertCount(t, len(members), db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, len(members)+2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			// old members no longer associated with policy
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, polID, polMember1.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, polID, polMember2.ID))

			// new members
			assertMembers(t, db, polID, members)
		},
		"updating policy with SOME members to NEW members with some REUSED members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			polMember1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			polMember2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			polMember3 := insertTestPolicyMember(t, db, polID, "team:local:friends")
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_members`))
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			member3 := genMember(t, "team:saml:gophers")
			member4 := genMember(t, "team:saml:editors")
			members := []storage.Member{member1, member2, member3, member4}

			resp, err := store.ReplacePolicyMembers(ctx, polID, members)
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 5, db.QueryRow(`SELECT count(*) FROM iam_members`))

			// re-used members plus new members
			assertMembers(t, db, polID, []storage.Member{polMember1, polMember2, member3, member4})

			// member still exists but disassociated from policy
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1`, polMember3.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, polID, polMember3.ID))

			// new members re-used so their new IDs are discarded
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, polID, member1.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, polID, member2.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1`, member1.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1`, member2.ID))
		},
		"updating policy by ADDING member from ANOTHER policy": func(t *testing.T) {
			ctx := context.Background()
			otherPolID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, otherPolID, "user:local:originaluser")

			polID := insertTestPolicy(t, db, "otherTestPolicy")
			member := genMember(t, "user:local:originaluser")

			// baseline: member is in just one policy
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, otherPolID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))

			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{member})
			require.NoError(t, err)
			require.NotNil(t, resp)

			// now still just one member but in two policies
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, otherPolID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"updating policy by REMOVING member from ANOTHER policy": func(t *testing.T) {
			ctx := context.Background()
			otherPolID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, otherPolID, "user:local:originaluser")

			polID := insertTestPolicy(t, db, "otherTestPolicy")
			_, err := db.Query(`INSERT INTO iam_policy_members (policy_id, member_id) values($1, $2)`, polID, member.ID)
			require.NoError(t, err)

			// baseline: member is in two policies
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, otherPolID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))

			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
			require.NoError(t, err)
			require.NotNil(t, resp)

			// now member remains in just one policy
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, otherPolID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"when the policy's projects and the project filter intersect, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"when the * project filter is passed, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestRemovePolicyMembers(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			resp, err := store.RemovePolicyMembers(ctx, wrongPolID, []storage.Member{member})
			assert.Error(t, err)
			assert.Empty(t, resp)
		},
		"policy not found reports an error": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")

			resp, err := store.RemovePolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member})

			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"removing members from policy with NO members results in an empty member list": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))

			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Empty(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"removing members from policy with SOME members to now have NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Empty(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"removing repeat members from policy with SOME members to now have LESS members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			remainingMember := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:fred")
			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 1, len(resp))
			require.Contains(t, resp, remainingMember)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`,
					polID, remainingMember.ID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"removing members from policy with ONE member to now have NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_members`))

			member1 := genMember(t, "user:local:fred")
			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Empty(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"removing only non-members from policy results in no change to policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 2, len(resp))
			require.Contains(t, resp, member1)
			require.Contains(t, resp, member2)

			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"removing members from policy with SOME members to now have less members with some ignored": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			polMember3 := insertTestPolicyMember(t, db, polID, "user:local:charmander")
			polMember4 := insertTestPolicyMember(t, db, polID, "user:local:squirtle")
			polMember5 := insertTestPolicyMember(t, db, polID, "user:local:bulbasaur")
			assertCount(t, 5, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 5, db.QueryRow(`SELECT count(*) FROM iam_members`))
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "team:saml:notfound1")
			member3 := genMember(t, "user:local:mary")
			member4 := genMember(t, "team:saml:notfound2")
			members := []storage.Member{member1, member2, member3, member4}

			resp, err := store.RemovePolicyMembers(ctx, polID, members)
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 3, len(resp))
			require.Contains(t, resp, polMember3)
			require.Contains(t, resp, polMember4)
			require.Contains(t, resp, polMember5)

			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 5, db.QueryRow(`SELECT count(*) FROM iam_members`))
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
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			members := []storage.Member{member1, member2}

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			resp, err := store.RemovePolicyMembers(ctx, polID, members)
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"when the * project filter is passed, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestAddPolicyMembers(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"fails to add with ErrNotFound when the database is empty and members are added to a non-existent policy": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			resp, err := store.AddPolicyMembers(ctx, wrongPolID, []storage.Member{member})

			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"fails to add with ErrNotFound when a policy exists but members are added to a non-existent policy": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")

			resp, err := store.AddPolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member})

			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"adding one member to a policy with NO members results in member being added": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))

			resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.NotEmpty(t, resp)

			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"adding several members to a policy with NO members results in members being added": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			member3 := genMember(t, "user:local:max")
			member4 := genMember(t, "user:local:ellen")
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))

			resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member1, member2, member3, member4})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.NotEmpty(t, resp)

			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3, member4})
		},
		"adding one member to a policy with SOME members succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
			member3 := genMember(t, "user:local:max")

			resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.NotEmpty(t, resp)

			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding several members to a policy with SOME members succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
			member3 := genMember(t, "user:local:max")
			member4 := genMember(t, "user:local:ellen")
			member5 := genMember(t, "user:local:barry")
			member6 := genMember(t, "user:local:jack")

			resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3, member4, member5, member6})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.NotEmpty(t, resp)

			assertCount(t, 6, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 6, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3, member4, member5, member6})
		},
		"adding same member more than once w/ single request doesn't result in duplicate policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			member3 := genMember(t, "user:local:ellen")
			repeatMember3 := genMember(t, "user:local:ellen")

			resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3, repeatMember3})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 3, len(resp))

			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding same member more than once w/ duplicate request doesn't result in duplicate policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			member3 := genMember(t, "user:local:ellen")

			// add ellen to the policy
			resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 3, len(resp))

			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})

			// attempt to add ellen again in a duplicate request
			resp, err = store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 3, len(resp))

			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_members`))
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
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID1))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID2))

			resp, err := store.AddPolicyMembers(ctx, polID2, []storage.Member{member1, member2, member3, member4})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 4, len(resp))

			assertMembers(t, db, polID1, []storage.Member{member1, member2, member3})
			assertMembers(t, db, polID2, []storage.Member{member1, member2, member3, member4})
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID1))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID2))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"adding members where the members match the existing policy membership results in no new members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member1, member2})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 2, len(resp))

			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
		},
		"adding member that's already a member of a different policy doesn't result in additional iam_members entry": func(t *testing.T) {
			ctx := context.Background()
			polID1 := insertTestPolicy(t, db, "testpolicy1")
			polID2 := insertTestPolicy(t, db, "testpolicy2")
			member := insertTestPolicyMember(t, db, polID1, "user:local:fred")
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID1))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID2))

			resp, err := store.AddPolicyMembers(ctx, polID2, []storage.Member{member})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 1, len(resp))

			assertMembers(t, db, polID1, []storage.Member{member})
			assertMembers(t, db, polID2, []storage.Member{member})
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, polID1, member.ID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, polID2, member.ID))
		},
		"when the policy's projects and the project filter intersect, add members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			resp, err := store.AddPolicyMembers(ctx, polID, members)
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"when the * project filter is passed, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			resp, err := store.AddPolicyMembers(ctx, polID, members)
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, v2.UnassignedProjectID})
			resp, err := store.AddPolicyMembers(ctx, polID, members)
			require.NoError(t, err)
			require.NotNil(t, resp)

			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_members`))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, v2.UnassignedProjectID})
			resp, err := store.AddPolicyMembers(ctx, polID, members)
			assert.Nil(t, resp)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestUpdatePolicy(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"policy not found": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name, typeVal := "somename", storage.Custom
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{member},
			}
			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, resp)
		},
		"policy not found with existing policies in store": func(t *testing.T) {
			// Add a different policy
			polID0 := genSimpleID(t, prngSeed)
			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
      VALUES ($1, 'deny'::iam_effect, array['iam:users:create', 'iam:users:delete'], array['iam:users']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($2, 'otherpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, genUUID(t), polID0)
			require.NoError(t, err)
			insertTestPolicyMember(t, db, polID0, "user:local:albertine")

			polID := genSimpleID(t, prngSeed)
			name, typeVal := "somename", storage.Custom
			member := genMember(t, "user:local:albertine")
			pol := storage.Policy{
				ID:      polID,
				Name:    name,
				Type:    typeVal,
				Members: []storage.Member{member},
			}
			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, resp)
		},
		"policy with no statements, updating fields": func(t *testing.T) {
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

			resp, err := store.UpdatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
		},
		"policy with no statements, changing the type": func(t *testing.T) {
			row := db.QueryRow(`INSERT INTO iam_policies
        (id, name, type)
        VALUES (uuid_generate_v4(), 'testpolicy', 'custom')
        RETURNING id`)
			require.NotNil(t, row)
			var polID string
			err := row.Scan(&polID)
			require.NoError(t, err)
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

			resp, err := store.UpdatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member.ID, member.Name))
		},
		"policy with no statements, adding two statements": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			sID0, sID1 := genUUID(t), genUUID(t)
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			member := genMember(t, "user:local:new_member")
			statement0 := storage.Statement{
				ID:        sID0,
				Effect:    storage.Deny,
				Resources: resources,
				Actions:   actions,
			}
			statement1 := storage.Statement{
				ID:        sID1,
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

			resp, err := store.UpdatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID1))
			assertCount(t, 2, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID0, pq.Array(resources), pq.Array(actions), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID1, pq.Array(resources), pq.Array(actions), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member.ID, member.Name))
		},
		"policy with two statements, removing one statement": func(t *testing.T) {
			polID, sID0, sID1 := genSimpleID(t, prngSeed), genUUID(t), genUUID(t)

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['compliance:profiles:download'], array['compliance:profiles']),
               ($2, 'deny'::iam_effect, array['iam:users:create', 'iam:users:create'], array['iam:users']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($3, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, sID1, polID)
			require.NoError(t, err)
			insertTestPolicyMember(t, db, polID, "user:local:albertine")

			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement := storage.Statement{
				ID:        sID0,
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

			resp, err := store.UpdatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID0, pq.Array(resources), pq.Array(actions), "deny"))

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID1))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID1, pq.Array(resources), pq.Array(actions), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member.ID, member.Name))
		},
		"policy statement conflict with existing policies in store, triggering rollback": func(t *testing.T) {
			// Note(sr): test case only serves to demonstrate transaction fix for update
			sID, polID, originalPolID := genUUID(t), genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)

			// add a policy using that statement
			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
      VALUES ($2, 'deny'::iam_effect, array['iam:users:create', 'iam:users:delete'], array['iam:users']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($1, 'firstpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, originalPolID, sID)
			require.NoError(t, err)
			member0 := insertTestPolicyMember(t, db, originalPolID, "user:local:albertine")

			// Add a different policy
			sID0, sID1 := genUUID(t), genUUID(t)
			_, err = db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
      VALUES ($1, 'deny'::iam_effect, array['compliance:profile:delete'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($2, 'otherpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID0, polID)
			require.NoError(t, err)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:albert")

			// update second policy with statement conflicting with other policy
			member := genMember(t, "user:local:new_member")
			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			resources1, actions1 := []string{"iam:teams"}, []string{"iam:teams:create"}
			statement0 := storage.Statement{
				ID:        sID1, // fresh
				Effect:    storage.Deny,
				Resources: resources1,
				Actions:   actions1,
			}
			statement1 := storage.Statement{
				ID:        sID, // conflict
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

			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.Error(t, err)
			assert.Nil(t, resp)

			// The first policy was left intact
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, originalPolID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, originalPolID, sID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID, pq.Array(resources), pq.Array(actions), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 AND member_id=$2`, originalPolID, member0.ID))

			// The update of the second policy was NOT half-done
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID1, pq.Array(resources1), pq.Array(actions1), "deny"))

			// The policy has no new statement after the update
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID0))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID))

			// and the policy members have not changed
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 AND member_id=$2`, polID, member1.ID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 AND member_id=$2`, polID, member.ID))
		},
		"policy with one statement, adding existing project to statement": func(t *testing.T) {
			polID, projID := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			sID := genUUID(t)

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['compliance:profiles:download'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($2, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID, polID)
			require.NoError(t, err)

			member := insertTestPolicyMember(t, db, polID, "user:local:totodile")
			insertTestProject(t, db, projID, "pokemon crystal", storage.Custom)

			resources, actions := []string{"iam:users"}, []string{"iam:users:create", "iam:users:delete"}
			statement := storage.Statement{
				ID:        sID,
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

			resp, err := store.UpdatePolicy(ctx, &pol)
			require.NoError(t, err)
			assert.Equal(t, &pol, resp)

			assertOne(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, name, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID, pq.Array(resources), pq.Array(actions), "deny"))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE statement_id=$1 AND project_id=$2`, sID, projID))

			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member.ID, member.Name))
		},
		"policy with one statement, adding non-existent project to statement fails": func(t *testing.T) {
			polID, projID := genSimpleID(t, prngSeed), genSimpleID(t, prngSeed)
			sID := genUUID(t)
			resources, actions := []string{"compliance:profiles"}, []string{"compliance:profiles:download"}

			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES ($1, 'allow'::iam_effect, array['compliance:profiles:download'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($2, 'testpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement) RETURNING policy_id;`, sID, polID)
			require.NoError(t, err)

			member := insertTestPolicyMember(t, db, polID, "user:local:totodile")

			statement := storage.Statement{
				ID:        sID,
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
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=$1`, projID))

			resp, err := store.UpdatePolicy(ctx, &pol)
			require.Error(t, err)
			assert.Nil(t, resp)

			// no update to policy or members
			assertEmpty(t,
				db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`,
					polID, newName, typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1 AND name=$2 AND type=$3`, polID, "testpolicy", typeVal.String()))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member.ID, member.Name))

			// no update to statement
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE statement_id=$1 AND project_id=$2`, sID, projID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1 AND statement_id=$2`, polID, sID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE id=$1 AND resources=$2 AND actions=$3 AND effect=$4`,
				sID, pq.Array(resources), pq.Array(actions), "allow"))
		},
		"policy with no projects to some projects": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name := "testPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2)`, polID, name)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)

			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: []string{projID},
			}

			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.ElementsMatch(t, []string{projID}, resp.Projects)
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
		},
		"policy with project to no projects": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name := "testPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2)`, polID, name)
			require.NoError(t, err)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))

			expProjs := []string{}
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: expProjs,
			}

			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.ElementsMatch(t, expProjs, resp.Projects)
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
		},
		"policy with projects to same projects": func(t *testing.T) {
			// TODO optimize/opt-out if they're the same?

			polID := genSimpleID(t, prngSeed)
			name := "testPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2)`, polID, name)
			require.NoError(t, err)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "too ordinary", storage.Custom)
			insertPolicyProject(t, db, polID, projID2)
			initPolProjCount := db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID)
			assertCount(t, 2, initPolProjCount)

			expProjs := []string{projID, projID2}
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: expProjs,
			}

			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, expProjs, resp.Projects)
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			expPolProjCount := db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID)
			assertCount(t, 2, expPolProjCount)
		},
		"policy with single project to diff project": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name := "testPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2)`, polID, name)
			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))

			projID2 := "ordinary-project"
			insertTestProject(t, db, projID2, "too ordinary", storage.Custom)
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: []string{projID2},
			}

			expProjs := []string{projID2}
			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.ElementsMatch(t, expProjs, resp.Projects)

			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
		},
		"policy with one project to additional project": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name := "testPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2)`, polID, name)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
			require.NoError(t, err)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))

			projID2 := "another-project"
			insertTestProject(t, db, projID2, "more", storage.Custom)

			expProjs := []string{projID, projID2}
			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: expProjs,
			}

			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.NoError(t, err)
			assert.Equal(t, expProjs, resp.Projects)

			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))

			projCount := db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID)
			assertCount(t, 2, projCount)
		},
		"policy with project to add non-existent project fails": func(t *testing.T) {
			polID := genSimpleID(t, prngSeed)
			name := "testPolicy"
			_, err := db.Exec(`INSERT INTO iam_policies (id, name) VALUES ($1, $2)`, polID, name)
			require.NoError(t, err)

			projID := "special-project"
			insertTestProject(t, db, projID, "too special", storage.Custom)
			insertPolicyProject(t, db, polID, projID)
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))

			pol := storage.Policy{
				ID:       polID,
				Name:     name,
				Projects: []string{projID, "not-real"},
			}

			resp, err := store.UpdatePolicy(ctx, &pol)
			assert.Error(t, err)
			assert.Nil(t, resp)

			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policies WHERE id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_statements WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_statements`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1`, polID))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_projects WHERE policy_id=$1`, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestCreateProject(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"successfully creates custom project": func(t *testing.T) {
			project := storage.Project{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"my-id-1"},
			}
			resp, err := store.CreateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"successfully creates chef-managed project": func(t *testing.T) {
			project := storage.Project{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.ChefManaged,
				Projects: []string{"my-id-1"},
			}
			resp, err := store.CreateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"does not create project with duplicate ID": func(t *testing.T) {
			projectID := "my-id-1"
			projectOriginal := storage.Project{
				ID:       projectID,
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"my-id-1"},
			}
			resp, err := store.CreateProject(ctx, &projectOriginal)
			require.NoError(t, err)
			require.Equal(t, &projectOriginal, resp)

			projectConflict := storage.Project{
				ID:       projectID,
				Name:     "Something Else",
				Type:     storage.Custom,
				Projects: []string{"my-id-1"},
			}
			resp, err = store.CreateProject(ctx, &projectConflict)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrConflict, err)
			assert.Nil(t, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestUpdateProject(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"successfully updates existing custom project": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:       "foo",
				Name:     "updated-name",
				Type:     storage.Custom,
				Projects: []string{"foo"},
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
				ID:       "foo",
				Name:     "updated-name",
				Type:     storage.Custom,
				Projects: []string{"foo"},
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
				ID:       "foo",
				Name:     "updated-name",
				Type:     storage.Custom,
				Projects: []string{"foo"},
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
				ID:       "not-found",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"not-found"},
			}
			resp, err := store.UpdateProject(ctx, &project)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, resp)
		},
		"returns ErrNotFound if the project exists but does not have a project in the project filter list": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:       "foo",
				Name:     "updated-name",
				Type:     storage.Custom,
				Projects: []string{"foo"},
			}
			ctx = insertProjectsIntoContext(ctx, []string{"wrong", "projects"})

			resp, err := store.UpdateProject(ctx, &project)
			assert.Equal(t, storage_errors.ErrNotFound, err)
			assert.Nil(t, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestGetProject(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
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
				ID:       "foo",
				Name:     "my foo project",
				Type:     storage.ChefManaged,
				Projects: []string{"foo"},
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:       "foo",
				Name:     "my foo project",
				Type:     storage.Custom,
				Projects: []string{"foo"},
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists with a project filter of *, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:       "foo",
				Name:     "my foo project",
				Type:     storage.Custom,
				Projects: []string{"foo"},
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
		db.flush(t)
	}
}

func TestDeleteProject(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
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
		{"deletes project with one project in database", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
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
		{"returns not found when the project filter excludes the project in question", func(t *testing.T) {
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
		db.flush(t)
	}
}

func TestListProjects(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
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
		{"when two projects (custom and chef-managed) exist, returns them", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertTestProject(t, db, "bar", "my bar project", storage.Custom)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{
				&storage.Project{
					ID:       "foo",
					Name:     "my foo project",
					Type:     storage.ChefManaged,
					Projects: []string{"foo"},
				},
				&storage.Project{
					ID:       "bar",
					Name:     "my bar project",
					Type:     storage.Custom,
					Projects: []string{"bar"},
				},
			}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"when multiple projects exist, filter based on projects lists", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertTestProject(t, db, "baz", "my baz project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{
				&storage.Project{
					ID:       "foo",
					Name:     "my foo project",
					Type:     storage.ChefManaged,
					Projects: []string{"foo"},
				},
				&storage.Project{
					ID:       "bar",
					Name:     "my bar project",
					Type:     storage.Custom,
					Projects: []string{"bar"},
				},
			}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"when multiple projects exist, returns everything when no project filter is specified (v2.0 case)", func(t *testing.T) {
			ctx := context.Background()
			ctx = insertProjectsIntoContext(ctx, []string{})
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertTestProject(t, db, "bar", "my bar project", storage.Custom)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{
				&storage.Project{
					ID:       "foo",
					Name:     "my foo project",
					Type:     storage.ChefManaged,
					Projects: []string{"foo"},
				},
				&storage.Project{
					ID:       "bar",
					Name:     "my bar project",
					Type:     storage.Custom,
					Projects: []string{"bar"},
				},
			}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"when multiple projects exist, filter based on projects lists", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertTestProject(t, db, "baz", "my baz project", storage.Custom)
			ctx = auth_context.NewOutgoingProjectsContext(auth_context.NewContext(ctx,
				[]string{}, []string{"foo", "bar"}, "resource", "action", "pol"))

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{
				&storage.Project{
					ID:       "foo",
					Name:     "my foo project",
					Type:     storage.ChefManaged,
					Projects: []string{"foo"},
				},
				&storage.Project{
					ID:       "bar",
					Name:     "my bar project",
					Type:     storage.Custom,
					Projects: []string{"bar"},
				},
			}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"when multiple projects exist, returns everything when no project filter is specified (v2.0 case)", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertTestProject(t, db, "baz", "my baz project", storage.Custom)

			ctx = auth_context.NewOutgoingProjectsContext(auth_context.NewContext(ctx,
				[]string{}, []string{}, "resource", "action", "pol"))

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{
				&storage.Project{
					ID:       "foo",
					Name:     "my foo project",
					Type:     storage.ChefManaged,
					Projects: []string{"foo"},
				},
				&storage.Project{
					ID:       "bar",
					Name:     "my bar project",
					Type:     storage.Custom,
					Projects: []string{"bar"},
				},
				&storage.Project{
					ID:       "baz",
					Name:     "my baz project",
					Type:     storage.Custom,
					Projects: []string{"baz"},
				},
			}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"when multiple projects exist, returns all projects will * filter passed", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertTestProject(t, db, "baz", "my baz project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{
				&storage.Project{
					ID:       "foo",
					Name:     "my foo project",
					Type:     storage.ChefManaged,
					Projects: []string{"foo"},
				},
				&storage.Project{
					ID:       "bar",
					Name:     "my bar project",
					Type:     storage.Custom,
					Projects: []string{"bar"},
				},
				&storage.Project{
					ID:       "baz",
					Name:     "my baz project",
					Type:     storage.Custom,
					Projects: []string{"baz"},
				},
			}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
	}
	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.flush(t)
	}
}

func TestCreateRole(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
	defer store.Close()
	ctx := context.Background()

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
			resp, err := store.CreateRole(ctx, &role)
			require.NoError(t, err)
			require.Equal(t, &role, resp)

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
			resp, err := store.CreateRole(ctx, &role)
			require.NoError(t, err)
			require.Equal(t, &role, resp)

			assertRolesMatch(t, db, role)
		},
		"successfully creates a role with a project": func(t *testing.T) {
			project := storage.Project{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"my-id-1"},
			}
			_, err := store.CreateProject(ctx, &project)
			require.NoError(t, err)

			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{project.ID},
			}
			resp, err := store.CreateRole(ctx, &role)
			require.NoError(t, err)
			require.Equal(t, &role, resp)

			assertRolesMatch(t, db, role)
		},
		"successfully creates role with multiple projects": func(t *testing.T) {
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{project1.ID, project2.ID},
			}
			resp, err := store.CreateRole(ctx, &role)
			require.NoError(t, err)
			require.Equal(t, &role, resp)

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
			resp, err := store.CreateRole(ctx, &role)
			require.NoError(t, err)
			require.Equal(t, &role, resp)

			role2 := storage.Role{
				ID:      roleID,
				Name:    "name4b",
				Type:    storage.Custom,
				Actions: []string{"action3", "action4"},
			}
			resp, err = store.CreateRole(ctx, &role2)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrConflict, err)
			assert.Nil(t, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestListRoles(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name3",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name3",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
		db.flush(t)
	}
}

func TestGetRole(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
		db.flush(t)
	}
}

func TestReset(t *testing.T) {
	store, db, prngSeed := setup(t)
	defer db.close(t)
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
			polID := genSimpleID(t, prngSeed)
			_, err := db.Exec(`
      WITH statement AS (INSERT INTO iam_statements (id, effect, actions, resources)
        VALUES (uuid_generate_v4(), 'allow'::iam_effect, array['iam:users:delete', 'iam:users:create'], array['iam:users']),
               (uuid_generate_v4(), 'deny'::iam_effect, array['compliance:profiles:download', 'compliance:profiles:delete'], array['compliance:profiles']) RETURNING id),
           policy AS (INSERT INTO iam_policies (id, name) VALUES ($1, 'otherpolicy') RETURNING id)
      INSERT INTO iam_policy_statements (policy_id, statement_id)
        (SELECT policy.id, statement.id FROM policy, statement)`, polID)
			require.NoError(t, err)
			insertTestPolicyMember(t, db, polID, "user:local:albertine")
			insertTestPolicyMember(t, db, polID, "user:local:othermember")

			require.NoError(t, store.Reset(ctx))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_members`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles`))
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestDeleteRole(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
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
		"deletes role with several roles in database when projects filter has intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name3",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name3",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name3",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
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
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name3",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
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
		db.flush(t)
	}
}

func TestUpdateRole(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
	defer store.Close()
	nonexistingRole := storage.Role{
		ID:      "nonexistant",
		Name:    "name",
		Actions: []string{"actionx"},
	}

	cases := map[string]func(*testing.T){
		"returns role not found error with empty database": func(t *testing.T) {
			ctx := context.Background()
			role, err := store.UpdateRole(ctx, &nonexistingRole)

			assert.Nil(t, role)
			assert.Error(t, err)
			assert.Equal(t, storage_errors.ErrNotFound, err)
		},
		"returns role not found with several roles in database": func(t *testing.T) {
			ctx := context.Background()
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
			ctx := context.Background()
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
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
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
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name2",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name3",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
			require.NoError(t, err)

			project4 := storage.Project{
				ID:       "project-4",
				Name:     "name4",
				Type:     storage.Custom,
				Projects: []string{"project-4"},
			}
			_, err = store.CreateProject(ctx, &project4)
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
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:       "project-3",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-3"},
			}
			_, err = store.CreateProject(ctx, &project3)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID, project3.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project2.ID},
			}
			ctx = insertProjectsIntoContext(ctx, []string{project2.ID, project1.ID})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project2.ID}, updatedRole.Projects)
		},
		"updates successfully when a project filter is *": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project1.ID},
			}
			ctx = insertProjectsIntoContext(ctx, []string{v2.AllProjectsExternalID})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project1.ID}, updatedRole.Projects)
		},
		"updates successfully when a role has no projects": func(t *testing.T) {
			ctx := context.Background()

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{},
			}
			ctx = insertProjectsIntoContext(ctx, []string{v2.UnassignedProjectID})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"updates the projects of a role to contain projects from empty": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:       "project-1",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-1"},
			}
			_, err := store.CreateProject(ctx, &project1)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:       "project-2",
				Name:     "name1",
				Type:     storage.Custom,
				Projects: []string{"project-2"},
			}
			_, err = store.CreateProject(ctx, &project2)
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
			ctx := context.Background()
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
			ctx := context.Background()
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
		db.flush(t)
	}
}

func TestMigrationStatusProvider(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"when nothing was ever stored, returns pristine": func(t *testing.T) {
			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Pristine, ms)
		},
		"record in-progress, read back": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.InProgress, ms)
		},
		"record success, read back": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Success(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Successful, ms)
		},
		"record failure, read back": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Failure(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Failed, ms)
		},
		"record pristine, read back": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Failure(ctx))
			require.NoError(t, store.Pristine(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Pristine, ms)
		},
		"record failure, record in-progress, record success, read back": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Failure(ctx))
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Success(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Successful, ms)
		},
		"cannot go straight to failure": func(t *testing.T) {
			require.Error(t, store.Failure(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Pristine, ms)
		},
		"cannot go straight to success": func(t *testing.T) {
			require.Error(t, store.Success(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Pristine, ms)
		},
		"cannot go from failure to success": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Failure(ctx))
			require.Error(t, store.Success(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Failed, ms)
		},
		"cannot go from success to in-progress": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Success(ctx))
			require.Error(t, store.InProgress(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.Successful, ms)
		},
		"cannot go from in-progress to pristine": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.Error(t, store.Pristine(ctx))

			ms, err := store.MigrationStatus(ctx)
			require.NoError(t, err)
			assert.Equal(t, storage.InProgress, ms)
		},
		"can go from success to pristine": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Success(ctx))
			require.NoError(t, store.Pristine(ctx))
		},
		"can go from failure to pristine": func(t *testing.T) {
			require.NoError(t, store.InProgress(ctx))
			require.NoError(t, store.Failure(ctx))
			require.NoError(t, store.Pristine(ctx))
		},
		// This is debatable, let's see what suits us best
		"cannot go from pristine to pristine": func(t *testing.T) {
			require.Error(t, store.Pristine(ctx))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.flush(t)
	}
}

func TestPurgeSubjectFromPolicies(t *testing.T) {
	store, db, _ := setup(t)
	defer db.close(t)
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

			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE member_id=$1`, member.ID))
		}},
		{"one policy matches, returns this policy's ID", func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, subject)

			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.Equal(t, []string{polID}, ids)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE member_id=$1`, member.ID))
		}},
		{"two policies match, returns their IDs", func(t *testing.T) {
			polID0 := insertTestPolicy(t, db, "testpolicy0")
			polID1 := insertTestPolicy(t, db, "testpolicy1")
			member := insertTestPolicyMember(t, db, polID0, subject)
			_, err := db.Exec(`INSERT INTO iam_policy_members (policy_id, member_id) values($1, $2)`, polID1, member.ID)
			require.NoError(t, err)

			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.ElementsMatch(t, []string{polID0, polID1}, ids)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE member_id=$1`, member.ID))
		}},
		{"one policy matches, with extra members, those are kept intact", func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			member0 := insertTestPolicyMember(t, db, polID, subject)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:member1")

			ids, err := store.PurgeSubjectFromPolicies(ctx, subject)
			require.NoError(t, err)
			assert.Equal(t, []string{polID}, ids)

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE member_id=$1`, member0.ID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE member_id=$1`, member1.ID))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.flush(t)
	}
}

func assertProjectsMatch(t *testing.T, db *testDB, project storage.Project) {
	t.Helper()
	dbProject := storage.Project{}
	err := db.QueryRow(`SELECT query_project($1);`, project.ID).Scan(&dbProject)
	require.NoError(t, err)
	assert.Equal(t, project, dbProject)
}

func assertRolesMatch(t *testing.T, db *testDB, role storage.Role) {
	t.Helper()
	dbRole := storage.Role{}
	err := db.QueryRow(`SELECT query_role($1);`, role.ID).Scan(&dbRole)
	require.NoError(t, err)
	assert.Equal(t, role, dbRole)
}

func assertEmpty(t *testing.T, row *sql.Row) {
	assertCount(t, 0, row)
}

func assertOne(t *testing.T, row *sql.Row) {
	assertCount(t, 1, row)
}

func assertCount(t *testing.T, expected int, row *sql.Row) {
	t.Helper()
	require.NotNil(t, row)
	var count int
	require.NoError(t, row.Scan(&count))
	assert.Equal(t, expected, count)
}

func assertMembers(t *testing.T, db *testDB, policyID string, members []storage.Member) {
	t.Helper()
	for _, member := range members {
		assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_policy_members WHERE policy_id=$1 and member_id=$2`, policyID, member.ID))
		assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE id=$1 AND name=$2`, member.ID, member.Name))
	}
}

func assertPolicy(t *testing.T, expectedPolicy, returnedPolicy *storage.Policy) {
	assert.Equal(t, expectedPolicy.ID, returnedPolicy.ID)
	assert.Equal(t, expectedPolicy.Name, returnedPolicy.Name)
	assert.Equal(t, expectedPolicy.Type, returnedPolicy.Type)
	assert.ElementsMatch(t, expectedPolicy.Members, returnedPolicy.Members)
	assert.ElementsMatch(t, expectedPolicy.Projects, returnedPolicy.Projects)
	assert.ElementsMatch(t, expectedPolicy.Statements, returnedPolicy.Statements)
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

func genUUID(t *testing.T) uuid.UUID {
	t.Helper()
	id, err := uuid.New()
	require.NoError(t, err)
	return id
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

func insertTestPolicy(t *testing.T, db *testDB, policyName string) string {
	row := db.QueryRow(fmt.Sprintf("INSERT INTO iam_policies "+
		"(id, name) VALUES (uuid_generate_v4(), '%s') "+
		"RETURNING id", policyName))
	require.NotNil(t, row)
	var polID string
	err := row.Scan(&polID)
	require.NoError(t, err)
	return polID
}

// Will fail on conflict with existing name.
func insertTestPolicyMember(t *testing.T, db *testDB, polID string, memberName string) storage.Member {
	member := genMember(t, memberName)

	_, err := db.Exec(`INSERT INTO iam_members (id, name) values ($1, $2)`, member.ID, member.Name)
	require.NoError(t, err)
	_, err = db.Exec(`INSERT INTO iam_policy_members (policy_id, member_id) values($1, $2)`, polID, member.ID)
	require.NoError(t, err)

	return member
}

func insertTestRole(t *testing.T,
	db *testDB, id string, name string, actions []string, projects []string) storage.Role {

	role := genRole(t, id, name, actions, projects)

	row := db.QueryRow(`INSERT INTO iam_roles (id, name, type, actions)  VALUES ($1, $2, $3, $4)
	RETURNING db_id;`,
		role.ID, role.Name, role.Type.String(), pq.Array(role.Actions))
	var dbID string
	err := row.Scan(&dbID)
	require.NoError(t, err)

	for _, project := range role.Projects {
		_, err := db.Exec(`INSERT INTO iam_role_projects (role_id, project_id) VALUES ($1, $2)`,
			&dbID, &project)
		require.NoError(t, err)
	}

	return role
}

func insertTestProject(t *testing.T, db *testDB, id string, name string, projType storage.Type) storage.Project {
	t.Helper()
	proj, err := storage.NewProject(id, name, projType)
	require.NoError(t, err)

	_, err = db.Exec(`INSERT INTO iam_projects (id, name, type, projects) values ($1, $2, $3, $4)`,
		proj.ID, proj.Name, projType.String(), pq.Array([]string{proj.ID}))
	require.NoError(t, err)

	return proj
}

func insertPolicyProject(t *testing.T, db *testDB, policyID string, projectId string) {
	t.Helper()
	_, err := db.Exec(`
			INSERT INTO iam_policy_projects (policy_id, project_id) VALUES ($1, $2);`,
		policyID, projectId)
	require.NoError(t, err)
}

func insertStatementProject(t *testing.T, db *testDB, statementID uuid.UUID, projectId string) {
	t.Helper()
	_, err := db.Exec(`
			INSERT INTO iam_statement_projects (statement_id, project_id) VALUES ($1, $2);`,
		statementID, projectId)
	require.NoError(t, err)
}

func setup(t *testing.T) (storage.Storage, *testDB, *prng.Prng) {
	t.Helper()
	ctx := context.Background()
	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for postgres storage")

	migrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../../postgres/migration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	dataMigrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../../postgres/datamigration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	if migrationConfig == nil && dataMigrationConfig == nil {
		t.Skipf("start pg container and set PG_URL to run")
	}

	// reset database the hard way -- we do this to ensure that our comparison
	// between database content and hardcoded storage default policies actually
	// compares the migrated policies with the hardcoded ones (and NOT the
	// hardcoded policies with the hardcoded policies).
	db := openDB(t)
	_, err = db.ExecContext(ctx, resetDatabaseStatement)
	require.NoError(t, err, "error resetting database")
	_, err = db.Exec(`CREATE EXTENSION IF NOT EXISTS "uuid-ossp"`)
	require.NoError(t, err, "error creating extension")

	backend, err := postgres.New(ctx, l, *migrationConfig, datamigration.Config(*dataMigrationConfig))
	require.NoError(t, err)
	return backend, &testDB{DB: db}, prng.Seed(t)
}

// migrationConfigIfPGTestsToBeRun either returns the pg migration config
// if PG_URL is set or we are in CI, otherwise it returns nil, indicating
// postgres based tests shouldn't be run.
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

func openDB(t *testing.T) *sql.DB {
	t.Helper()
	db, err := sql.Open("postgres", "postgres://postgres:postgres@127.0.0.1:5432/authz_test?sslmode=disable")
	require.NoError(t, err, "error opening db")
	err = db.Ping()
	require.NoError(t, err, "error pinging db")

	return db
}

func insertProjectsIntoContext(ctx context.Context, projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(auth_context.NewContext(ctx,
		[]string{}, projects, "resource", "action", "pol"))
}

func (d *testDB) flush(t *testing.T) {
	_, err := d.Exec(`DELETE FROM iam_policies CASCADE; DELETE FROM iam_members CASCADE;
		DELETE FROM iam_roles CASCADE; DELETE FROM iam_projects CASCADE; DELETE FROM iam_role_projects CASCADE;
		DELETE FROM migration_status; INSERT INTO migration_status(state) VALUES ('init')`)
	require.NoError(t, err)
}

func (d *testDB) close(t *testing.T) {
	t.Helper()
	require.NoError(t, d.Close())
}
