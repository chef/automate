package postgres_test

import (
	"context"
	"database/sql"
	"sort"
	"testing"

	"github.com/jaswdr/faker"
	"github.com/lib/pq"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/prng"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/testhelpers"
	"github.com/chef/automate/lib/grpc/auth_context"
)

const (
	Applied          = "applied"
	Staged           = "staged"
	SuperuserSubject = "tls:service:deployment-service:internal"
)

const (
	policyMembersByMemberName = "SELECT count(*) FROM iam_policy_members WHERE member_id=member_db_id($1)"
	policyMembersFull         = "SELECT count(*) FROM iam_policy_members WHERE policy_id=policy_db_id($1) and member_id=member_db_id($2)"
	policyMembersByPolicyID   = "SELECT count(*) FROM iam_policy_members WHERE policy_id=policy_db_id($1)"
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
	return auth_context.NewOutgoingContext(auth_context.NewContext(ctx, subjects, projects, "res", "act"))
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
