package legacy

import (
	"context"
	"database/sql"
	"net/url"
	"testing"

	"github.com/gofrs/uuid"
	_ "github.com/golang-migrate/migrate/database/postgres" // make driver available
	_ "github.com/golang-migrate/migrate/source/file"       // make source available

	constants_v2 "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v2"
	"github.com/chef/automate/lib/logger"
	"github.com/golang-migrate/migrate"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const (
	v2DefaultAndLegacyPolicyCount = 13 // owner, editor, viewer, ingest + legacy
	v2DefaultRoleCount            = 4  // owner, editor, viewer, ingest
	v2DefaultProjectCount         = 2  // ~~All Projects~~, (unassigned)
	pgURL                         = "postgres://postgres@127.0.0.1:5432/authz_test?sslmode=disable"
)

func addMigrationsTable(u, table string) (string, error) {
	pgURL, err := url.Parse(u)
	if err != nil {
		return "", err
	}
	if table != "" {
		q := pgURL.Query()
		q.Set("x-migrations-table", table)
		pgURL.RawQuery = q.Encode()
	}
	return pgURL.String(), nil
}

func openDB(t *testing.T) *sql.DB {
	t.Helper()
	db, err := sql.Open("postgres", "postgres://postgres:postgres@127.0.0.1:5432/authz_test?sslmode=disable")
	require.NoError(t, err, "error opening db")
	err = db.Ping()
	require.NoError(t, err, "error pinging db")

	return db
}

const resetDatabaseStatement = `DROP SCHEMA public CASCADE;
CREATE SCHEMA public;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;`

func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
}

func setupDB(ctx context.Context, t *testing.T) (*sql.DB, error) {
	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for postgres storage")

	// reset database the hard way -- we do this to ensure that our comparison
	// between database content and hardcoded storage default policies actually
	// compares the migrated policies with the hardcoded ones (and NOT the
	// hardcoded policies with the hardcoded policies).
	db := openDB(t)
	_, err = db.ExecContext(ctx, resetDatabaseStatement)
	require.NoError(t, err, "error resetting database")
	_, err = db.Exec(`CREATE EXTENSION IF NOT EXISTS "uuid-ossp"`)
	require.NoError(t, err, "error creating extension")

	pgURL := pgURL
	migrationsPath := "../sql"
	migrationsTable := ""

	l.Infof("Running db migrations from %q", migrationsPath)
	purl, err := addMigrationsTable(pgURL, migrationsTable)
	if err != nil {
		return nil, errors.Wrap(err, "parse PG URL")
	}
	t.Log("DEBUG")
	t.Log(migrationsPath)
	t.Log(purl)
	scheme := addScheme(migrationsPath)
	t.Log(scheme)
	m, err := migrate.New(scheme, purl)
	if err != nil {
		return nil, errors.Wrap(err, "init migrator")
	}

	err = m.Migrate(74)
	if err != nil {
		return nil, errors.Wrap(err, "failed to migrate to pre-force-upgrade schema")
	}

	return db, nil
}

func TestMigrateToV2(t *testing.T) {
	ctx := context.Background()
	db, err := setupDB(ctx, t)
	require.NoError(t, err, "setup db")
	// prng := prng.Seed(t)

	cases := map[string]func(*testing.T){
		"empty store/default state": func(t *testing.T) {
			err := MigrateToV2(ctx, db)
			require.NoError(t, err)

			for _, pol := range v2DefaultPolicies() {
				resp, err := queryTestPolicy(ctx, pol.ID, db)
				require.NoError(t, err)
				assert.Equal(t, pol.ID, resp.ID)
			}

			for _, role := range defaultRoles() {
				resp, err := queryRole(ctx, db, role.ID)
				assert.NoError(t, err)
				assert.Equal(t, role.ID, resp.ID)
			}

		},
		"empty store, custom v1 policy with subjects is migrated": func(t *testing.T) {
			polID := genUUID(t)

			action := "create"
			subjects := []string{"user:ldap:bob", "team:ldap:ops"}
			resource := "ingest:nodes"
			effect := "allow"
			v1pol, err := storePolicy(ctx, db, polID.String(), action, subjects, resource, effect)
			require.NoError(t, err)
			require.NotNil(t, v1pol)

			err = MigrateToV2(ctx, db)
			require.NoError(t, err)

			v2PolicyCount, err := queryV2PolicyCount(ctx, db)
			require.NoError(t, err)

			assert.Equal(t, v2DefaultAndLegacyPolicyCount+1, v2PolicyCount)

			migratedPol, err := queryTestPolicy(ctx, polID.String(), db)
			require.NoError(t, err)

			assert.Equal(t, polID.String(), migratedPol.ID)
			assert.Equal(t, polID.String()+" (custom)", migratedPol.Name)
			assert.ElementsMatch(t, []string{"user:ldap:bob", "team:ldap:ops"}, memberSliceToStringSlice(migratedPol.Members))
			require.Equal(t, 1, len(migratedPol.Statements))
			statement := migratedPol.Statements[0]
			assert.Equal(t, Allow, statement.Effect)
			assert.Equal(t, []string{"*:create"}, statement.Actions)
			assert.Equal(t, []string{"infra:nodes"}, statement.Resources)

			err = deletePol(ctx, db, polID.String())
			require.NoError(t, err)
		},
		"empty store, custom v1 policy without subjects is not migrated": func(t *testing.T) {
			polID := genUUID(t)

			action := "create"
			subjects := []string{}
			resource := "ingest:nodes"
			effect := "allow"
			v1pol, err := storePolicy(ctx, db, polID.String(), action, subjects, resource, effect)
			require.NoError(t, err)
			require.NotNil(t, v1pol)

			err = MigrateToV2(ctx, db)
			require.NoError(t, err)

			v2PolicyCount, err := queryV2PolicyCount(ctx, db)
			require.NoError(t, err)

			assert.Equal(t, v2DefaultAndLegacyPolicyCount, v2PolicyCount)

			migratedPol, err := queryTestPolicy(ctx, polID.String(), db)
			require.Nil(t, migratedPol)
			require.Error(t, err)
		},
		"on admin token policy, adds members to admin policy": func(t *testing.T) {
			polID1 := genUUID(t)
			polID2 := genUUID(t)
			tok1 := "token:282f41f1-e763-4094-9c59-c4eec1b71532"
			tok2 := "token:382f41f1-e763-4094-9c59-c4eec1b71534"

			action := "*"
			subjects1 := []string{tok1}
			subjects2 := []string{tok2}
			resource := "*"
			effect := "allow"
			v1pol, err := storePolicy(ctx, db, polID1.String(), action, subjects1, resource, effect)
			require.NoError(t, err)
			require.NotNil(t, v1pol)
			v2pol, err := storePolicy(ctx, db, polID2.String(), action, subjects2, resource, effect)
			require.NoError(t, err)
			require.NotNil(t, v2pol)

			err = MigrateToV2(ctx, db)
			v2PolicyCount, err := queryV2PolicyCount(ctx, db)
			require.NoError(t, err)
			// members should be added to default admin policy
			assert.Equal(t, v2DefaultAndLegacyPolicyCount, v2PolicyCount)

			adminPol, err := queryTestPolicy(ctx, constants_v2.AdminPolicyID, db)
			require.NoError(t, err)
			require.NotNil(t, adminPol)

			memberNames := make([]string, len(adminPol.Members))
			for _, mem := range adminPol.Members {
				memberNames = append(memberNames, mem.Name)
			}
			assert.Contains(t, memberNames, tok1)
			assert.Contains(t, memberNames, tok2)

			err = deletePol(ctx, db, polID2.String())
			require.NoError(t, err)
			err = deletePol(ctx, db, polID1.String())
			require.NoError(t, err)
		},
		"two unconvertible custom v1 policies do not prevent migration": func(t *testing.T) {
			polID1 := genUUID(t)
			polID2 := genUUID(t)
			valPolID := genUUID(t)
			invalidRes := "injest:nodes"
			invalidAct := "mewantfood"
			invV1pol1, err := storePolicy(ctx, db, polID1.String(), "create", []string{"user:ldap:bob"}, invalidRes, "allow")
			require.NoError(t, err)
			require.NotNil(t, invV1pol1)
			invV1pol2, err := storePolicy(ctx, db, polID2.String(), invalidAct, []string{"user:ldap:bob"}, "ingest:nodes", "allow")
			require.NoError(t, err)
			require.NotNil(t, invV1pol2)
			validV1pol, err := storePolicy(ctx, db, valPolID.String(), "create", []string{"user:ldap:bob"}, "ingest:nodes", "allow")
			require.NoError(t, err)
			require.NotNil(t, validV1pol)

			err = MigrateToV2(ctx, db)
			require.NoError(t, err)

			v2PolicyCount, err := queryV2PolicyCount(ctx, db)
			require.NoError(t, err)

			assert.Equal(t, v2DefaultAndLegacyPolicyCount+1, v2PolicyCount)

			// valid one is migrated
			valid, err := queryTestPolicy(ctx, valPolID.String(), db)
			require.NoError(t, err)
			require.NotNil(t, valid)

			err = deletePol(ctx, db, valPolID.String())
			require.NoError(t, err)
		},
	}

	for desc, test := range cases {
		flush(t, db)
		t.Run(desc, test)
	}
}

func queryTestPolicy(ctx context.Context, id string, db *sql.DB) (*v2Policy, error) {
	tx, err := db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, errors.Wrap(err, "begin queryTestPolicy tx")
	}

	resp, err := queryPolicy(ctx, id, tx)
	if err != nil {
		return nil, errors.Wrap(err, "query policy")
	}

	err = tx.Commit()
	if err != nil {
		return nil, errors.Wrap(err, "end queryTestPolicy tx")
	}

	return resp, nil
}

func queryRole(ctx context.Context, db *sql.DB, id string) (*v2Role, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, errors.Wrap(err, "could not begin transaction")
	}

	var role v2Role
	row := tx.QueryRowContext(ctx, `SELECT query_role($1);`, id)
	err = row.Scan(&role)
	if err != nil {
		return nil, errors.Wrap(err, "could not query role")
	}

	err = tx.Commit()
	if err != nil {
		return nil, errors.Wrap(err, "could not commit query role tx")
	}

	return &role, nil
}

func flush(t *testing.T, db *sql.DB) {
	_, err := db.Exec(`DELETE FROM iam_policies CASCADE;
		DELETE FROM iam_members CASCADE;
		DELETE FROM iam_roles CASCADE;
		DELETE FROM iam_projects CASCADE;
		DELETE FROM iam_projects_graveyard CASCADE;`)
	require.NoError(t, err)
}

// func getMigrationStatus(ctx, db *sql.db) (err error, status string) {
// 	r, err := db.ExecContext(ctx, "SELECT * FROM migration_status")
// 	if err != nil {
// 		return err
// 	}
// 	status, err = r.Scan(&r)
// 	return err, status
// }

// func getPolicyFromStore(t *testing.T, store *cache.Cache, id string) v2Policy {
// 	t.Helper()
// 	storedPol, ok := store.Get(id)
// 	require.True(t, ok, "stored in cache")
// 	pol, ok := storedPol.(*storage.Policy)
// 	require.True(t, ok, "cannot cast to Policy")
// 	return *pol
// }

// func assertProjectsMatch(t *testing.T, storageProject string, apiProject string) {
// 	if storageProject == constants_v2.AllProjectsID {
// 		storageProject = constants_v2.AllProjectsExternalID
// 	}
// 	assert.Equal(t, storageProject, apiProject, "statement projects differ")
// }

// func assertStatementsMatch(t *testing.T, storageStatement storage.Statement, apiStatement api_v2.Statement) {
// 	if storageStatement.Actions != nil && apiStatement.Actions != nil {
// 		assert.Equal(t, storageStatement.Actions, apiStatement.Actions, "statement actions differ")
// 	}
// 	assert.Equal(t, int(storageStatement.Effect), int(apiStatement.Effect), "statement effects differ")
// 	if storageStatement.Resources != nil && apiStatement.Resources != nil {
// 		assert.Equal(t, storageStatement.Resources, apiStatement.Resources, "statement resources differ")
// 	}
// 	assert.Equal(t, storageStatement.Role, apiStatement.Role, "statement roles differ")
// 	// This allows for grpc return of []string(nil) being compared to []string{}
// 	if len(storageStatement.Projects) != 0 || len(apiStatement.Projects) != 0 {
// 		for i, project := range storageStatement.Projects {
// 			assertProjectsMatch(t, project, apiStatement.Projects[i])
// 		}
// 	}
// }

// func assertPoliciesMatch(t *testing.T, storagePolicy *storage.Policy, apiPolicy *api_v2.Policy) {
// 	assert.Equal(t, storagePolicy.Name, apiPolicy.Name, "policy names differ")
// 	assert.Equal(t, storagePolicy.Type.String(), strings.ToLower(apiPolicy.Type.String()), "policy types differ")
// 	assert.Equal(t, len(storagePolicy.Members), len(apiPolicy.Members), "number of policy members differ")
// 	assertMembersMatch(t, storagePolicy.Members, apiPolicy.Members)
// 	assert.Equal(t, len(storagePolicy.Statements), len(apiPolicy.Statements), "number of policy statements differ")
// 	for i, statement := range storagePolicy.Statements {
// 		assertStatementsMatch(t, statement, *apiPolicy.Statements[i])
// 	}
// }

// func assertMembersMatch(t *testing.T, storageMembers []storage.Member, apiMembers []string) {
// 	if storageMembers == nil {
// 		assert.Nil(t, apiMembers, "policy members differ (nil)")
// 	} else {
// 		for i, member := range storageMembers {
// 			assert.Equal(t, member.Name, apiMembers[i], "policy members differ (slice value)")
// 		}
// 	}
// }

// func getRoleFromStore(t *testing.T, store *cache.Cache, id string) storage.Role {
// 	t.Helper()
// 	storedRole, ok := store.Get(id)
// 	require.True(t, ok, "stored in cache")
// 	role, ok := storedRole.(*storage.Role)
// 	require.True(t, ok, "cannot cast to Role")
// 	return *role
// }

// func assertRolesMatch(t *testing.T, storageRole storage.Role, apiRole api_v2.Role) {
// 	assert.Equal(t, storageRole.ID, apiRole.Id, "role names differ")
// 	assert.Equal(t, storageRole.Name, apiRole.Name, "role names differ")
// 	assert.Equal(t, storageRole.Type.String(), strings.ToLower(apiRole.Type.String()), "role types differ")
// 	assert.Equal(t, storageRole.Actions, apiRole.Actions, "role actions differ")
// 	assert.Equal(t, storageRole.Projects, apiRole.Projects, "role projects differ")
// }

// Mini-factory for policy generation.
// The id is optional; if a zero value is given, a real one will be generated
// func genPolicy(t *testing.T, id string, p *prng.Prng) storage.Policy {

// 	const ( // no special significance to these constant choices
// 		maxMembers          = 10
// 		maxStatements       = 25
// 		maxActions          = 5
// 		maxResources        = 3
// 		maxProjects         = 10
// 		maxPolicyNameLength = 50
// 	)
// 	effects := []storage.Effect{storage.Allow, storage.Deny}
// 	types := []storage.Type{storage.Custom, storage.ChefManaged}
// 	faker := faker.NewWithSeed(p)

// 	if id == "" {
// 		id = faker.Lorem().Word()
// 	}

// 	memberCount := rand.Intn(maxMembers)
// 	var members []storage.Member
// 	for i := 0; i <= memberCount; i++ {
// 		memberStr := fmt.Sprintf("%s:%s:%s-%d",
// 			[]string{"user", "team"}[rand.Intn(1)],
// 			[]string{"local", "ldap", "saml"}[rand.Intn(2)],
// 			faker.Lorem().Word(),
// 			i) // add i, Word() could yield duplicates
// 		members = append(members, genMember(t, memberStr))
// 	}

// 	statementCount := 1 + rand.Intn(maxStatements)
// 	statements := make([]storage.Statement, statementCount)
// 	for i := range statements {

// 		actionCount := 1 + rand.Intn(maxActions-1) // store will have [1, 5] actions
// 		actions := make([]string, actionCount)
// 		for i := range actions {
// 			actions[i] = fmt.Sprintf("%s:%s:%s-%d",
// 				faker.Lorem().Word(), faker.Lorem().Word(), faker.Lorem().Word(), i)
// 		}

// 		resourceCount := rand.Intn(maxResources)
// 		resources := make([]string, resourceCount)
// 		for i := range resources {
// 			resources[i] = fmt.Sprintf("%s:%s:%s-%d",
// 				faker.Lorem().Word(), faker.Lorem().Word(), faker.Lorem().Word(), i)
// 		}

// 		projectCount := 1 + rand.Intn(maxProjects)
// 		projects := make([]string, projectCount)
// 		for i := range projects {
// 			projects[i] = fmt.Sprintf("%s-%d",
// 				faker.Lorem().Word(), i)
// 		}

// 		statements[i] = storage.Statement{
// 			Actions:   actions,
// 			Resources: resources,
// 			Role:      faker.Lorem().Word(),
// 			Projects:  projects,
// 			Effect:    effects[rand.Intn(len(effects))],
// 		}
// 	}

// 	return storage.Policy{
// 		ID:         id,
// 		Name:       faker.Lorem().Text(maxPolicyNameLength),
// 		Members:    members,
// 		Type:       types[rand.Intn(len(types))],
// 		Statements: statements,
// 	}
// }

// func genRole(t *testing.T, id string, p *prng.Prng) storage.Role {
// 	t.Helper()

// 	const maxActions = 5

// 	faker := faker.NewWithSeed(p)

// 	if id == "" {
// 		id = faker.Lorem().Word()
// 	}

// 	name := faker.Lorem().Word()
// 	actionCount := 1 + rand.Intn(maxActions-1) // store will have [1, 5] actions
// 	actions := make([]string, actionCount)
// 	for i := range actions {
// 		actions[i] = fmt.Sprintf("%s:%s:%s",
// 			faker.Lorem().Word(), faker.Lorem().Word(), faker.Lorem().Word())
// 	}

// 	role, err := storage.NewRole(id, name, storage.Custom, actions, []string{})
// 	require.NoError(t, err)
// 	return *role
// }

// func genMember(t *testing.T, name string) storage.Member {
// 	t.Helper()
// 	member, err := storage.NewMember(name)
// 	require.NoError(t, err)
// 	return member
// }

// func addArbitraryRoleToStore(t *testing.T, store *cache.Cache, p *prng.Prng) storage.Role {
// 	return addArbitraryRolesToStore(t, store, p, 1)[0]
// }

// func addArbitraryRolesToStore(t *testing.T, store *cache.Cache, p *prng.Prng, n int) []storage.Role {
// 	roles := make([]storage.Role, n)
// 	for i := 0; i < n; i++ {
// 		role := genRole(t, "", p)
// 		role.ID = fmt.Sprintf("%s-%d", role.ID, i)
// 		store.Add(role.ID, &role, cache.NoExpiration)
// 		roles[i] = role
// 	}
// 	return roles
// }

// func addArbitraryPolicyToStore(t *testing.T, store *cache.Cache, p *prng.Prng) storage.Policy {
// 	return addArbitraryPoliciesToStore(t, store, p, 1)[0]
// }

// func addArbitraryPoliciesToStore(t *testing.T, store *cache.Cache, p *prng.Prng, n int) []storage.Policy {
// 	policies := make([]storage.Policy, n)
// 	for i := 0; i < n; i++ {
// 		policyToStore := genPolicy(t, "", p)
// 		policyToStore.ID = fmt.Sprintf("%s-%d", policyToStore.ID, i)
// 		// API can only add custom type
// 		policyToStore.Type = storage.Custom
// 		store.Add(policyToStore.ID, &policyToStore, cache.NoExpiration)
// 		policies[i] = policyToStore
// 	}
// 	return policies
// }

// func addSomePoliciesToStore(t *testing.T, store *cache.Cache, p *prng.Prng) (storage.Policy, map[string]storage.Policy) {
// 	itemCount := 3 + rand.Intn(5) // store will have [3, 8) elements
// 	policies := make(map[string]storage.Policy, itemCount)
// 	targetIndex := rand.Intn(itemCount)
// 	var targetPol storage.Policy
// 	for i, pol := range addArbitraryPoliciesToStore(t, store, p, itemCount) {
// 		policies[pol.ID] = pol
// 		if i == targetIndex {
// 			targetPol = pol
// 		}
// 	}
// 	return targetPol, policies
// }

// func addSomeRolesToStore(t *testing.T, store *cache.Cache, p *prng.Prng) (storage.Role, map[string]storage.Role) {
// 	itemCount := 3 + rand.Intn(5) // store will have [3, 8) elements
// 	roles := make(map[string]storage.Role, itemCount)
// 	targetIndex := rand.Intn(itemCount)
// 	var targetRole storage.Role
// 	for i, role := range addArbitraryRolesToStore(t, store, p, itemCount) {
// 		roles[role.ID] = role
// 		if i == targetIndex {
// 			targetRole = role
// 		}
// 	}
// 	return targetRole, roles
// }

// func generateTestPolicies(ctx context.Context, t *testing.T,
// 	cl api_v2.PoliciesClient, policies []*api_v2.CreatePolicyReq) []string {

// 	t.Helper()
// 	policyResponses := make([]string, len(policies))
// 	for i, req := range policies {
// 		resp, err := cl.CreatePolicy(ctx, req)
// 		if assert.NoError(t, err) {
// 			policyResponses[i] = resp.Id
// 		}
// 	}
// 	return policyResponses
// }

// func assertInterfaceMapContainsPolicy(t *testing.T,
// 	data map[string]interface{}, target *api_v2.CreatePolicyReq) bool {

// 	t.Helper()

// 	targetDatum := map[string]interface{}{
// 		"name":       target.Name,
// 		"members":    target.Members,
// 		"statements": target.Statements,
// 	}
// 	for _, policy := range data {
// 		if reflect.DeepEqual(policy, targetDatum) {
// 			return true
// 		}
// 	}
// 	return false
// }

// func id(t *testing.T, p *prng.Prng) string {
// 	t.Helper()
// 	faker := faker.NewWithSeed(p)
// 	return faker.Lorem().Word() + "-" + faker.Lorem().Word()
// }

func genUUID(t *testing.T) uuid.UUID {
	t.Helper()
	i, err := uuid.NewV4()
	require.NoError(t, err)
	return i
}

// func wellknown(t *testing.T, wellknownID string) *storage_v1.Policy {
// 	t.Helper()
// 	v1DefaultPols, err := storage_v1.DefaultPolicies()
// 	require.NoError(t, err)
// 	inputPol, found := v1DefaultPols[wellknownID]
// 	require.True(t, found)
// 	return inputPol
// }

// need

func storePolicy(
	ctx context.Context, db *sql.DB, id string,
	action string, subjects []string, resource string, effect string,
) (*v1Policy, error) {

	pm := policyMap{
		Subjects: subjects,
		Action:   action,
		Resource: resource,
		Effect:   effect,
	}

	pol := dbPolicy{}
	r := db.QueryRowContext(ctx,
		`INSERT INTO policies (id, policy_data, version)
					VALUES ($1, $2, $3)
					RETURNING id, policy_data`,
		id, pm, 1,
	)

	err := r.Scan(&pol.ID, &pol.PolicyData)
	if err != nil {
		return nil, errors.Wrap(err, "could not insert test policy")
	}

	return toStoragePolicy(pol), nil
}

func queryV2PolicyCount(ctx context.Context, db *sql.DB) (int, error) {
	var count int
	r := db.QueryRowContext(ctx, "SELECT COUNT(*) from iam_policies")
	if err := r.Scan(&count); err != nil {
		return 0, err
	}

	return count, nil
}

func deletePol(ctx context.Context, db *sql.DB, id string) error {
	_, err := db.ExecContext(ctx, "DELETE FROM policies WHERE id=$1", id)
	return err
}

func queryV1PolicyID(ctx context.Context, db *sql.DB, id string) (*v1Policy, error) {
	pol := dbPolicy{}

	r := db.QueryRowContext(ctx, "SELECT id FROM policies WHERE id=$1", id)
	if err := r.Scan(&pol.ID); err != nil {
		return nil, err
	}
	return toStoragePolicy(pol), nil
}
