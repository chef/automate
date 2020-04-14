package legacy

import (
	"context"
	"database/sql"
	"net/url"
	"os"
	"strings"
	"testing"

	"github.com/gofrs/uuid"
	_ "github.com/golang-migrate/migrate/database/postgres" // make driver available
	_ "github.com/golang-migrate/migrate/source/file"       // make source available

	constants "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v2"
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
	ciPGURL                       = "postgres://postgres@127.0.0.1:5432/authz_test?sslmode=disable"
)

const resetDatabaseStatement = `DROP SCHEMA public CASCADE;
CREATE SCHEMA public;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;`

func TestMigrateToV2(t *testing.T) {
	ctx := context.Background()
	db, err := setupDB(ctx, t)
	require.NoError(t, err, "setup db")
	// prng := prng.Seed(t)

	cases := map[string]func(*testing.T){
		"empty store/default state": func(t *testing.T) {
			err := MigrateToV2(ctx, db, true)
			require.NoError(t, err)

			for _, pol := range v2DefaultPolicies() {
				resp, err := queryTestPolicy(ctx, pol.ID, db)
				require.NoError(t, err)
				assert.Equal(t, pol.ID, resp.ID)
			}

			for _, pol := range v1LegacyPolicies() {
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

			err = MigrateToV2(ctx, db, true)
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
			assert.Equal(t, []string{"ingest:nodes"}, statement.Resources)

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

			err = MigrateToV2(ctx, db, true)
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

			err = MigrateToV2(ctx, db, true)
			v2PolicyCount, err := queryV2PolicyCount(ctx, db)
			require.NoError(t, err)
			// members should be added to default admin policy
			assert.Equal(t, v2DefaultAndLegacyPolicyCount, v2PolicyCount)

			adminPol, err := queryTestPolicy(ctx, constants.AdminPolicyID, db)
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

			err = MigrateToV2(ctx, db, true)
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

func TestSingleTermResourceMigration(t *testing.T) {
	ctx := context.Background()
	db, err := setupDB(ctx, t)
	require.NoError(t, err, "setup db")

	cases := map[string]func(*testing.T){
		"policies with the single term resource that granted permissions on v1 are migrated": func(t *testing.T) {
			singleTermResources := []string{
				"nodes",
				"events",
				"license",
				"nodemanagers",
				"service_groups",
			}
			nodesPolID, eventsPolID, licPolID := genUUID(t).String(), genUUID(t).String(), genUUID(t).String()
			nodeManID, svcGroupPol := genUUID(t).String(), genUUID(t).String()

			policyIDs := []string{nodesPolID, eventsPolID, licPolID, nodeManID, svcGroupPol}

			for i, resource := range singleTermResources {
				_, err := storePolicy(ctx, db, policyIDs[i], "*", []string{"user:ldap:bob"}, resource, "allow")
				require.NoError(t, err)
			}

			err = MigrateToV2(ctx, db, true)
			require.NoError(t, err)

			migratedResources := []string{
				"infra:nodes",
				"event:events",
				"system:license",
				"infra:nodeManagers",
				"applications:serviceGroups",
			}
			for j, id := range policyIDs {
				migratedPol, err := queryTestPolicy(ctx, id, db)
				require.NoError(t, err)
				require.NotNil(t, migratedPol)

				statement := migratedPol.Statements[0]
				assert.Equal(t, []string{migratedResources[j]}, statement.Resources)
			}
		},
		"any other policies with a single term resource are skipped": func(t *testing.T) {
			singleTermResources := []string{
				"auth",
				"service_info",
				"users",
				"auth_introspection",
				"cfgmgmt",
				"compliance",
				"ingest",
				"secrets",
				"telemetry",
				"notifications",
			}

			policyIDs := make([]string, len(singleTermResources))
			for i, resource := range singleTermResources {
				id := genUUID(t).String()
				policyIDs[i] = id

				_, err := storePolicy(ctx, db, id, "*", []string{"user:ldap:bob"}, resource, "allow")
				require.NoError(t, err)
			}

			err = MigrateToV2(ctx, db, true)
			require.NoError(t, err)

			for _, id := range policyIDs {
				migratedPol, err := queryTestPolicy(ctx, id, db)
				require.Error(t, err)
				require.Nil(t, migratedPol)
			}
		},
	}

	for desc, test := range cases {
		flush(t, db)
		t.Run(desc, test)
	}
}

func setupDB(ctx context.Context, t *testing.T) (*sql.DB, error) {
	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for postgres storage")

	// reset database the hard way -- we do this to ensure that our comparison
	// between database content and hardcoded storage default policies actually
	// compares the migrated policies with the hardcoded ones (and NOT the
	// hardcoded policies with the hardcoded policies).
	db, pgURL := openDB(t)

	// if db is nil, we are not in CI or we did not request the tests that require the db to be up
	if db == nil {
		t.Log("Not in CI and PGURL not passed, skipping tests that depend on an active db")
		t.SkipNow()
	}

	_, err = db.ExecContext(ctx, resetDatabaseStatement)
	require.NoError(t, err, "error resetting database")
	_, err = db.Exec(`CREATE EXTENSION IF NOT EXISTS "uuid-ossp"`)
	require.NoError(t, err, "error creating extension")

	migrationsPath := "../sql"
	migrationsTable := ""

	l.Infof("Running db migrations from %q", migrationsPath)
	purl, err := addMigrationsTable(pgURL, migrationsTable)
	if err != nil {
		return nil, errors.Wrap(err, "parse PG URL")
	}

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

func openDB(t *testing.T) (*sql.DB, string) {
	t.Helper()

	ciMode := os.Getenv("CI") == "true"

	var customPGURL string
	pgURLPassed := true
	if ciMode {
		customPGURL = ciPGURL
	} else {
		customPGURL, pgURLPassed = os.LookupEnv("PG_URL")
	}

	// If PG_URL wasn't passed (and we aren't in CI)
	// we shouldn't run the postgres tests, return nil.
	if !pgURLPassed {
		return nil, customPGURL
	}

	db, err := sql.Open("postgres", customPGURL)
	require.NoError(t, err, "error opening db")
	err = db.Ping()
	require.NoError(t, err, "error pinging db")

	return db, customPGURL
}

func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
}

func queryTestPolicy(ctx context.Context, id string, db *sql.DB) (*v2Policy, error) {
	tx, err := db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, errors.Wrap(err, "begin queryTestPolicy tx")
	}

	resp, err := queryPolicy(ctx, id, tx)
	if err != nil {
		// The not found on json unmarshall case
		if strings.HasPrefix(err.Error(), "sql: Scan error on column index 0") &&
			strings.HasSuffix(err.Error(), "not found") {
			return nil, errors.Errorf("policy not found %s", id)
		}
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

func genUUID(t *testing.T) uuid.UUID {
	t.Helper()
	i, err := uuid.NewV4()
	require.NoError(t, err)
	return i
}

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

// MemberSliceToStringSlice returns a slice of the
// names of members or an empty string slice if
// the member array is of length zero or nil.
func memberSliceToStringSlice(m []v2Member) []string {
	memberSlice := make([]string, len(m))
	for i, member := range m {
		memberSlice[i] = member.Name
	}
	return memberSlice
}

// v1LegacyPolicies is a consolidated list of the v1LegacyPolicies that are ported
// in the migratev1policies call. They-- like everything under legacy-- should
// remain untouched once this code is live.
func v1LegacyPolicies() []v2Policy {
	allUsers := v2Member{Name: "user:*"}
	allTokens := v2Member{Name: "token:*"}

	s1 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"compliance:*"})
	compliancePol := v2Policy{
		ID:         constants.CompliancePolicyID,
		Name:       "[Legacy] Compliance Access",
		Members:    []v2Member{allUsers},
		Statements: []v2Statement{s1},
		Type:       Custom,
	}

	s2 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"compliance:profiles:*"})
	complianceProfilePol := v2Policy{
		ID:         constants.ComplianceTokenPolicyID,
		Name:       "[Legacy] Compliance Profile Access",
		Members:    []v2Member{allTokens},
		Statements: []v2Statement{s2},
		Type:       Custom,
	}

	s3 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"event:*"})
	eventPol := v2Policy{
		ID:         constants.EventsPolicyID,
		Name:       "[Legacy] Events Access",
		Members:    []v2Member{allUsers},
		Statements: []v2Statement{s3},
		Type:       Custom,
	}

	s4 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"infra:*"})
	infraPol := v2Policy{
		ID:         constants.CfgmgmtPolicyID,
		Name:       "[Legacy] Infrastructure Automation Access",
		Members:    []v2Member{allUsers},
		Statements: []v2Statement{s4},
		Type:       Custom,
	}

	s5 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"ingest:*"})
	ingestPol := v2Policy{
		ID:         constants.IngestPolicyID,
		Name:       "[Legacy] Ingest Access",
		Members:    []v2Member{allTokens},
		Statements: []v2Statement{s5},
		Type:       Custom,
	}

	s6 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"infra:nodes:*"})
	nodePol := v2Policy{
		ID:         constants.NodesPolicyID,
		Name:       "[Legacy] Nodes Access",
		Members:    []v2Member{allUsers},
		Statements: []v2Statement{s6},
		Type:       Custom,
	}

	s7 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"infra:nodeManagers:*"})
	nodeManagerPol := v2Policy{
		ID:         constants.NodeManagersPolicyID,
		Name:       "[Legacy] Node Managers Access",
		Members:    []v2Member{allUsers},
		Statements: []v2Statement{s7},
		Type:       Custom,
	}

	s8 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"secrets:*"})
	secretPol := v2Policy{
		ID:         constants.SecretsPolicyID,
		Name:       "[Legacy] Secrets Access",
		Members:    []v2Member{allUsers},
		Statements: []v2Statement{s8},
		Type:       Custom,
	}

	s9 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"system:telemetryConfig:*"})
	telemetryPol := v2Policy{
		ID:         constants.TelemetryPolicyID,
		Name:       "[Legacy] Telemetry Access",
		Members:    []v2Member{allUsers},
		Statements: []v2Statement{s9},
		Type:       Custom,
	}

	return []v2Policy{
		compliancePol,
		complianceProfilePol,
		eventPol,
		infraPol,
		ingestPol,
		nodePol,
		nodeManagerPol,
		secretPol,
		telemetryPol,
	}
}
