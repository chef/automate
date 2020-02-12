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

func openDB(t *testing.T) *sql.DB {
	t.Helper()
	db, err := sql.Open("postgres", "postgres://postgres:postgres@127.0.0.1:5432/authz_test?sslmode=disable")
	require.NoError(t, err, "error opening db")
	err = db.Ping()
	require.NoError(t, err, "error pinging db")

	return db
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
