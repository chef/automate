package postgres_test

import (
	"context"
	"database/sql"
	"fmt"
	"net/url"
	"os"
	"sort"
	"testing"
	"time"

	"github.com/davecgh/go-spew/spew"
	_ "github.com/lib/pq" // sql driver for postgres
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	constants "github.com/chef/automate/components/authz-service/constants/v1"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage "github.com/chef/automate/components/authz-service/storage/v1"
	"github.com/chef/automate/components/authz-service/storage/v1/postgres"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
)

const resetDatabaseStatement = `DROP SCHEMA public CASCADE;
CREATE SCHEMA public;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp"
`

func TestPostgres(t *testing.T) {
	ctx := context.Background()
	// Note: to set up PG locally for running these tests, run the following from your command line:
	// docker run --name authz-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=authz_test -p 5432:5432 -d postgres:9
	// (from components/authz-service) PG_URL="postgres://postgres:postgres@127.0.0.1:5432/authz_test?sslmode=disable" make test
	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "init logger for postgres storage")

	migrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../../postgres/migration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	dataMigrationConfig, err := migrationConfigIfPGTestsToBeRun(l, "../../postgres/datamigration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg data config for tests: %s", err.Error())
	}

	if migrationConfig == nil {
		t.Skipf("start pg container and set PG_URL to run")
	}

	// reset database the hard way -- we do this to ensure that our comparison
	// between database content and hardcoded storage default policies actually
	// compares the migrated policies with the hardcoded ones (and NOT the
	// hardcoded policies with the hardcoded policies).
	db := openDB(t)
	_, err = db.ExecContext(ctx, resetDatabaseStatement)
	require.NoError(t, err)

	backend, err := postgres.New(ctx, l, *migrationConfig, datamigration.Config(*dataMigrationConfig))
	require.NoError(t, err)

	// ! \\ this needs to happen BEFORE the reset
	t.Run("ensure hardcoded policies are in sync", func(t *testing.T) {
		pols, err := backend.ListPolicies(ctx)
		require.NoError(t, err)
		actual := []*storage.Policy{}
		for _, pol := range pols {
			// We reset CreatedAt, otherwise we'd have to work harder to compare
			// below: now, we can just use assert.ElementsMatch
			pol.CreatedAt = time.Time{}
			actual = append(actual, pol)
		}

		defaults, err := storage.DefaultPolicies()
		require.NoError(t, err)
		expected := []*storage.Policy{}
		for _, def := range defaults {
			expected = append(expected, def)
		}
		sort.Sort(polArray(expected))
		sort.Sort(polArray(actual))
		assert.Equal(t, expected, actual)
	})

	// Reset db before each test. This will restore default policies.
	r := backend.(storage.Resetter)

	testFuncs := map[string]func(*testing.T, storage.Storage, context.Context){
		"ListPolicies":             testListPolicies,
		"StorePolicy":              testStorePolicy,
		"DeletePolicy":             testDeletePolicy,
		"PurgeSubjectFromPolicies": testPurgeSubjectFromPolicies,
	}
	for name, test := range testFuncs {
		t.Run(name, func(t *testing.T) {
			if err := r.Reset(ctx); err != nil {
				t.Fatalf("reset postgres: %s", err)
			}
			t.Run("empty database besides default policies", func(t *testing.T) {
				resp, err := backend.ListPolicies(ctx)
				require.NoError(t, err)
				if !assert.Equalf(t, len(constants.DefaultPolicyIDs), len(resp), "expected only default policies") {
					dumpResp(t, resp)
				}
			})
			test(t, backend, ctx)
		})
	}
}

func dumpResp(t *testing.T, resp interface{}) {
	t.Errorf("resp: %s", spew.Sdump(resp))
}

func testListPolicies(t *testing.T, s storage.Storage, ctx context.Context) {
	t.Run("database with default policies", func(t *testing.T) {
		resp, err := s.ListPolicies(ctx)
		require.NoError(t, err)
		if !assert.Equalf(t, len(constants.DefaultPolicyIDs), len(resp), "expected one policy plus defaults") {
			dumpResp(t, resp)
		}
	})
}

func testStorePolicy(t *testing.T, s storage.Storage, ctx context.Context) {
	t.Run("database with one row in addition to default policies", func(t *testing.T) {
		resp, err := s.StorePolicy(
			ctx,
			"*",
			[]string{"team:local:other"},
			"auth:*",
			"allow",
		)

		require.NoError(t, err)
		assert.IsType(t, uuid.UUID{}, resp.ID)
		assert.WithinDuration(t, time.Now(), resp.CreatedAt, time.Minute)
		assert.Equal(t, time.Time{}, resp.UpdatedAt) // it's uninitialized
		assert.Equal(t, []string{"team:local:other"}, resp.Subjects)
		assert.Equal(t, "auth:*", resp.Resource)
		assert.Equal(t, "*", resp.Action)
		assert.Equal(t, "allow", resp.Effect)
	})
}

func testDeletePolicy(t *testing.T, s storage.Storage, ctx context.Context) {
	db := openDB(t)

	t.Run("database with one row", func(t *testing.T) {
		u := uuid.Must(uuid.NewV4())
		p := []byte(`{"subjects": ["team:local:admins"], "action": "*", "resource": "auth:*", "effect": "allow"}`)
		_, err := db.Exec(`INSERT INTO policies (id, policy_data, version)
                         VALUES ($1, $2, $3)`, u, p, storage.Version)
		require.NoError(t, err, "arrange: add row")

		resp, err := s.DeletePolicy(ctx, u.String())
		require.NoError(t, err)
		assert.Equal(t, u, resp.ID)
		assert.WithinDuration(t, time.Now(), resp.CreatedAt, time.Minute)
		assert.Equal(t, []string{"team:local:admins"}, resp.Subjects)
		assert.Equal(t, "auth:*", resp.Resource)
		assert.Equal(t, "*", resp.Action)
		assert.Equal(t, "allow", resp.Effect)

		t.Run("empty database besides default policies", func(t *testing.T) {
			resp, err := s.ListPolicies(ctx)
			require.NoError(t, err)
			if !assert.Equalf(t, len(constants.DefaultPolicyIDs), len(resp), "expected only default policies") {
				dumpResp(t, resp)
			}
		})
	})

	t.Run("database returns ErrNotFound for non-existant policy", func(t *testing.T) {
		_, err := s.DeletePolicy(ctx, uuid.Must(uuid.NewV4()).String())
		assert.Equal(t, storage_errors.ErrNotFound, err)
	})

	t.Run("database returns ErrCannotDelete when attempting to delete non-deletable policy", func(t *testing.T) {
		_, err := s.DeletePolicy(ctx, constants.AdminPolicyID)
		assert.Equal(t, storage_errors.ErrCannotDelete, err)
	})

	t.Run("database error returns internal", func(t *testing.T) {
		_, err := s.DeletePolicy(ctx, "not-a-uuid")
		assert.Equal(t, storage_errors.ErrDatabase, err)
	})
}

func testPurgeSubjectFromPolicies(t *testing.T, s storage.Storage, ctx context.Context) {
	t.Run("database with only default policies", func(t *testing.T) {
		noopCases := map[string]string{
			"subject not in policy":           "user:local:purge",
			"subject in non-deletable policy": "team:local:admins",
		}
		for desc, tc := range noopCases {
			t.Run(desc, func(t *testing.T) {
				resp, err := s.PurgeSubjectFromPolicies(ctx, tc)
				require.NoError(t, err)
				assert.Empty(t, resp)

				listResp, err := s.ListPolicies(ctx)
				require.NoError(t, err)
				if !assert.Equalf(t, len(constants.DefaultPolicyIDs), len(listResp), "expected only default policies") {
					dumpResp(t, listResp)
				}
			})
		}
	})

	t.Run("with matching policies", func(t *testing.T) {
		cases := map[string]string{
			"single match": `["user:local:alice"]`,
			"first match":  `["user:local:alice", "user:local:bob"]`,
			"last match":   `["user:local:bob", "user:local:alice"]`,
		}

		for desc, tc := range cases {
			t.Run(desc, func(t *testing.T) {
				db := openDB(t)
				u := uuid.Must(uuid.NewV4())
				p := []byte(fmt.Sprintf(`{"subjects": %s, "action": "*", "resource": "auth:*", "effect": "allow"}`, tc))
				_, err := db.ExecContext(ctx, `INSERT INTO policies (id, policy_data, version) VALUES ($1, $2, $3)`,
					u, p, storage.Version)
				require.NoError(t, err, "arrange: add row")
				defer func() {
					_, err := db.ExecContext(ctx, `DELETE FROM policies WHERE id=$1`, u)
					require.NoError(t, err, "cleanup")
				}()

				resp, err := s.PurgeSubjectFromPolicies(ctx, "user:local:alice")
				require.NoError(t, err)
				assert.Equal(t, []uuid.UUID{u}, resp)

				listResp, err := s.ListPolicies(ctx)
				require.NoError(t, err)
				if !assert.Equalf(t, len(constants.DefaultPolicyIDs)+1, len(listResp), "expected only default policies + 1") {
					dumpResp(t, listResp)
				}
				for _, pol := range listResp {
					if pol.ID == u {
						assert.WithinDuration(t, time.Now(), pol.UpdatedAt, time.Second)
					}
				}

				c := 0
				err = db.QueryRowContext(ctx,
					`SELECT COUNT(*) FROM policies WHERE policy_data->'subjects' ? 'user:local:alice'`,
				).Scan(&c)
				require.NoError(t, err)
				assert.Zero(t, c, "expected no policy with that subject left")
			})
		}
	})
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

type polArray []*storage.Policy

func (ps polArray) Less(i, j int) bool {
	return ps[i].ID.String() <= ps[j].ID.String()
}

func (ps polArray) Swap(i, j int) {
	ps[i], ps[j] = ps[j], ps[i]
}

func (ps polArray) Len() int {
	return len(ps)
}
