package migration_test

import (
	"net/url"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/session-service/migration"
	"github.com/chef/automate/lib/logger"
)

const wantLogs = false

// TestMigrations only tries to migrate everything UP once. This is supposed to
// ALWAYS be run in CI pipeline, since we'll always have a fresh DB there.
// For local development, this test will be SKIPPED iff the connection attempt
// fails.
// To use this for testing migrations, set your PG_URL env variable to point to
// a postgres instance
func TestMigrations(t *testing.T) {
	var l logger.Logger
	var err error
	if wantLogs {
		l, err = logger.NewLogger("text", "debug")
		if err != nil {
			t.Fatal(err)
		}
	} else {
		l = logger.NewTestLogger()
	}

	mustParse := func(s string) *url.URL {
		u, err := url.Parse(s) // nolint: vetshadow
		if err != nil {
			t.Fatalf("parse URL %v: %s", s, err)
		}
		return u
	}

	pgURLGiven := false
	// Note: this matches CI's postgresql
	pgURL := "postgresql://postgres@127.0.0.1:5432/sessions_test?sslmode=disable"
	if v, found := os.LookupEnv("PG_URL"); found {
		pgURL = v
		pgURLGiven = true
	}
	mig := migration.Config{
		Path:   "sql",
		Logger: l,
		PG:     mustParse(pgURL),
	}

	err = mig.Go()
	if err != nil {
		if pgURLGiven || os.Getenv("CI") == "true" {
			// we're on CI system, or we've been given a PG_URL explicitly, so everything
			// is fatal
			assert.Nil(t, err)
		} else {
			t.Logf("Cannot reach postgres at %s, skipping migrations tests", pgURL)
			t.Log(`
-------------------------------------------------------------------------------------------------
To run these tests, set env var PG_URL="postgresql://user:pass@ip:port/tablename?sslmode=disable"
-------------------------------------------------------------------------------------------------`)
			t.Skipf("no postgres available: %s", err)
		}
	}
}
