package pg

import (
	"context"
	"database/sql"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

// runMigrations tries to execute all the migrations we know of
func runMigrations(db *sql.DB, url, path string) error {
	// We're migrating to golang-migrate/migrate instead of this hand-rolled code:
	//
	// If it exists, we're migrating the existing migration table to a 'schema_migrations'
	// table conforming to what golang-migrate expects.
	// When this is done, we drop that table, so the code here will only run once.
	// We then run the usual "up" migration code, which will pick up where we've left off.
	// Existing migrations have been converted into up.sql files accordingly, keeping
	// the numbering intact.

	ctx := context.TODO()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		return errors.Wrap(err, "beginning transaction")
	}

	if _, err := tx.ExecContext(ctx, `CREATE TABLE IF NOT EXISTS schema_migrations
(version BIGINT NOT NULL PRIMARY KEY, dirty BOOLEAN NOT NULL)`); err != nil {
		return errors.Wrap(err, "create new migration table 'schema_migrations'")
	}

	// Note(sr) This needs to happen in a function because the "migrations" table might
	// have already been dropped (i.e. on all but the first run), and such checks can only
	// happen in functions. Just letting it crash is not an option since it aborts our
	// transaction.
	const defineMigrationFunction = `CREATE OR REPLACE FUNCTION move_if_exists() RETURNS void AS
$$
BEGIN
  IF EXISTS(SELECT * FROM information_schema.tables
              WHERE table_schema = current_schema()
              AND table_name = 'migrations') THEN
     INSERT INTO schema_migrations (
       SELECT MAX(num) AS version, FALSE AS dirty FROM migrations
     );
  END IF;
END;
$$
LANGUAGE plpgsql;`

	if _, err := tx.ExecContext(ctx, defineMigrationFunction); err != nil {
		return errors.Wrap(err, "create move_if_exists function")
	}
	if _, err := tx.ExecContext(ctx, "SELECT move_if_exists()"); err != nil {
		return errors.Wrap(err, "converting migration table columns")
	}
	if _, err := tx.ExecContext(ctx, "DROP TABLE IF EXISTS migrations"); err != nil {
		return errors.Wrap(err, "dropping migration table")
	}

	if err := tx.Commit(); err != nil {
		return errors.Wrap(err, "commit migration table migration")
	}

	return errors.Wrap(migrator.Migrate(url, path, logger.NewLogrusStandardLogger(), false), "run migrations")
}
