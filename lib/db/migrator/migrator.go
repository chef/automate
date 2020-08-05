package migrator

import (
	"net/url"

	"github.com/golang-migrate/migrate"
	_ "github.com/golang-migrate/migrate/database/postgres" // make driver available
	_ "github.com/golang-migrate/migrate/source/file"       // make source available
	"github.com/pkg/errors"

	"github.com/chef/automate/lib/logger"
)

type migrationLog struct {
	logger.Logger
	verbose bool
}

func (l migrationLog) Verbose() bool {
	return l.verbose
}

// Migrate executes all migrations we have
func Migrate(pgURL, migrationsPath string, l logger.Logger, verbose bool) error {
	return MigrateWithMigrationsTable(pgURL, migrationsPath, "", l, verbose)
}

// MigrateWithMigrationsTable executes all migrations we have, using the
// specified migrations table.
func MigrateWithMigrationsTable(pgURL, migrationsPath, migrationsTable string, l logger.Logger, verbose bool) error {
	l.Infof("running db migrations from %q", migrationsPath)
	purl, err := addMigrationsTable(pgURL, migrationsTable)
	if err != nil {
		return errors.Wrap(err, "parse PG URL")
	}
	m, err := migrate.New(addScheme(migrationsPath), purl)
	if err != nil {
		return errors.Wrap(err, "init migrator")
	}
	m.Log = migrationLog{Logger: l, verbose: verbose}
	version, dirty, err := m.Version()
	if err != nil && err != migrate.ErrNilVersion {
		return errors.Wrap(err, "init migrator - error getting migration version")
	}

	if dirty {
		// force to prior version to reattempt migration
		err := m.Force(int(version) - 1)
		if err != nil {
			return errors.Wrap(err, "force to working schema version")
		}
		l.Infof("Forced to previous version: %v to reattempt migration", int(version)-1)
	} else {
		l.Infof("Current schema version: %v", version)
	}

	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "migration attempt failed")
	}

	l.Infof("Completed db migrations")

	// The first error is trying to Close() the source. For our file source,
	// that's always nil
	_, err = m.Close()
	return errors.Wrap(err, "close migrations connection")
}

// DestructiveMigrateForTests will:
// * Drop the database to give you a clean slate
// * Run all your migrations
// * Forcibly run all the migrations again to verify that they are idempotent.
//
// Obviously you don't want this for production, but you should use it instead
// of the plain Migrate function in your tests if you can.
func DestructiveMigrateForTests(pgURL, migrationsPath string, l logger.Logger, verbose bool) error {
	l.Infof("running db migrations in destructo test mode for DB %q from %q", pgURL, migrationsPath)
	purl, err := addMigrationsTable(pgURL, "")

	m, err := migrate.New(addScheme(migrationsPath), purl)
	if err != nil {
		return errors.Wrapf(err, "migrator setup failed for %q", pgURL)
	}
	m.Log = migrationLog{Logger: l, verbose: verbose}
	if err := m.Drop(); err != nil {
		return errors.Wrapf(err, "drop database failed for %q", pgURL)
	}
	if err := m.Drop(); err != nil {
		return errors.Wrapf(err, "drop database failed for %q", pgURL)
	}
	if err := m.Up(); err != nil {
		return errors.Wrapf(err, "failed applying migrations to clean database %q", pgURL)
	}
	if err := m.Force(-1); err != nil {
		return errors.Wrapf(err, "failed to force migration state to version 0 for db %q", pgURL)
	}
	if err := m.Up(); err != nil {
		return errors.Wrapf(err, "failed to re-apply migration to migrated database %q - missing IF (NOT) EXISTS?", pgURL)
	}

	return nil
}

func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
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
