package migrator

import (
	"net/url"
	"os"

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

const filePath = "/hab/.skip_migration"

// Migrate executes all migrations we have
// Addind the filePath check in the below code to avoid the execution for the Migation code
// on the other than First Frontend node.
// Migration code will execute on the First node (Bootstrap_automate node)
// On the remaining node migration code execution is not required
func Migrate(pgURL, migrationsPath string, l logger.Logger, verbose bool) error {
	_, err := os.Stat(filePath)
	if err == nil {
		l.Infof("File '%s' exists\n", filePath)
		return nil
	} else if os.IsNotExist(err) {
		l.Infof("File '%s' does not exist\n", filePath)
		return MigrateWithMigrationsTable(pgURL, migrationsPath, "", l, verbose)
	} else {
		l.Infof("Error checking file: %v\n", err)
		return MigrateWithMigrationsTable(pgURL, migrationsPath, "", l, verbose)
	}
	//l.Infof("Ideally this should not be printed")
	//return nil
}

// MigrateWithMigrationsTable executes all migrations we have, using the
// specified migrations table.
func MigrateWithMigrationsTable(pgURL, migrationsPath, migrationsTable string, l logger.Logger, verbose bool) error {
	l.Infof("running db migrations from %q", migrationsPath)
	m, err := newMigratorBackend(pgURL, migrationsPath, l, verbose)
	if err != nil {
		return err
	}

	version, dirty, err := m.Version()
	if err != nil && err != migrate.ErrNilVersion {
		return errors.Wrap(err, "init migrator - error getting migration version")
	}

	// Do not take action based on the return value of m.Version(); Version() is
	// not protected by the exclusive lock, if it reports dirty, it could be
	// another front-end is running a migration now.
	l.Infof("Current schema version: %v dirty: %t", version, dirty)

	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "migration attempt failed")
	}
	if err != nil && err == migrate.ErrLocked {
		return errors.Wrap(err, "database locked: peer attempting migration?")
	}
	if _, ok := err.(migrate.ErrDirty); ok {
		l.WithError(err).Error("Migration attempt failed due to previous migration error leaving database in a dirty state")
		// this is vulnerable to a race condition if two or more front-ends reach
		// this branch of the code at once, they might both run all of the steps,
		// but they had to have gotten a dirty schema state while no process had
		// the lock, so this will only happen following some other error.
		version, _, err := m.Version()
		if err != nil {
			return errors.Wrap(err, "init migrator - error getting migration version")
		}
		l.Infof("Attempting to set recorded schema level to %d and rerun subsequent migration(s)", version)
		err = m.Force(int(version) - 1)
		if err != nil {
			return errors.Wrap(err, "force to previous schema version")
		}
		err = m.Up()
		if err != nil {
			return errors.Wrap(err, "re-migration attempt after force schema version failed")
		}
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
	l.Infof("running db migrations in destructive test mode for DB %q from %q", pgURL, migrationsPath)
	m, err := newMigratorBackend(pgURL, migrationsPath, l, verbose)
	if err != nil {
		return err
	}
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

func Drop(pgURL, migrationsPath string, l logger.Logger, verbose bool) error {
	l.Infof("dropping all tables in DB %q", pgURL)
	m, err := newMigratorBackend(pgURL, migrationsPath, l, verbose)
	if err != nil {
		return err
	}
	if err := m.Drop(); err != nil {
		return errors.Wrapf(err, "drop database failed for %q", pgURL)
	}
	return nil
}

func newMigratorBackend(pgURL, migrationsPath string, l logger.Logger, verbose bool) (*migrate.Migrate, error) {
	purl, err := addMigrationsTable(pgURL, "")
	if err != nil {
		return nil, errors.Wrapf(err, "migrator setup failed for %q", pgURL)
	}

	m, err := migrate.New(addScheme(migrationsPath), purl)
	if err != nil {
		return nil, errors.Wrapf(err, "migrator setup failed for %q", pgURL)
	}
	m.Log = migrationLog{Logger: l, verbose: verbose}
	return m, nil
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
