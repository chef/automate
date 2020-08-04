package migrator

import (
	"crypto/sha256"
	"fmt"
	"net/url"
	"strconv"

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
	l.Infof("Running db migrations from %q", migrationsPath)
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

func lockIdForURL(pgURL string) (int64, error) {
	u, err := url.Parse(pgURL)
	if err != nil {
		return 0, err
	}
	hex := fmt.Sprintf("%x", sha256.Sum256([]byte(u.Path)))
	dec, err := strconv.ParseInt(hex, 16, 64)
	if err != nil {
		return 0, err
	}
	return dec, nil
}
