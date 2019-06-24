package migrator

import (
	"net/url"

	"github.com/golang-migrate/migrate"
	_ "github.com/golang-migrate/migrate/database/postgres" // make driver available
	_ "github.com/golang-migrate/migrate/source/file"       // make source available
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

// Migrate executes all migrations we have
func Migrate(pgURL string, migrationsPath string) error {
	logrus.Infof("Running db migrations from %q", migrationsPath)
	m, err := migrate.New(addScheme(migrationsPath), pgURL)
	if err != nil {
		return errors.Wrap(err, "init migrator")
	}
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
		logrus.Infof("Forced to previous version: %v to reattempt migration", int(version)-1)
	} else {
		logrus.Infof("Current schema version: %v", version)
	}

	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "migration attempt failed")
	}

	logrus.Infof("Completed db migrations")

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
