package migration

import (
	"context"
	"database/sql"
	"net/url"

	"github.com/chef/automate/lib/logger"
	"github.com/golang-migrate/migrate"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
)

// Config holds the information needed to connect to the database (PGURL), to
// find the migration SQL files (Path), and log debug messages (Logger).
type Config struct {
	PGURL  *url.URL
	Path   string
	Logger logger.Logger
}

const (
	// PRE_FORCE_UPGRADE_MIGRATION is last schema migration before force-upgrade.
	// All of our golang force-upgrade code assumes we are on this schema version.
	PRE_FORCE_UPGRADE_MIGRATION = 74
)

// Migrate executes all migrations we have
func (c *Config) Migrate(dataMigConf datamigration.Config) error {
	pgURL := c.PGURL.String()
	migrationsPath := c.Path
	l := c.Logger
	migrationsTable := ""
	ctx := context.TODO()

	l.Infof("Running db migrations from %q", migrationsPath)
	purl, err := addMigrationsTable(pgURL, migrationsTable)
	if err != nil {
		return errors.Wrap(err, "parse PG URL")
	}
	m, err := migrate.New(addScheme(migrationsPath), purl)
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
		l.Infof("Forced to previous version: %v to reattempt migration", int(version)-1)
	} else {
		l.Infof("Current schema version: %v", version)
	}

	// TODO we want this to no run any migrations
	// if we are already past the last pre-force-upgrade migration
	// so confirm that behavior. otherwise need an if statement here.
	// WE WILL NEED IF STATEMENT HERE
	err = m.Migrate(PRE_FORCE_UPGRADE_MIGRATION)
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "migration up to IAM V2 force upgrade failed")
	}

	// TODO force upgrade
	db, err := sql.Open("postgres", pgURL)
	if err != nil {
		return err
	}

	if err := db.Ping(); err != nil {
		return errors.Wrap(err, "opening database connection")
	}

	// TODO check if we've already run force-upgrade, all the force-upgrade stuff
	// should be in an if block and should only happen if we haven't force-upgraded yet.

	// begin force-migration if

	// TODO set in-progress flag for new table to indicate force-upgrade (different from IAM version)?
	// Not sure exactly if / how we wanna handle this yet.

	err = migrateToV2(ctx, db)
	if err != nil {
		// TODO set force-upgrade to failed?
		return errors.Wrap(err, "IAM v2 force-upgrade failed")
	}
	// TODO set successful flag for force-upgrade

	// end force-migration if

	// this is idempontent and should be a no-op besides reading
	// the data_migrations table if we are post-force-upgrade
	err = dataMigConf.Migrate()
	if err != nil {
		return errors.Wrap(err, "IAM data migrations failed")
	}

	// perform remaining migrations
	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "migrations after IAM V2 force upgrade failed")
	}

	l.Infof("Completed db migrations")

	err = db.Close()
	if err != nil {
		return errors.Wrap(err, "close migration db connection")
	}

	// The first error is trying to Close() the source. For our file source,
	// that's always nil
	_, err = m.Close()
	return errors.Wrap(err, "close migrations connection")
}

// TODO these were stolen from migrate.go
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

func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
}
