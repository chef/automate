package migration

import (
	"context"
	"database/sql"
	"net/url"

	"github.com/chef/automate/lib/logger"
	"github.com/golang-migrate/migrate"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy"
	constants_v2 "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v2"
)

// Config holds the information needed to connect to the database (PGURL), to
// find the migration SQL files (Path), and log debug messages (Logger).
type Config struct {
	PGURL  *url.URL
	Path   string
	Logger logger.Logger
}

const (
	// PreForceUpgradeMigration is last schema migration before force-upgrade.
	// All of our golang force-upgrade code assumes we are on this schema version.
	PreForceUpgradeMigration = 74
)

// Migrate executes all migrations we have
func (c *Config) Migrate(dataMigConf datamigration.Config) error {
	c.Logger.Warn("DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG: INIT")

	pgURL := c.PGURL.String()
	migrationsPath := c.Path
	l := c.Logger
	migrationsTable := ""
	ctx := context.Background()

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

	if version < PreForceUpgradeMigration {
		err = m.Migrate(PreForceUpgradeMigration)
		if err != nil && err != migrate.ErrNoChange {
			return errors.Wrap(err, "migration up to IAM V2 force upgrade failed")
		}
	}

	// run IAM V2 force upgrade if necessary
	db, err := sql.Open("postgres", pgURL)
	if err != nil {
		return err
	}

	if err := db.Ping(); err != nil {
		return errors.Wrap(err, "opening database connection")
	}

	notOnV2, isDirty, err := legacy.MigrateFromScratch(ctx, db)
	if err != nil {
		return errors.Wrap(err, "failed to retrieve migration_status")
	}
	c.Logger.Warnf("DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG: ABOUT TO MIGRATE %t", notOnV2)

	if notOnV2 {
		if isDirty {
			// get IAM db in original, clean state to avoid conflicts
			err = legacy.ResetIAMDb(ctx, db)
			if err != nil {
				return errors.Wrap(err, "reset IAM V2 database")
			}
			if err := dataMigConf.Reset(); err != nil {
				return errors.Wrap(err, "reset v2 data migrations")
			}
		}

		c.Logger.Warn("DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG: IN PROGRESS")
		err = legacy.RecordMigrationStatus(ctx, constants_v2.EnumInProgress, db)
		if err != nil {
			return errors.Wrapf(err, "failed to set IAM v2 migration_status to %s", constants_v2.EnumInProgress)
		}
		err = legacy.MigrateToV2(ctx, db)
		if err != nil {
			c.Logger.Warn("DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG: FAILED")
			statusErr := legacy.RecordMigrationStatus(ctx, constants_v2.EnumFailed, db)
			if statusErr != nil {
				return errors.Wrapf(statusErr, "failed to set IAM v2 migration_status to %s:%s", constants_v2.EnumFailed, err.Error())
			}
			return errors.Wrap(err, "IAM v2 force-upgrade failed")
		}
		c.Logger.Warn("DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG: SUCCESS")
		err = legacy.RecordMigrationStatus(ctx, constants_v2.EnumSuccessful, db)
		if err != nil {
			return errors.Wrapf(err, "failed to set IAM v2 migration_status to %s", constants_v2.EnumSuccessful)
		}
	}
	c.Logger.Warn("DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG: POST MIGRATION")

	// idempotent
	err = dataMigConf.Migrate()
	if err != nil {
		return errors.Wrap(err, "IAM data migrations failed")
	}

	// perform remaining migrations
	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "migrations failed")
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
