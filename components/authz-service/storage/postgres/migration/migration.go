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
	constants "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v2"
)

// Config holds the information needed to connect to the database (PGURL), to
// find the migration SQL files (Path), and log debug messages (Logger).
type Config struct {
	PGURL              *url.URL
	Path               string
	Logger             logger.Logger
	MaxConnections     int
	MaxIdleConnections int
}

const (
	// PreForceUpgradeMigration is last schema migration before force-upgrade.
	// All of our golang force-upgrade code assumes we are on this schema version.
	PreForceUpgradeMigration = 74
)

// Migrate executes all migrations we have
func (c *Config) Migrate(dataMigConf datamigration.Config) error {
	pgURL := c.PGURL.String()
	migrationsPath := c.Path
	l := c.Logger
	migrationsTable := ""
	ctx := context.Background()

	l.Info("Initializing DB migrations...")
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
		l.Infof("Forced to previous version %v to reattempt migration", int(version)-1)
	} else {
		l.Infof("Current schema version: %v", version)
	}

	if version < PreForceUpgradeMigration {
		l.Infof("Migrating schema version %v to %v...", version, PreForceUpgradeMigration)
		err = m.Migrate(PreForceUpgradeMigration)
		if err != nil && err != migrate.ErrNoChange {
			return errors.Wrapf(err, "migration up to version %v failed", PreForceUpgradeMigration)
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

	isOnV1, isDirty, err := legacy.MigrationStatus(ctx, db)
	if err != nil {
		return errors.Wrap(err, "failed to retrieve migration_status")
	}

	// do not migrate policies if we're on a fresh install
	migrateV1Policies := version != 0

	if isOnV1 {
		if isDirty { // we've attempted to migrate and not finished
			// get IAM db in original, clean state to avoid conflicts
			l.Infof("Detected unfinished migration; resetting IAM DB to clean state")
			err = legacy.ResetIAMDb(ctx, db)
			if err != nil {
				return errors.Wrap(err, "reset database")
			}
			if err := dataMigConf.Reset(); err != nil {
				return errors.Wrap(err, "reset data migrations")
			}
			if version != 0 {
				migrateV1Policies = true
			}
		}

		err = legacy.RecordMigrationStatus(ctx, constants.EnumInProgress, db)
		if err != nil {
			return errors.Wrapf(err, "failed to set IAM v2 migration_status to %s", constants.EnumInProgress)
		}
		l.Info("Setting up IAM data basics...")
		err = legacy.MigrateToV2(ctx, db, migrateV1Policies)
		if err != nil {
			statusErr := legacy.RecordMigrationStatus(ctx, constants.EnumFailed, db)
			if statusErr != nil {
				return errors.Wrapf(statusErr, "failed to set IAM migration_status to %s:%s", constants.EnumFailed, err.Error())
			}
			return errors.Wrap(err, "IAM data basics failed")
		}
		err = legacy.RecordMigrationStatus(ctx, constants.EnumSuccessful, db)
		if err != nil {
			return errors.Wrapf(err, "failed to set IAM migration_status to %s", constants.EnumSuccessful)
		}
	}

	// idempotent
	l.Info("Checking for data migrations...")
	err = dataMigConf.Migrate()
	if err != nil {
		return errors.Wrap(err, "IAM data migrations failed")
	}

	// perform remaining migrations
	l.Infof("Checking for remaining schema migrations...")
	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "migrations failed")
	}

	version, _, err = m.Version()
	if err != nil {
		return errors.Wrap(err, "failed to get version post-migration")
	}

	l.Infof("DB initialization complete at version %v", version)

	err = db.Close()
	if err != nil {
		return errors.Wrap(err, "close migration db connection")
	}

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
