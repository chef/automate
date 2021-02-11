package postgres

import (
	"database/sql"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"

	"github.com/chef/automate/components/notifications-service2/pkg/config"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
)

type Postgres struct {
	DbMap        *gorp.DbMap
	dbConn       *sql.DB
	URI          string
	SchemaPath   string
	MaxIdleConns int
	MaxOpenConns int
}

func Start(c *config.Notifications) (*Postgres, error) {
	p := &Postgres{
		URI:          c.Postgres.URI,
		MaxIdleConns: c.Postgres.MaxIdleConns,
		MaxOpenConns: c.Postgres.MaxOpenConns,
		SchemaPath:   c.Postgres.SchemaPath,
	}
	if err := p.Connect(); err != nil {
		return nil, err
	}
	if err := p.Migrate(); err != nil {
		return nil, err
	}
	return p, nil
}

func (db *Postgres) Connect() error {
	dbConn, err := libdb.PGOpen(db.URI)
	if err != nil {
		return errors.Wrapf(err, "Failed to open database with uri: %s", db.URI)
	}
	db.dbConn = dbConn

	if db.MaxIdleConns > 0 {
		dbConn.SetMaxIdleConns(db.MaxIdleConns)
	}
	if db.MaxOpenConns > 0 {
		dbConn.SetMaxOpenConns(db.MaxOpenConns)
	}
	// Configure the database mapping object
	db.DbMap = &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}}

	// Verify database
	err = db.Ping()
	if err != nil {
		return errors.Wrapf(err, "Failed to ping database with uri: %s", db.URI)
	}
	return nil
}

// ping will verify if the database mapped with gorp is available
func (db *Postgres) Ping() error {
	return db.dbConn.Ping()
}

func (db *Postgres) Migrate() error {
	if err := migrator.Migrate(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}
	return nil
}

// DestructiveMigrateForTests clears the database then forcibly runs the
// migrations twice. This is important to test because our db tooling will
// retry schema migrations in face of connection errors, etc.; the retry can
// only succeed if the schem migrations are written in idempotent style.
func (db *Postgres) DestructiveMigrateForTests() error {
	if err := migrator.DestructiveMigrateForTests(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return errors.Wrapf(err, "Unable to re-run migrations on existing db. [path:%s]", db.SchemaPath)
	}
	return nil
}

// Clear deletes everything in the database. It's here to support integration
// testing of migrations. Using it outside of testing will probably cause data
// loss.
func (db *Postgres) Clear() error {
	if err := migrator.Drop(db.URI, db.SchemaPath, logger.NewLogrusStandardLogger(), true); err != nil {
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", db.SchemaPath)
	}

	// The implementation of `Drop()` in the migrator only deletes tables and it
	// also ignores the schema_migrations table. We do additional cleanup
	// manually here.
	if _, err := db.Exec("DROP TYPE IF EXISTS rule_event CASCADE;"); err != nil {
		return errors.Wrapf(err, "enum cleanup failed on %q", db.URI)
	}

	if _, err := db.Exec("DROP TYPE IF EXISTS rule_action CASCADE;"); err != nil {
		return errors.Wrapf(err, "enum cleanup failed on %q", db.URI)
	}

	if _, err := db.Exec("DROP TABLE IF EXISTS schema_migrations CASCADE;"); err != nil {
		return errors.Wrapf(err, "schema_migrations cleanup failed on %q", db.URI)
	}

	return nil
}

func (db *Postgres) Exec(query string, args ...interface{}) (sql.Result, error) {
	return db.dbConn.Exec(query, args...)
}
