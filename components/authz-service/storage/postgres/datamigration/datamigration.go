package datamigration

import (
	"database/sql"
	"net/url"

	"github.com/golang-migrate/migrate"
	"github.com/golang-migrate/migrate/database/postgres" // make driver available
	_ "github.com/golang-migrate/migrate/source/file"     // make source available
	"github.com/pkg/errors"

	"github.com/chef/automate/lib/logger"
)

// NOTE (TC): datamigration is the last set of IAM force-upgrade data related
// transformations. PLEASE DO NOT MODIFY THESE MIGRATIONS.
//
// Config holds the information needed to connect to the database (PGURL), to
// find the migration SQL files (Path), and log debug messages (Logger).
type Config struct {
	PGURL  *url.URL
	Path   string
	Logger logger.Logger
}

type migrationLog struct {
	logger.Logger
}

func (migrationLog) Verbose() bool {
	return false
}

// Migrate executes all migrations we have
func (c *Config) Migrate() error {
	m, err := c.open()
	if err != nil {
		return err
	}

	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "execute v2 data migrations")
	}

	// The first error is trying to Close() the source. For our file source,
	// that's always nil
	_, err = m.Close() // nolint: gas
	return errors.Wrap(err, "close v2 data migrations connection")
}

// Reset will drop the current state of the migration database.
// We don't need to reverse migrate since on reset we are just
// dropping the whole database.
func (c *Config) Reset() error {
	m, err := c.open()
	if err != nil {
		return err
	}

	err = m.Down()
	if err == migrate.ErrNoChange {
		err = nil
	}
	if err != nil {
		return err
	}

	// The first error is trying to Close() the source. For our file source,
	// that's always nil
	_, err = m.Close() // nolint: gas
	return errors.Wrap(err, "close v2 data migrations connection")
}

func (c *Config) open() (*migrate.Migrate, error) {
	db, err := sql.Open("postgres", c.PGURL.String())
	if err != nil {
		return nil, errors.Wrap(err, "init v2 sql client")
	}
	driver, err := postgres.WithInstance(db, &postgres.Config{
		MigrationsTable: "data_migrations",
	})
	if err != nil {
		return nil, errors.Wrap(err, "init v2 data migration driver")
	}
	m, err := migrate.NewWithDatabaseInstance(
		addScheme(c.Path),
		"postgres", driver)
	if err != nil {
		return nil, errors.Wrap(err, "init v2 data migration")
	}

	m.Log = migrationLog{c.Logger} // nolint: govet
	return m, nil
}

func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
}
