package migration

import (
	"net/url"

	"github.com/golang-migrate/migrate"
	_ "github.com/golang-migrate/migrate/database/postgres" // make driver available
	_ "github.com/golang-migrate/migrate/source/file"       // make source available
	"github.com/pkg/errors"

	"github.com/chef/automate/lib/logger"
)

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
	m, err := migrate.New(addScheme(c.Path), c.PGURL.String())
	if err != nil {
		return errors.Wrap(err, "init migrate")
	}

	m.Log = migrationLog{c.Logger} // nolint: govet

	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "execute migrations")
	}

	// The first error is trying to Close() the source. For our file source,
	// that's always nil
	_, err = m.Close() // nolint: gas
	return errors.Wrap(err, "close migrations connection")
}

func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
}
