package migration

import (
	"net/url"

	"github.com/golang-migrate/migrate"
	_ "github.com/golang-migrate/migrate/database/postgres"
	_ "github.com/golang-migrate/migrate/source/file"
	"github.com/pkg/errors"

	"github.com/chef/automate/lib/logger"
)

type Config struct {
	PG     *url.URL
	Path   string
	Logger logger.Logger
}

type migrationLog struct {
	logger.Logger
}

func (migrationLog) Verbose() bool {
	return false
}

// Go executes all migrations we have
func (c *Config) Go() error {
	m, err := migrate.New(addScheme(c.Path), c.PG.String())
	if err != nil {
		return errors.Wrap(err, "init migrate")
	}

	m.Log = migrationLog{c.Logger} // nolint: govet

	err = m.Up()
	if err != nil && err != migrate.ErrNoChange {
		return errors.Wrap(err, "execute migrations")
	}

	// first error is always nil for the file source driver
	_, err = m.Close() // nolint: gas
	return errors.Wrap(err, "close migrations connection")
}

func addScheme(p string) string {
	u := url.URL{}
	u.Scheme = "file"
	u.Path = p
	return u.String()
}
