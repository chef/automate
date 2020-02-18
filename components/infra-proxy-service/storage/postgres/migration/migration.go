package migration

import (
	"net/url"

	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

// Config holds the information needed to connect to the database (PGURL), to
// find the migration SQL files (Path), and log debug messages (Logger).
type Config struct {
	PGURL  *url.URL
	Path   string
	Logger logger.Logger
}

// Migrate executes all migrations we have
func (c *Config) Migrate() error {
	return migrator.Migrate(c.PGURL.String(), c.Path, c.Logger, false)
}
