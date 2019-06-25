package migration

import (
	"net/url"

	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type Config struct {
	PG     *url.URL
	Path   string
	Logger logger.Logger
}

// Go executes all migrations we have
func (c *Config) Go() error {
	return migrator.Migrate(c.PG.String(), c.Path, c.Logger, false)
}
