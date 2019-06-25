package datamigration

import (
	"net/url"

	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

// NOTE (TC): These are IAM V2 specific data migrations. DO NOT APPLY ANY SCHEMA CHANGES HERE!
// Read more in v2_data_migrations.md.
//
// Config holds the information needed to connect to the database (PGURL), to
// find the migration SQL files (Path), and log debug messages (Logger).
type Config struct {
	PGURL  *url.URL
	Path   string
	Logger logger.Logger
}

// Migrate executes all migrations we have
func (c *Config) Migrate() error {
	return migrator.MigrateWithMigrationsTable(c.PGURL.String(), c.Path, "data_migrations", c.Logger, false)
}
