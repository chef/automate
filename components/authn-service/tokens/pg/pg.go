package pg

import (
	"database/sql"
	"net/url"

	"github.com/pkg/errors"
	"go.uber.org/zap"

	// adapter for database/sql
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/db"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"

	// TODO do we need this?
	_ "github.com/lib/pq"
)

// Config is the configuration this adapter takes
type Config struct {
	PGURL          string `json:"pg_url"`
	Database       string `json:"database"`
	MaxConnections int    `json:"max_connections"`
	MigrationsPath string `json:"migrations"`
}

// adapter carries the adapter's state
type adapter struct {
	db        *sql.DB
	logger    *zap.Logger
	validator tokens.ProjectValidator
}

// Open returns a storage adapter based on the config.
func (c *Config) Open(_ *certs.ServiceCerts, logger *zap.Logger,
	pv tokens.ProjectValidator) (tokens.Storage, error) {
	if c.PGURL == "" {
		if c.Database == "" {
			return nil, errors.New("either pg_url or database must be provided")
		}
		var err error

		c.PGURL, err = platform_config.PGURIFromEnvironment(c.Database)
		if err != nil {
			return nil, errors.Wrap(err, "could not get pg url")
		}
	}
	u, err := url.Parse(c.PGURL)
	if err != nil {
		return nil, errors.Wrap(err, "parse pg_url config")
	}

	db, err := c.initPostgresDB(u)
	if err != nil {
		return nil, errors.Wrap(err, "connect to database")
	}
	if err := runMigrations(db, u.String(), c.MigrationsPath); err != nil {
		return nil, errors.Wrap(err, "failed to setup database schema")
	}

	return &adapter{db: db, logger: logger, validator: pv}, nil
}

func (c *Config) initPostgresDB(pgURL *url.URL) (*sql.DB, error) {
	d, err := db.PGOpen(pgURL.String())
	if err != nil {
		return nil, err
	}
	if c.MaxConnections > 0 {
		d.SetMaxOpenConns(c.MaxConnections)
	}
	if err := d.Ping(); err != nil {
		return nil, errors.Wrap(err, "opening database connection")
	}

	return d, nil
}
