package pg

import (
	"database/sql"
	"net/url"

	"github.com/pkg/errors"
	"go.uber.org/zap"

	// adapter for database/sql
	_ "github.com/lib/pq"

	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/platform"
	"github.com/chef/automate/lib/tls/certs"
)

// Config is the configuration this adapter takes
type Config struct {
	PGURL          string `json:"pg_url"`
	Database       string `json:"database"`
	MaxConnections int    `json:"max_connections"`
}

// adapter carries the adapter's state
type adapter struct {
	db     *sql.DB
	logger *zap.Logger
}

// Open returns a storage adapter based on the config.
func (c *Config) Open(_ *certs.ServiceCerts, logger *zap.Logger) (tokens.Storage, error) {
	if c.PGURL == "" {
		if c.Database == "" {
			return nil, errors.New("either pg_url or database must be provided")
		}
		var err error

		c.PGURL, err = platform.PGURIFromEnvironment(c.Database)
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
	if err := migrate(db); err != nil {
		return nil, errors.Wrap(err, "failed to setup database schema")
	}

	return &adapter{db: db, logger: logger}, nil
}

func (c *Config) initPostgresDB(pgURL *url.URL) (*sql.DB, error) {
	db, err := sql.Open("postgres", pgURL.String())
	if err != nil {
		return nil, err
	}
	if c.MaxConnections > 0 {
		db.SetMaxOpenConns(c.MaxConnections)
	}
	if err := db.Ping(); err != nil {
		return nil, errors.Wrap(err, "opening database connection")
	}

	return db, nil
}
