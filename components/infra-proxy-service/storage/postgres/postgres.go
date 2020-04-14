package postgres

import (
	"context"
	"database/sql"
	"regexp"

	"github.com/lib/pq" // adapter for database/sql
	"github.com/pkg/errors"

	authz "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres/migration"
	"github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/logger"
)

type postgres struct {
	db          *sql.DB
	logger      logger.Logger
	authzClient authz.AuthorizationClient
}

type querier interface {
	QueryRowContext(context.Context, string, ...interface{}) *sql.Row
}

// New instantiates and returns a postgres storage implementation
func New(logger logger.Logger, migrationConfig migration.Config, authzClient authz.AuthorizationClient) (storage.Storage, error) {

	db, err := initPostgresDB(migrationConfig.PGURL.String())
	if err != nil {
		return nil, errors.Wrap(err, "initialize database")
	}

	if err := migrationConfig.Migrate(); err != nil {
		return nil, errors.Wrap(err, "database migrations")
	}

	return &postgres{db, logger, authzClient}, nil
}

func initPostgresDB(pgURL string) (*sql.DB, error) {
	d, err := db.PGOpen(pgURL)
	if err != nil {
		return nil, err
	}

	if err := d.Ping(); err != nil {
		return nil, errors.Wrap(err, "opening database connection")
	}

	return d, nil
}

// translate lib/pq error into storage.Err*
func parsePQError(e *pq.Error) error {
	// defined in https://github.com/lib/pq/blob/88edab0803230a3898347e77b474f8c1820a1f20/error.go#L78
	switch e.Code.Name() {
	case "unique_violation":
		return storage.ErrConflict
	case "foreign_key_violation":
		// In this piece of code, a foreign key violation means the server the org
		// should be added to doesn't exist.
		// The only way to distinguish this violation by comparing the server_id key in Error detail.
		// DETAIL:  Key (server_id)=(2e831dec-f25c-4fde-aef6-8f68ea366251) is not present in table "servers".
		if regexp.MustCompile(`\(server_id\).*is not present.*table.*\"servers\"`).MatchString(e.Detail) {
			return storage.ErrForeignKeyViolation
		}
		// Marking other foreign key violations as a record can not be deleted.
		// DETAIL: Key (id)=(2e831dec-f25c-4fde-aef6-8f68ea36625f) is still referenced from table "orgs".
		return storage.ErrCannotDelete
	}

	return e
}

// processError is used to translate DB-related errors into the error types
// defined for our storage implementations
func (p *postgres) processError(err error) error {
	if err, ok := err.(*pq.Error); ok {
		return parsePQError(err)
	}
	if err == sql.ErrNoRows {
		return storage.ErrNotFound
	}
	p.logger.Debugf("unknown error type from database: %v", err)
	return err
}
