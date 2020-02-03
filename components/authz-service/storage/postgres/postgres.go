package postgres

import (
	"context"
	"database/sql"
	"strings"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
)

// New returns a new sq.DB connector with the automatic default migrations applied.
func New(ctx context.Context, migConf migration.Config, dataMigConf datamigration.Config) (*sql.DB, error) {
	pgURL := migConf.PGURL.String()

	if err := migConf.Migrate(dataMigConf); err != nil {
		return nil, errors.Wrapf(
			err,
			"apply database migrations from %s",
			migConf.Path,
		)
	}

	db, err := initPostgresDB(ctx, pgURL)
	if err != nil {
		return nil, errors.Wrapf(
			err,
			"connect to database at %s",
			pgURL,
		)
	}
	return db, nil
}

func initPostgresDB(ctx context.Context, pgURL string) (*sql.DB, error) {
	db, err := sql.Open("postgres", pgURL)
	if err != nil {
		return nil, err
	}

	if err := db.PingContext(ctx); err != nil {
		return nil, err
	}

	return db, nil
}

// ProcessError is used to translate DB-related errors into the error types
// defined for our storage implementations.
func ProcessError(err error) error {
	if err == sql.ErrNoRows {
		return storage.ErrNotFound
	}

	// The not found on json unmarshall case
	if strings.HasPrefix(err.Error(), "sql: Scan error on column index 0") &&
		strings.HasSuffix(err.Error(), "not found") {
		return storage.ErrNotFound
	}

	if err, ok := err.(*pq.Error); ok {
		return parsePQError(err)
	}

	return err
}

// parsePQError is able to parse pq specific errors into storage interface errors.
func parsePQError(e *pq.Error) error {
	switch e.Code {
	case "DRPPC": // Custom code: attempt to delete a non-deletable policy defined in migration 02
		return storage.ErrCannotDelete
	case "23505": // Unique violation
		return storage.ErrConflict
	case "23503": // Foreign key violation
		return &storage.ForeignKeyError{Msg: e.Message}
	case "P0002": // Not found in plpgsql ("no_data_found")
		return storage.ErrNotFound
	case "20000": // Not found
		return storage.ErrNotFound
	case "PRJTR": // Custom code: attempt to change a rule's projects that are immutable
		return storage.ErrChangeProjectForRule
	case "RDLTD": // Custom code: attempt to update a rule that is staged for deletion
		return storage.ErrMarkedForDeletion
	case "RLTYP": // Custom code: attempt to update a rule's type that is immutable
		return storage.ErrChangeTypeForRule
	}

	return storage.ErrDatabase
}
