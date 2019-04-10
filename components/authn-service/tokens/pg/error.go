package pg

import (
	"database/sql"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	tokens "github.com/chef/automate/components/authn-service/tokens/types"
)

func processSQLError(err error, wrap string) error {
	if err == sql.ErrNoRows {
		return &tokens.NotFoundError{}
	}

	if err, ok := err.(*pq.Error); ok {
		return parsePQError(err)
	}

	// unknown other type of error
	return errors.Wrap(err, wrap)
}

// TODO (tc): Redundant with some of authz's sql / pg error handling.
// We should standardize our pq / sql error handling.
func parsePQError(err *pq.Error) error {
	switch err.Code {
	// Unique violation
	case "23505":
		return &tokens.ConflictError{}
	}
	return err
}
