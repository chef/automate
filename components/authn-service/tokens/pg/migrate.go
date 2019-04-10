package pg

import (
	"database/sql"
	"fmt"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/authn-service/constants"
)

// migrate tries to execute all the migrations we know of
func migrate(c *sql.DB) error {
	// Acquire advisory lock
	_, err := c.Exec("SELECT pg_advisory_lock($1);", constants.AdvisoryLockID)
	if err != nil {
		return errors.Wrapf(err, "acquiring advisory lock %d", constants.AdvisoryLockID)
	}

	_, err = c.Exec(`
  CREATE TABLE IF NOT EXISTS migrations (
    num INTEGER NOT NULL,
    descr TEXT,
    at TIMESTAMPTZ NOT NULL
  );
`)
	if err != nil {
		return errors.Wrap(err, "creating migration table")
	}

	i := 0
	done := false
	var tx *sql.Tx
	for {

		tx, err = c.Begin()
		if err != nil {
			break
		}

		// Within a transaction, perform a single migration.
		var num sql.NullInt64
		n := 0

		if err = tx.QueryRow(`SELECT MAX(num) FROM migrations;`).Scan(&num); err != nil {
			err = errors.Wrap(err, "select max migration")
			break
		}
		if num.Valid { // if not, n will still be 0
			n = int(num.Int64)
		}

		if n >= len(migrations) {
			done = true
		} else {
			migrationNum := n + 1
			m := migrations[n]
			if _, err = tx.Exec(m.statement); err != nil {
				err = errors.Wrapf(err, "migration '%s' (%d) failed", m.descr, migrationNum)
				break
			}

			q := `INSERT INTO migrations (num, descr, at) VALUES ($1, $2, now());`
			if _, err = tx.Exec(q, migrationNum, m.descr); err != nil {
				err = errors.Wrap(err, "update migration table")
				break
			}
		}
		// done with one migration, commit transaction
		if err = tx.Commit(); err != nil {
			break
		}

		if done {
			break
		}
		i++ // rinse and repeat for next migration
	}

	if err != nil {
		// This adapter will go away soon. No need to fix this.
		tx.Rollback() // nolint
	}

	// Release advisory lock
	_, releaseErr := c.Exec("SELECT pg_advisory_unlock($1);", constants.AdvisoryLockID)
	if releaseErr != nil {
		releaseErr = errors.Wrapf(releaseErr, "releasing advisory lock %d", constants.AdvisoryLockID)
		// Want to report back on both the migration error and also the advisory release error, if there are both.
		if err != nil {
			return errors.New(err.Error() + releaseErr.Error())
		}
		return releaseErr
	}

	return err
}

type migration struct {
	descr     string
	statement string
}

var migrations = []migration{
	{
		descr: "create clients table",
		statement: `CREATE TABLE IF NOT EXISTS chef_authn_clients (
                  id TEXT NOT NULL PRIMARY KEY,
                  token TEXT NOT NULL,
                  created TIMESTAMPTZ,
                  updated TIMESTAMPTZ
                );`,
	},
	{
		descr:     "add 'active' column to clients table",
		statement: `ALTER TABLE chef_authn_clients ADD COLUMN active BOOLEAN NOT NULL DEFAULT TRUE;`,
	},
	{
		descr:     "change id to type UUID",
		statement: `ALTER TABLE chef_authn_clients ALTER COLUMN id TYPE uuid USING id::uuid;`,
	},
	{
		descr:     "add 'description' column to clients table",
		statement: `ALTER TABLE chef_authn_clients ADD COLUMN description TEXT NOT NULL CHECK (description <> '');`,
	},
	{
		descr:     "rename clients table tokens table",
		statement: `ALTER TABLE chef_authn_clients RENAME TO chef_authn_tokens;`,
	},
	{
		descr:     "rename 'token' column of tokens table to 'value'",
		statement: `ALTER TABLE chef_authn_tokens RENAME COLUMN token TO value;`,
	},
	{
		descr:     "change id to type text",
		statement: `ALTER TABLE chef_authn_tokens ALTER COLUMN id TYPE TEXT USING id::TEXT;`,
	},
	// This migration will add '{"default"}' project in everywhere, as old tokens need
	// to start with a project.
	{
		descr: "add column project_ids",
		statement: fmt.Sprintf(
			`ALTER TABLE chef_authn_tokens ADD COLUMN project_ids TEXT[] NOT NULL DEFAULT '{%s}' CHECK (project_ids <> '{}');`,
			"default"),
	},
	// This migration will remove adding '{"default"}' in, as we don't assume you want to add to the
	// default project. If someone tries to add a token without a project, they should fail based
	// on the above constraint. We have validation in the domain to prevent that from ever happening though.
	{
		descr:     "remove default project_ids",
		statement: `ALTER TABLE chef_authn_tokens ALTER COLUMN project_ids DROP DEFAULT;`,
	},
	// tokens no longer have to have projects
	{
		descr:     "drop constraint",
		statement: `ALTER TABLE chef_authn_tokens DROP CONSTRAINT IF EXISTS chef_authn_tokens_project_ids_check;`,
	},
	// we default to [] instead of null
	{
		descr:     "add default empty array for project_ids",
		statement: `ALTER TABLE chef_authn_tokens ALTER COLUMN project_ids SET DEFAULT '{}';`,
	},
}
