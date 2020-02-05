package migration

import (
	"context"
	"database/sql"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/lib/pq"
	"github.com/pkg/errors"
)

func createRole(ctx context.Context, db *sql.DB, role *v2Role) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return errors.Wrap(err, "begin create role tx")
	}

	row := tx.QueryRowContext(ctx, `INSERT INTO iam_roles (id, name, type, actions) VALUES ($1, $2, $3, $4)
		RETURNING db_id`,
		role.ID, role.Name, role.Type.String(), pq.Array(role.Actions))
	var dbID string
	if err := row.Scan(&dbID); err != nil {
		return errors.Wrap(err, "insert role")
	}

	_, err = tx.ExecContext(ctx,
		`INSERT INTO iam_role_projects (role_id, project_id)
		SELECT $1, project_db_id(p) FROM unnest($2::TEXT[]) as p`,
		dbID, pq.Array(role.Projects))
	if err != nil {
		return errors.Wrap(err, "insert role projects")
	}

	err = tx.Commit()
	if err != nil {
		return storage_errors.NewTxCommitError(err)
	}

	return nil
}

func createV2Policy(ctx context.Context, db *sql.DB, pol *v2Policy) (*v2Policy, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, errors.Wrap(err, "createV2Policy")
	}

	projects := pol.Projects
	if projects == nil {
		projects = []string{}
	}

	if err := insertCompletePolicy(ctx, pol, projects, tx); err != nil {
		return nil, errors.Wrap(err, "createV2Policy")
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewTxCommitError(err)
	}

	// Currently, we don't change anything from what is passed in.
	return pol, nil
}

func insertCompletePolicy(ctx context.Context, pol *v2Policy, projects []string, q *sql.Tx) error {
	if err := insertPolicyWithQuerier(ctx, pol, q); err != nil {
		return err
	}

	if err := associatePolicyWithProjects(ctx, pol.ID, projects, q); err != nil {
		return err
	}

	if err := insertPolicyStatementsWithQuerier(ctx, pol.ID, pol.Statements, q); err != nil {
		return err
	}
	return nil
}

func insertPolicyWithQuerier(ctx context.Context, inputPol *v2Policy, q *sql.Tx) error {
	_, err := q.ExecContext(ctx,
		`SELECT insert_iam_policy($1, $2, $3);`,
		inputPol.ID, inputPol.Name, inputPol.Type.String(),
	)
	if err != nil {
		return errors.Wrap(err, "insertPolicyWithQuerier")
	}

	err = replacePolicyMembersWithQuerier(ctx, inputPol.ID, inputPol.Members, q)
	return errors.Wrap(err, "replace policy members")
}

func replacePolicyMembersWithQuerier(ctx context.Context, policyID string, members []v2Member,
	q *sql.Tx) error {
	// Cascading drop any existing members.
	_, err := q.ExecContext(ctx,
		`DELETE FROM iam_policy_members WHERE policy_id=policy_db_id($1);`, policyID)
	if err != nil {
		return errors.Wrap(err, "replacePolicyMembersWithQuerier")
	}

	// Insert new members.
	for _, member := range members {
		err = insertOrReusePolicyMemberWithQuerier(ctx, policyID, member, q)
		if err != nil {
			return errors.Wrap(err, "replacePolicyMembersWithQuerier")
		}
	}
	return nil
}

func insertOrReusePolicyMemberWithQuerier(ctx context.Context, policyID string, member v2Member,
	q *sql.Tx) error {
	// First, we insert the member but on conflict do nothing. Then, we insert the member
	// into the policy. This is safe to do non-transactionally right now, since we don't support
	// updating either iam_members id or name columns which is the entire table. Also, we are currently
	// not deleting any of the rows, but reusing them per name string.

	_, err := q.ExecContext(ctx,
		"INSERT INTO iam_members (name) VALUES ($1) ON CONFLICT DO NOTHING",
		member.Name)
	if err != nil {
		return errors.Wrapf(err, "failed to upsert member %s", member.Name)
	}

	// For now, let's just ignore conflicts if someone is trying to add a user that is already a member.
	_, err = q.ExecContext(ctx,
		`INSERT INTO iam_policy_members (policy_id, member_id)
			VALUES (policy_db_id($1), member_db_id($2)) ON CONFLICT DO NOTHING`, policyID, member.Name)
	return errors.Wrapf(err, "failed to upsert member link: member=%s, policy_id=%s", member.Name, policyID)
}

func associatePolicyWithProjects(ctx context.Context,
	policyID string, inProjects []string,
	q *sql.Tx) error {

	// TODO this might be simplified as we modify how projects are assigned
	// Drop any existing associations.
	_, err := q.ExecContext(ctx,
		"DELETE FROM iam_policy_projects WHERE policy_id=policy_db_id($1)", policyID)
	if err != nil {
		return err
	}
	for _, project := range inProjects {
		_, err := q.ExecContext(ctx,
			`INSERT INTO iam_policy_projects (policy_id, project_id) VALUES (policy_db_id($1), project_db_id($2))`,
			&policyID, &project)
		if err != nil {
			return errors.Wrap(err, "associatePolicyWithProjects")
		}
	}

	return nil
}

func insertPolicyStatementsWithQuerier(ctx context.Context,
	policyID string, inputStatements []v2Statement,
	q *sql.Tx) error {
	for _, s := range inputStatements {
		_, err := q.ExecContext(ctx,
			`SELECT insert_iam_statement_into_policy($1, $2, $3, $4, $5, $6);`,
			policyID, s.Effect.String(), pq.Array(s.Actions),
			pq.Array(s.Resources), s.Role, pq.Array(s.Projects),
		)
		if err != nil {
			return errors.Wrap(err, "insertPolicyStatementsWithQuerier")
		}
	}

	return nil
}

func addPolicyMembers(ctx context.Context, db *sql.DB, id string, members []v2Member) ([]v2Member, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, errors.Wrap(err, "addPolicyMembers")
	}

	// Project filtering handled in here. We'll return a 404 right away if we can't find
	// the policy via ID as filtered by projects.
	_, err = queryPolicy(ctx, id, tx, false)
	if err != nil {
		return nil, errors.Wrap(err, "addPolicyMembers")
	}

	for _, member := range members {
		err := insertOrReusePolicyMemberWithQuerier(ctx, id, member, tx)
		if err != nil {
			return nil, errors.Wrap(err, "addPolicyMembers")
		}
	}

	members, err = getPolicyMembersWithQuerier(ctx, id, tx)
	if err != nil {
		return nil, errors.Wrap(err, "addPolicyMembers")
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewTxCommitError(err)
	}
	return members, nil
}

func getPolicyMembersWithQuerier(ctx context.Context, id string, q *sql.Tx) ([]v2Member, error) {
	rows, err := q.QueryContext(ctx,
		`SELECT m.name FROM iam_policy_members AS pm
			JOIN iam_members AS m ON pm.member_id=m.db_id
			WHERE pm.policy_id=policy_db_id($1) ORDER BY m.name ASC`, id)

	if err != nil {
		return nil, err
	}

	defer func() {
		rows.Close()
	}()

	members := []v2Member{}
	for rows.Next() {
		var member v2Member
		if err := rows.Scan(&member.Name); err != nil {
			return nil, errors.Wrap(err, "getPolicyMembersWithQuerier")
		}
		members = append(members, member)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return members, nil
}

func queryPolicy(ctx context.Context, id string, q *sql.Tx, selectForUpdate bool) (*v2Policy, error) {
	var pol v2Policy
	query := "SELECT query_policy($1, $2)"
	if selectForUpdate {
		query = "SELECT query_policy($1, $2) FOR UPDATE"
	}
	if err := q.QueryRowContext(ctx, query, id, pq.Array([]string{})).
		Scan(&pol); err != nil {
		return nil, err
	}

	return &pol, nil
}

func (p *pg) insertOrReusePolicyMemberWithQuerier(ctx context.Context, policyID string, member v2Member,
	q *sql.Tx) error {
	// First, we insert the member but on conflict do nothing. Then, we insert the member
	// into the policy. This is safe to do non-transactionally right now, since we don't support
	// updating either iam_members id or name columns which is the entire table. Also, we are currently
	// not deleting any of the rows, but reusing them per name string.

	_, err := q.ExecContext(ctx,
		"INSERT INTO iam_members (name) VALUES ($1) ON CONFLICT DO NOTHING",
		member.Name)
	if err != nil {
		return errors.Wrapf(err, "failed to upsert member %s", member.Name)
	}

	// For now, let's just ignore conflicts if someone is trying to add a user that is already a member.
	_, err = q.ExecContext(ctx,
		`INSERT INTO iam_policy_members (policy_id, member_id)
			VALUES (policy_db_id($1), member_db_id($2)) ON CONFLICT DO NOTHING`, policyID, member.Name)
	return errors.Wrapf(err, "failed to upsert member link: member=%s, policy_id=%s", member.Name, policyID)
}

func recordMigrationStatus(ctx context.Context, ms string, db *sql.DB) error {
	_, err := db.ExecContext(ctx, `UPDATE migration_status SET state=$1`, ms)
	return err
}
