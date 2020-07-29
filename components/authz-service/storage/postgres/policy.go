package postgres

import (
	"context"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/projectassignment"
)

// CreatePolicy stores a new policy and its statements in postgres and returns the final policy.
func (p *pg) CreatePolicy(ctx context.Context, pol *storage.Policy, skipProjectsCheckOnV1PolicyMigration bool) (*storage.Policy, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Note(sr): we're using BeginTx with the context that'll be cancelled in a
	// `defer` when the function ends. This should rollback transactions that
	// haven't been committed -- what would happen when any of the following
	// `err != nil` cases return early.
	// However, I haven't played with this extensively, so there's a bit of a
	// chance that this understanding is just plain wrong.

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	projects := pol.Projects
	if projects == nil {
		projects = []string{}
	}

	// skip project permissions check on upgrade from v1 or for chef-managed policies
	if !skipProjectsCheckOnV1PolicyMigration && pol.Type == storage.Custom {
		err = p.ensureNoProjectsMissingWithQuerier(ctx, tx, projects)
		if err != nil {
			return nil, p.processError(err)
		}

		err = projectassignment.AuthorizeProjectAssignment(ctx,
			p.engine,
			auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
			[]string{},
			projects,
			false)
		if err != nil {
			return nil, p.processError(err)
		}
	}

	if err := p.insertCompletePolicy(ctx, pol, projects, tx); err != nil {
		return nil, p.processError(err)
	}

	if err := p.notifyPolicyChange(ctx, tx); err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	// Currently, we don't change anything from what is passed in.
	return pol, nil
}

func (p *pg) PurgeSubjectFromPolicies(ctx context.Context, sub string) ([]string, error) {
	var polIDs []string
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}
	// Note(sr) 2018-11-26: We're keeping the iam_members reference. Should we
	// remove it? "Just" removing the iam_members entry and relying to CASCADE to
	// remove the membership rows from iam_policy_members doesn't do the trick here
	// -- not if we care about the affected policy IDs. (We at the moment don't
	// prescribe this, but it feels like the better choice.)

	row := tx.QueryRowContext(ctx, `
		WITH pol_db_ids AS (
			DELETE FROM iam_policy_members
			WHERE member_id=(SELECT db_id FROM iam_members WHERE name=$1)
			RETURNING policy_id
		)
		SELECT array_agg(id)
		FROM iam_policies
		WHERE db_id IN (SELECT * FROM pol_db_ids)`, sub)
	err = row.Scan(pq.Array(&polIDs))
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.notifyPolicyChange(ctx, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, p.processError(err)
	}

	return polIDs, nil
}

func (p *pg) ListPolicies(ctx context.Context) ([]*storage.Policy, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	var pols []*storage.Policy
	rows, err := p.db.QueryContext(ctx,
		"SELECT query_policies from query_policies($1)", pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		var pol storage.Policy
		if err := rows.Scan(&pol); err != nil {
			return nil, p.processError(err)
		}
		pols = append(pols, &pol)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return pols, nil
}

func (p *pg) GetPolicy(ctx context.Context, id string) (*storage.Policy, error) {
	pol, err := p.queryPolicy(ctx, id, p.db, false)
	if err != nil {
		return nil, p.processError(err)
	}

	return pol, nil
}

func (p *pg) DeletePolicy(ctx context.Context, id string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return p.processError(err)
	}

	// Project filtering handled in here
	_, err = p.queryPolicy(ctx, id, tx, false)
	if err != nil {
		return p.processError(err)
	}

	_, err = tx.ExecContext(ctx,
		`DELETE FROM iam_policies WHERE id=$1;`,
		id,
	)
	if err != nil {
		return p.processError(err)
	}

	err = p.notifyPolicyChange(ctx, tx)
	if err != nil {
		return p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return storage.NewTxCommitError(err)
	}

	return nil
}

func (p *pg) UpdatePolicy(ctx context.Context, pol *storage.Policy) (*storage.Policy, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	// Project filtering handled in here. We'll return a 404 right away if we can't find
	// the policy via ID as filtered by projects. Also locks relevant rows so we can check
	// project assignment permissions without them being changed under us.
	oldPolicy, err := p.queryPolicy(ctx, pol.ID, tx, true)
	if err != nil {
		return nil, p.processError(err)
	}

	newProjects := pol.Projects
	err = p.ensureNoProjectsMissingWithQuerier(ctx, tx, newProjects)
	if err != nil {
		return nil, p.processError(err)
	}

	err = projectassignment.AuthorizeProjectAssignment(ctx,
		p.engine,
		auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
		oldPolicy.Projects,
		newProjects,
		true)
	if err != nil {
		return nil, p.processError(err)
	}

	// Since we are forcing users to update the entire policy, we should delete
	// all existing statements for simplicity for now. Let's not delete the actual
	// policy row to preserve that record / id.
	//
	// This will cascade delete all related statements.
	if _, err := tx.ExecContext(ctx,
		"DELETE FROM iam_statements WHERE policy_id=policy_db_id($1)",
		pol.ID,
	); err != nil {
		if err := p.processError(err); err != storage.ErrNotFound {
			return nil, err
		}
	}

	res, err := tx.ExecContext(ctx,
		"UPDATE iam_policies SET (name, type) = ($2, $3) WHERE id = $1 RETURNING id",
		pol.ID, pol.Name, pol.Type.String(),
	)
	if err != nil {
		return nil, p.processError(err)
	}

	affected, err := res.RowsAffected()
	if err != nil {
		return nil, p.processError(err)
	}
	if affected == 0 {
		return nil, storage.ErrNotFound
	}

	// Update policy's projects
	if err := p.associatePolicyWithProjects(ctx, pol.ID, newProjects, tx); err != nil {
		return nil, p.processError(err)
	}

	// Also replace any existing policy members and update with new members.
	if err := p.replacePolicyMembersWithQuerier(ctx, pol.ID, pol.Members, tx); err != nil {
		return nil, p.processError(err)
	}

	if err := p.insertPolicyStatementsWithQuerier(ctx, pol.ID, pol.Statements, tx); err != nil {
		return nil, p.processError(err)
	}

	if err := p.notifyPolicyChange(ctx, tx); err != nil {
		return nil, p.processError(err)
	}

	if err := tx.Commit(); err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	// Currently, we don't change anything from what is passed in.
	return pol, nil
}

func (p *pg) GetPolicyChangeID(ctx context.Context) (string, error) {
	var policyChangeID string

	row := p.db.QueryRowContext(ctx, "SELECT policy_change_id FROM policy_change_tracker LIMIT 1;")

	if err := row.Scan(&policyChangeID); err != nil {
		return "", p.processError(err)
	}

	return policyChangeID, nil
}

func (p *pg) GetPolicyChangeNotifier(ctx context.Context) (storage.PolicyChangeNotifier, error) {
	return newPolicyChangeNotifier(ctx, p.conninfo)
}

func (p *pg) insertCompletePolicy(ctx context.Context, pol *storage.Policy, projects []string, q Querier) error {
	if err := p.insertPolicyWithQuerier(ctx, pol, q); err != nil {
		return err
	}

	if err := p.associatePolicyWithProjects(ctx, pol.ID, projects, q); err != nil {
		return err
	}

	if err := p.insertPolicyStatementsWithQuerier(ctx, pol.ID, pol.Statements, q); err != nil {
		return err
	}
	return nil
}

// insertPolicyWithQuerier inserts a new custom policy. It does not return the
// new policy since there are no DEFAULTS in the iam_policy table.
func (p *pg) insertPolicyWithQuerier(ctx context.Context, inputPol *storage.Policy, q Querier) error {
	_, err := q.ExecContext(ctx,
		`SELECT insert_iam_policy($1, $2, $3);`,
		inputPol.ID, inputPol.Name, inputPol.Type.String(),
	)
	if err != nil {
		return err
	}

	err = p.replacePolicyMembersWithQuerier(ctx, inputPol.ID, inputPol.Members, q)
	return errors.Wrap(err, "replace policy members")
}

// insertPolicyStatements WithQuerier inserts a new statement and associates it with an existing policy.
// Does not return the statements since they will be exactly the same as passed in since
// statements have no defaults in the database.
func (p *pg) insertPolicyStatementsWithQuerier(ctx context.Context,
	policyID string, inputStatements []storage.Statement,
	q Querier) error {
	for _, s := range inputStatements {
		_, err := q.ExecContext(ctx,
			`SELECT insert_iam_statement_into_policy($1, $2, $3, $4, $5, $6);`,
			policyID, s.Effect.String(), pq.Array(s.Actions),
			pq.Array(s.Resources), s.Role, pq.Array(s.Projects),
		)
		if err != nil {
			return p.processError(err)
		}
	}

	return nil
}

// insertPolicyProjectsWithQuerier creates new associations between a policy and its projects.
func (p *pg) associatePolicyWithProjects(ctx context.Context,
	policyID string, inProjects []string,
	q Querier) error {

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
			return p.processError(err)
		}
	}

	return nil
}

// queryPolicy returns a policy based on id or an error. Can optionally lock for updates.
func (p *pg) queryPolicy(ctx context.Context, id string, q Querier, selectForUpdate bool) (*storage.Policy, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	var pol storage.Policy
	query := "SELECT query_policy($1, $2)"
	if selectForUpdate {
		query = "SELECT query_policy($1, $2) FOR UPDATE"
	}
	if err := q.QueryRowContext(ctx, query, id, pq.Array(projectsFilter)).
		Scan(&pol); err != nil {
		return nil, err
	}

	return &pol, nil
}
