package postgres

import (
	"context"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/projectassignment"
)

func (p *pg) CreateRole(ctx context.Context, role *storage.Role, skipProjectsCheckOnV1PolicyMigration bool) (*storage.Role, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	projects := role.Projects
	// ensure initial chef-managed policies/roles created
	if !skipProjectsCheckOnV1PolicyMigration && role.Type == storage.Custom {
		err = p.ensureNoProjectsMissingWithQuerier(ctx, tx, role.Projects)
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

	err = p.insertRoleWithQuerier(ctx, role, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.notifyPolicyChange(ctx, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	return role, nil
}

func (p *pg) ListRoles(ctx context.Context) ([]*storage.Role, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	var roles []*storage.Role
	rows, err := p.db.QueryContext(ctx, "SELECT query_roles($1)", pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		var role storage.Role
		if err := rows.Scan(&role); err != nil {
			return nil, p.processError(err)
		}
		roles = append(roles, &role)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return roles, nil
}

func (p *pg) GetRole(ctx context.Context, id string) (*storage.Role, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	doesIntersect, err := checkIfRoleIntersectsProjectsFilter(ctx, tx, id, projectsFilter)
	if err != nil {
		return nil, p.processError(err)
	}
	if !doesIntersect {
		return nil, storage.ErrNotFound
	}

	var role storage.Role
	row := tx.QueryRowContext(ctx, `SELECT query_role($1);`, id)
	err = row.Scan(&role)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	return &role, nil
}

func (p *pg) DeleteRole(ctx context.Context, id string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return err
	}

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return p.processError(err)
	}

	doesIntersect, err := checkIfRoleIntersectsProjectsFilter(ctx, tx, id, projectsFilter)
	if err != nil {
		return p.processError(err)
	}
	if !doesIntersect {
		return storage.ErrNotFound
	}

	res, err := tx.ExecContext(ctx, "DELETE FROM iam_roles WHERE id=$1", id)
	if err != nil {
		return p.processError(err)
	}

	err = p.singleRowResultOrNotFoundErr(res)
	if err != nil {
		return err
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

func (p *pg) UpdateRole(ctx context.Context, role *storage.Role) (*storage.Role, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	doesIntersect, err := checkIfRoleIntersectsProjectsFilter(ctx, tx, role.ID, projectsFilter)
	if err != nil {
		return nil, p.processError(err)
	}
	if !doesIntersect {
		return nil, storage.ErrNotFound
	}

	newProjects := role.Projects
	var oldRole storage.Role
	// get the old role and lock the role for updates (still readable)
	// until the update completes or the transaction fails so that
	// the project diff doesn't change under us while we perform permission checks.
	queryRoleRow := tx.QueryRowContext(ctx, `SELECT query_role($1) FOR UPDATE;`, role.ID)
	err = queryRoleRow.Scan(&oldRole)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.ensureNoProjectsMissingWithQuerier(ctx, tx, newProjects)
	if err != nil {
		return nil, p.processError(err)
	}

	err = projectassignment.AuthorizeProjectAssignment(ctx,
		p.engine,
		auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
		oldRole.Projects,
		newProjects,
		true)
	if err != nil {
		return nil, p.processError(err)
	}

	row := tx.QueryRowContext(ctx,
		`UPDATE iam_roles SET (name, actions) =
			($2, $3) WHERE id = $1 RETURNING db_id`,
		role.ID, role.Name, pq.Array(role.Actions),
	)
	// TODO: check not found case
	var dbID string
	if err := row.Scan(&dbID); err != nil {
		return nil, p.processError(err)
	}

	_, err = tx.ExecContext(ctx,
		"DELETE FROM iam_role_projects WHERE role_id=$1", dbID)
	if err != nil {
		return nil, p.processError(err)
	}

	_, err = tx.ExecContext(ctx,
		`INSERT INTO iam_role_projects (role_id, project_id)
		SELECT $1, db_id FROM iam_projects WHERE id=ANY($2)`,
		dbID, pq.Array(role.Projects))
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.notifyPolicyChange(ctx, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	return role, nil
}

func checkIfRoleIntersectsProjectsFilter(ctx context.Context, q Querier,
	id string, projectsFilter []string) (bool, error) {

	// If no filter was specified, do not filter.
	if len(projectsFilter) == 0 {
		return true, nil
	}

	// Return true or false if there is intersection between iam_role_projects and projectsFilter,
	// assuming '{(unassigned)}' in the case that iam_role_projects is empty. If a role of id
	// doesn't exist, this will return a proper SQL "no rows" error when passed to processError.
	row := q.QueryRowContext(ctx, "SELECT COALESCE(projects_match(role_projects($1), $2), false)", id, pq.Array(projectsFilter))

	var result bool
	err := row.Scan(&result)
	if err != nil {
		return false, err
	}
	return result, nil
}

func (p *pg) insertRoleWithQuerier(ctx context.Context, role *storage.Role, q Querier) error {
	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return p.processError(err)
	}

	row := q.QueryRowContext(ctx, `INSERT INTO iam_roles (id, name, type, actions) VALUES ($1, $2, $3, $4)
		RETURNING db_id`,
		role.ID, role.Name, role.Type.String(), pq.Array(role.Actions))
	var dbID string
	if err := row.Scan(&dbID); err != nil {
		return p.processError(err)
	}

	_, err = q.ExecContext(ctx,
		`INSERT INTO iam_role_projects (role_id, project_id)
		SELECT $1, project_db_id(p) FROM unnest($2::TEXT[]) as p`,
		dbID, pq.Array(role.Projects))
	if err != nil {
		return p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return storage.NewTxCommitError(err)
	}

	return nil
}
