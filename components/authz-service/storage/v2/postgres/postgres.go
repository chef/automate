package postgres

import (
	"context"
	"database/sql"
	"fmt"
	"strings"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	v2 "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
)

type pg struct {
	db          *sql.DB
	logger      logger.Logger
	dataMigConf datamigration.Config
}

// New instantiates the postgres storage backend.
func New(ctx context.Context, l logger.Logger, migConf migration.Config,
	dataMigConf datamigration.Config) (v2.Storage, error) {

	l.Infof("applying database migrations from %s to %s", migConf.Path, migConf.PGURL.String())

	db, err := postgres.New(ctx, migConf)
	if err != nil {
		return nil, err
	}

	return &pg{db: db, logger: l, dataMigConf: dataMigConf}, nil
}

type Querier interface {
	ExecContext(context.Context, string, ...interface{}) (sql.Result, error)
	QueryContext(context.Context, string, ...interface{}) (*sql.Rows, error)
	QueryRowContext(context.Context, string, ...interface{}) *sql.Row
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * POLICIES  * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// CreatePolicy stores a new policy and its statements in postgres and returns the final policy.
func (p *pg) CreatePolicy(ctx context.Context, pol *v2.Policy) (*v2.Policy, error) {
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

	err = p.insertPolicyWithQuerier(ctx, pol, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.associatePolicyWithProjects(ctx, pol.ID, pol.Projects, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.insertPolicyStatementsWithQuerier(ctx, pol.ID, pol.Statements, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
	}

	// Currently, we don't change anything from what is passed in.
	return pol, nil
}

func (p *pg) PurgeSubjectFromPolicies(ctx context.Context, sub string) ([]string, error) {
	var polIDs []string
	// Note(sr) 2018-11-26: We're keeping the iam_members reference. Should we
	// remove it? "Just" removing the iam_members entry and relying to CASCADE to
	// remove the membership rows from iam_policy_members doesn't do the trick here
	// -- not if we care about the affected policy IDs. (We at the moment don't
	// prescribe this, but it feels like the better choice.)
	row := p.db.QueryRowContext(ctx, `
WITH pol_ids AS (DELETE FROM iam_policy_members
                 WHERE member_id=(SELECT id FROM iam_members WHERE name=$1)
                 RETURNING policy_id)
SELECT array_agg(policy_id) FROM pol_ids`,
		sub)
	err := row.Scan(pq.Array(&polIDs))
	if err != nil {
		return nil, p.processError(err)
	}

	return polIDs, nil
}

func (p *pg) ListPolicies(ctx context.Context) ([]*v2.Policy, error) {
	var pols []*v2.Policy
	rows, err := p.db.QueryContext(ctx, `SELECT query_policies from query_policies();`)

	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		var pol v2.Policy
		err = rows.Scan(&pol)
		if err != nil {
			return nil, p.processError(err)
		}
		pols = append(pols, &pol)
	}

	return pols, nil
}

func (p *pg) GetPolicy(ctx context.Context, id string) (*v2.Policy, error) {
	pol, err := p.queryPolicy(ctx, id)
	if err != nil {
		return nil, p.processError(err)
	}

	return pol, nil
}

func (p *pg) DeletePolicy(ctx context.Context, id string) error {
	_, err := p.queryPolicy(ctx, id)
	if err != nil {
		return p.processError(err)
	}

	_, err = p.db.ExecContext(ctx,
		`DELETE FROM iam_policies WHERE id=$1;`,
		id,
	)
	if err != nil {
		return p.processError(err)
	}

	return nil
}

func (p *pg) UpdatePolicy(ctx context.Context, pol *v2.Policy) (*v2.Policy, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	// Since we are forcing users to update the entire policy, we should delete
	// all existing statements for simplicity for now. Let's not delete the actual
	// policy row to preserve that record / id.
	//
	// This will cascade delete all related statements.
	_, err = tx.ExecContext(ctx,
		`DELETE FROM iam_policy_statements WHERE policy_id=$1;`,
		pol.ID,
	)
	if err != nil {
		err = p.processError(err)
		switch err {
		// Ignore not found errors here since a policy doesn't have to have statements.
		case storage_errors.ErrNotFound: // continue
		default:
			return nil, err
		}
	}

	rows, err := tx.QueryContext(ctx,
		`UPDATE iam_policies SET (name, type) =
			($2, $3) WHERE id = $1 RETURNING id;`,
		pol.ID, pol.Name, pol.Type.String(),
	)
	if err != nil {
		return nil, p.processError(err)
	}

	// Close is idempotent so no worries if it gets called twice.
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	if !rows.Next() {
		return nil, storage_errors.ErrNotFound
	}

	// Must close all rows of a transaction before opening new rows below.
	// Otherwise, you get a super unhelpful error from pq like:
	// pq: unexpected Parse response 'C'
	err = rows.Close()
	if err != nil {
		p.logger.Warnf("failed to close db rows: %s", err.Error())
		return nil, p.processError(err)
	}

	// Update policy's projects
	err = p.associatePolicyWithProjects(ctx, pol.ID, pol.Projects, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	// Also replace any existing policy members and update with new members.
	err = p.replacePolicyMembersWithQuerier(ctx, pol.ID, pol.Members, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.insertPolicyStatementsWithQuerier(ctx, pol.ID, pol.Statements, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
	}

	// Currently, we don't change anything from what is passed in.
	return pol, nil
}

func (p *pg) ApplyV2DataMigrations(_ context.Context) error {
	return p.dataMigConf.Migrate()
}

// insertPolicyWithQuerier inserts a new custom policy. It does not return the
// new policy since there are no DEFAULTS in the iam_policy table.
func (p *pg) insertPolicyWithQuerier(ctx context.Context, inputPol *v2.Policy, q Querier) error {
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
	policyID string, inputStatements []v2.Statement,
	q Querier) error {
	for _, s := range inputStatements {
		_, err := q.ExecContext(ctx,
			`SELECT insert_iam_statement_into_policy($1, $2, $3, $4, $5, $6, $7);`,
			policyID, s.ID, s.Effect.String(), pq.Array(s.Actions),
			pq.Array(s.Resources), s.Role, pq.Array(s.Projects),
		)
		if err != nil {
			err = p.processError(err)
			switch err {
			case storage_errors.ErrForeignKey: // occurs when a project in the statement does not exist
				return errors.Errorf("not allowed: one or more of the projects %s does not exist", s.Projects)
			default:
				return err
			}
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
		`DELETE FROM iam_policy_projects WHERE policy_id=$1;`, policyID)
	if err != nil {
		return err
	}
	for _, project := range inProjects {
		_, err := q.ExecContext(ctx,
			`INSERT INTO iam_policy_projects (policy_id, project_id) VALUES ($1, $2)`,
			&policyID, &project)
		if err != nil {
			err = p.processError(err)
			switch err {
			case storage_errors.ErrForeignKey: // occurs when a project in the policy does not exist
				return errors.Errorf("not allowed: one or more of the projects %s does not exist", inProjects)
			default:
				return err
			}
		}
	}

	return nil
}

// queryPolicy returns a policy based on id or an error.
func (p *pg) queryPolicy(ctx context.Context, id string) (*v2.Policy, error) {
	var pol v2.Policy
	row := p.db.QueryRowContext(ctx, `SELECT query_policy from query_policy($1);`, id)
	err := row.Scan(&pol)
	if err != nil {
		return nil, err
	}
	return &pol, nil
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * *  MEMBERS  * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

func (p *pg) ListPolicyMembers(ctx context.Context, id string) ([]v2.Member, error) {
	members, err := p.getPolicyMembers(ctx, id)
	if err != nil {
		return nil, p.processError(err)
	}

	return members, nil
}

func (p *pg) AddPolicyMembers(ctx context.Context, id string, members []v2.Member) ([]v2.Member, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	for _, member := range members {
		err := p.insertOrReusePolicyMemberWithQuerier(ctx, id, member, tx)
		if err != nil {
			err = p.processError(err)
			switch err {
			case storage_errors.ErrForeignKey: // occurs when id not found for policy
				return nil, storage_errors.ErrNotFound
			default:
				return nil, err
			}
		}
	}

	members, err = p.getPolicyMembersWithQuerier(ctx, id, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
	}
	return members, nil
}

func (p *pg) ReplacePolicyMembers(ctx context.Context, policyID string, members []v2.Member) ([]v2.Member, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.replacePolicyMembersWithQuerier(ctx, policyID, members, tx)
	if err != nil {
		err = p.processError(err)
		switch err {
		case storage_errors.ErrForeignKey: // occurs when id not found for policy
			return nil, storage_errors.ErrNotFound
		default:
			return nil, err
		}
	}

	// fetch fresh data so returned data will reflect that any pre-existing members re-use existing IDs
	members, err = p.getPolicyMembersWithQuerier(ctx, policyID, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
	}

	return members, err
}

// RemovePolicyMembers takes in a policy ID and a
// list of members to remove and return the list of remaining users.
func (p *pg) RemovePolicyMembers(ctx context.Context,
	policyID string, members []v2.Member) ([]v2.Member, error) {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	for _, member := range members {
		_, err := tx.ExecContext(ctx,
			`DELETE FROM iam_policy_members WHERE policy_id=$1 AND
				member_id=(SELECT id from iam_members WHERE name=$2);`, policyID, member.Name)
		if err != nil {
			err = p.processError(err)
			switch err {
			case storage_errors.ErrNotFound: // continue
			case storage_errors.ErrForeignKey:
				return nil, storage_errors.ErrNotFound
			default:
				return nil, err
			}
		}
	}

	// fetch fresh data so returned data will reflect that any pre-existing members re-use existing IDs
	members, err = p.getPolicyMembersWithQuerier(ctx, policyID, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
	}

	return members, nil
}

func (p *pg) replacePolicyMembersWithQuerier(ctx context.Context, policyID string, members []v2.Member,
	q Querier) error {
	// Cascading drop any existing members.
	_, err := q.ExecContext(ctx,
		`DELETE FROM iam_policy_members WHERE policy_id=$1;`, policyID)
	if err != nil {
		return err
	}

	// Insert new members.
	for _, member := range members {
		err = p.insertOrReusePolicyMemberWithQuerier(ctx, policyID, member, q)
		if err != nil {
			return err
		}
	}
	return nil
}

// insertOrReusePolicyMemberWithQuerier takes in a member (including a new ID) and a policyID.
// If the member already exists in iam_members, it will ignore the new ID and use
// the existing one. Otherwise, it'll just use the existing ID. In either case,
// it inserts the new or existing member into iam_policy_members association table.
func (p *pg) insertOrReusePolicyMemberWithQuerier(ctx context.Context, policyID string, member v2.Member,
	q Querier) error {
	// First, we insert the member but on conflict do nothing. Then, we insert the member
	// into the policy. This is safe to do non-transactionally right now, since we don't support
	// updating either iam_members id or name columns which is the entire table. Also, we are currently
	// not deleting any of the rows, but reusing them per name string.

	_, err := q.ExecContext(ctx, `INSERT INTO iam_members (id, name)  VALUES ($1, $2) ON CONFLICT DO NOTHING;`,
		member.ID, member.Name)
	if err != nil {
		return err
	}

	// For now, let's just ignore conflicts if someone is trying to add a user that is already a member.
	_, err = q.ExecContext(ctx,
		`INSERT INTO iam_policy_members (policy_id, member_id)
			values($1, (SELECT id FROM iam_members WHERE name=$2)) ON CONFLICT DO NOTHING;`, policyID, member.Name)
	return err
}

func (p *pg) getPolicyMembers(ctx context.Context, id string) ([]v2.Member, error) {
	return p.getPolicyMembersWithQuerier(ctx, id, p.db)
}

func (p *pg) getPolicyMembersWithQuerier(ctx context.Context, id string, q Querier) ([]v2.Member, error) {
	var exists bool
	err := q.QueryRowContext(ctx, `SELECT EXISTS(SELECT 1 FROM iam_policies WHERE id=$1)`, id).Scan(&exists)
	if err != nil {
		return nil, errors.Wrap(err, "check policy existence")
	}
	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	rows, err := q.QueryContext(ctx,
		`SELECT m.id, m.name FROM iam_policy_members AS pm
			INNER JOIN iam_members AS m ON pm.member_id=m.id
			WHERE pm.policy_id=$1 ORDER BY m.name ASC;`, id)

	if err != nil {
		return nil, err
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	members := []v2.Member{}
	for rows.Next() {
		var member v2.Member
		err = rows.Scan(&member.ID, &member.Name)
		if err != nil {
			return nil, p.processError(err)
		}
		members = append(members, member)
	}
	return members, nil
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * *   ROLES   * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

func (p *pg) CreateRole(ctx context.Context, role *v2.Role) (*v2.Role, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.insertRoleWithQuerier(ctx, role, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
	}

	return role, nil
}

func (p *pg) ListRoles(ctx context.Context) ([]*v2.Role, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
	}

	var roles []*v2.Role
	rows, err := p.db.QueryContext(ctx, `SELECT query_roles from query_roles($1);`, pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		var role v2.Role
		err = rows.Scan(&role)
		if err != nil {
			return nil, p.processError(err)
		}
		roles = append(roles, &role)
	}

	return roles, nil
}

func (p *pg) GetRole(ctx context.Context, id string) (*v2.Role, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
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
		return nil, storage_errors.ErrNotFound
	}

	var role v2.Role
	row := tx.QueryRowContext(ctx, `SELECT query_role($1);`, id)
	err = row.Scan(&role)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
	}

	return &role, nil
}

func (p *pg) DeleteRole(ctx context.Context, id string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return p.processError(err)
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
		return storage_errors.ErrNotFound
	}

	res, err := tx.ExecContext(ctx, `DELETE FROM iam_roles WHERE id=$1;`, id)
	if err != nil {
		return p.processError(err)
	}

	count, err := res.RowsAffected()
	if err != nil {
		return p.processError(err)
	} else if count != 1 {
		return storage_errors.ErrNotFound
	}

	err = tx.Commit()
	if err != nil {
		return storage_errors.NewErrTxCommit(err)
	}

	return nil
}

func (p *pg) UpdateRole(ctx context.Context, role *v2.Role) (*v2.Role, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
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
		return nil, storage_errors.ErrNotFound
	}

	row := tx.QueryRowContext(ctx,
		`UPDATE iam_roles SET (name, actions) =
			($2, $3) WHERE id = $1 RETURNING db_id;`,
		role.ID, role.Name, pq.Array(role.Actions),
	)
	// TODO: check not found case
	var dbID string
	if err := row.Scan(&dbID); err != nil {
		return nil, p.processError(err)
	}

	// TODO bd: we'll be adding the authorization check in the handler of the gateway
	// to ensure this prior to beta release

	// bd: for now, the below sql query assumes that all desired project changes are authorized
	// so all we need to do is replace the existing projects with the desired projects
	_, err = tx.ExecContext(ctx,
		`DELETE FROM iam_role_projects WHERE role_id=$1`, &dbID)
	if err != nil {
		return nil, p.processError(err)
	}

	if len(role.Projects) > 0 {
		sql := "INSERT INTO iam_role_projects (role_id, project_id) VALUES "
		for _, projectID := range role.Projects {
			sql += fmt.Sprintf("(%s, '%s'), ", dbID, projectID)
		}
		sql = strings.TrimSuffix(sql, ", ")

		_, err = tx.ExecContext(ctx, sql)
		if err != nil {
			return nil, p.processError(err)
		}
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage_errors.NewErrTxCommit(err)
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
	// doesn't exist, this will return 0 rows which will bubble up to NotFoundErr when passed to processError.
	row := q.QueryRowContext(ctx,
		`SELECT COALESCE(array_agg(rp.project_id)
				FILTER (WHERE rp.project_id IS NOT NULL), '{(unassigned)}') && $2 AS intersection
			FROM iam_roles AS r
			LEFT OUTER JOIN iam_role_projects AS rp ON rp.role_id=r.db_id
			WHERE r.id = $1 GROUP BY rp.project_id;`,
		id, pq.Array(projectsFilter))

	var result bool
	err := row.Scan(&result)
	if err != nil {
		return false, err
	}
	return result, nil
}

func (p *pg) insertRoleWithQuerier(ctx context.Context, role *v2.Role, q Querier) error {
	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return p.processError(err)
	}

	row := q.QueryRowContext(ctx, `INSERT INTO iam_roles (id, name, type, actions)  VALUES ($1, $2, $3, $4)
		RETURNING db_id;`,
		role.ID, role.Name, role.Type.String(), pq.Array(role.Actions))
	var dbID string
	if err := row.Scan(&dbID); err != nil {
		return p.processError(err)
	}

	for _, project := range role.Projects {
		_, err := q.ExecContext(ctx,
			`INSERT INTO iam_role_projects (role_id, project_id) VALUES ($1, $2)`,
			&dbID, &project)
		if err != nil {
			return p.processError(err)
		}
	}

	err = tx.Commit()
	if err != nil {
		return storage_errors.NewErrTxCommit(err)
	}

	return nil
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * *   PROJECTS  * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

func (p *pg) CreateProject(ctx context.Context, project *v2.Project) (*v2.Project, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if err := p.insertProjectWithQuerier(ctx, project, p.db); err != nil {
		return nil, p.processError(err)
	}

	// Currently, we don't change anything from what is passed in.
	return project, nil
}

func (p *pg) UpdateProject(ctx context.Context, project *v2.Project) (*v2.Project, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
	}

	// Update project if ID found AND intersection between projects and projectsFilter,
	// unless the projectsFilter is empty (v2.0 case).
	res, err := p.db.ExecContext(ctx,
		`UPDATE iam_projects SET name =
			$2 WHERE id = $1 AND (array_length($3::TEXT[], 1) IS NULL OR projects && $3);`,
		project.ID, project.Name, pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}

	count, err := res.RowsAffected()
	if err != nil {
		return nil, p.processError(err)
	} else if count != 1 {
		return nil, storage_errors.ErrNotFound
	}

	// Currently, we don't change anything from what is passed in.
	return project, nil
}

func (p *pg) GetProject(ctx context.Context, id string) (*v2.Project, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
	}

	var project v2.Project
	// Retrieve project if ID found AND intersection between projects and projectsFilter,
	// unless the projectsFilter is empty (v2.0 case).
	row := p.db.QueryRowContext(ctx, `SELECT query_project($1, $2)`, id, pq.Array(projectsFilter))
	if err := row.Scan(&project); err != nil {
		return nil, p.processError(err)
	}

	return &project, nil
}

func (p *pg) DeleteProject(ctx context.Context, id string) error {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return p.processError(err)
	}

	// Delete project if ID found AND intersection between projects and projectsFilter,
	// unless the projectsFilter is empty (v2.0 case).
	res, err := p.db.ExecContext(ctx,
		`DELETE FROM iam_projects WHERE id=$1 AND (array_length($2::TEXT[], 1) IS NULL OR projects && $2);`,
		id, pq.Array(projectsFilter),
	)
	if err != nil {
		return p.processError(err)
	}

	count, err := res.RowsAffected()
	if err != nil {
		return p.processError(err)
	} else if count != 1 {
		return storage_errors.ErrNotFound
	}

	return nil
}

func (p *pg) ListProjects(ctx context.Context) ([]*v2.Project, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
	}

	// List all projects that have intersection between projects and projectsFilter,
	// unless the projectsFilter is empty (v2.0 case).
	rows, err := p.db.QueryContext(ctx, `SELECT query_projects($1)`, pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	var projects []*v2.Project
	for rows.Next() {
		var project v2.Project
		err = rows.Scan(&project)
		if err != nil {
			return nil, p.processError(err)
		}
		projects = append(projects, &project)
	}

	return projects, nil
}

func (p *pg) insertProjectWithQuerier(ctx context.Context, project *v2.Project, q Querier) error {
	_, err := q.ExecContext(ctx, `INSERT INTO iam_projects (id, name, type, projects)  VALUES ($1, $2, $3, $4);`,
		project.ID, project.Name, project.Type.String(), pq.Array([]string{project.ID}))
	if err != nil {
		return err
	}
	return nil
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * SUPPORT * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

func (p *pg) Reset(ctx context.Context) error {
	if _, err := p.db.ExecContext(ctx,
		`TRUNCATE TABLE iam_policies, iam_members, iam_roles, iam_projects CASCADE;`); err != nil {
		return errors.Wrap(err, "truncate database")
	}

	if err := p.dataMigConf.Reset(); err != nil {
		return errors.Wrap(err, "reset v2 data migrations")
	}

	return nil
}

func (p *pg) Close() error {
	return errors.Wrap(p.db.Close(), "close database connection")
}

func (p *pg) Pristine(ctx context.Context) error {
	return p.recordMigrationStatus(ctx, enumPristine)
}

func (p *pg) Success(ctx context.Context) error {
	return p.recordMigrationStatus(ctx, enumSuccessful)
}

func (p *pg) SuccessBeta1(ctx context.Context) error {
	return p.recordMigrationStatus(ctx, enumSuccessfulBeta1)
}

func (p *pg) InProgress(ctx context.Context) error {
	return p.recordMigrationStatus(ctx, enumInProgress)
}

func (p *pg) Failure(ctx context.Context) error {
	return p.recordMigrationStatus(ctx, enumFailed)
}

func (p *pg) MigrationStatus(ctx context.Context) (v2.MigrationStatus, error) {
	var status string
	row := p.db.QueryRowContext(ctx, `SELECT state FROM migration_status`)
	err := row.Scan(&status)
	if err != nil {
		return 0, err // shouldn't happen, migration initializes state
	}
	switch status {
	case enumPristine:
		return v2.Pristine, nil
	case enumSuccessful:
		return v2.Successful, nil
	case enumSuccessfulBeta1:
		return v2.SuccessfulBeta1, nil
	case enumInProgress:
		return v2.InProgress, nil
	case enumFailed:
		return v2.Failed, nil
	}
	return 0, fmt.Errorf("unexpected migration status: %q", status)
}

const (
	enumPristine        = "init"
	enumInProgress      = "in-progress"
	enumSuccessful      = "successful"
	enumSuccessfulBeta1 = "successful-beta1"
	enumFailed          = "failed"
)

func (p *pg) recordMigrationStatus(ctx context.Context, ms string) error {
	_, err := p.db.ExecContext(ctx, `UPDATE migration_status SET state=$1`, ms)
	return err
}

func (p *pg) processError(err error) error {
	p.logger.Debugf("err: %v", err)
	err = postgres.ProcessError(err)
	if err == storage_errors.ErrDatabase {
		p.logger.Warnf("unknown error type from database: %v", err)
	}
	return err
}

// projectsListFromContext returns the project list from the context.
// In the case that the project list was ["*"], we return an empty list,
// since we do not wish to filter on projects.
func projectsListFromContext(ctx context.Context) ([]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		projectsFilter = []string{}
	}
	return projectsFilter, nil
}
