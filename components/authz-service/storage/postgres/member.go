package postgres

import (
	"context"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/storage"
)

func (p *pg) ListPolicyMembers(ctx context.Context, id string) ([]storage.Member, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	// Project filtering handled in here. We'll return a 404 here right away if
	// we can't find the policy via ID as filtered by projects.
	_, err = p.queryPolicy(ctx, id, tx, false)
	if err != nil {
		return nil, p.processError(err)
	}

	members, err := p.getPolicyMembersWithQuerier(ctx, id, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	return members, nil
}

func (p *pg) AddPolicyMembers(ctx context.Context, id string, members []storage.Member) ([]storage.Member, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	// Project filtering handled in here. We'll return a 404 right away if we can't find
	// the policy via ID as filtered by projects.
	_, err = p.queryPolicy(ctx, id, tx, false)
	if err != nil {
		return nil, p.processError(err)
	}

	for _, member := range members {
		err := p.insertOrReusePolicyMemberWithQuerier(ctx, id, member, tx)
		if err != nil {
			return nil, p.processError(err)
		}
	}

	members, err = p.getPolicyMembersWithQuerier(ctx, id, tx)
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
	return members, nil
}

func (p *pg) ReplacePolicyMembers(ctx context.Context, policyID string, members []storage.Member) ([]storage.Member, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	// Project filtering handled in here. We'll return a 404 right away if we can't find
	// the policy via ID as filtered by projects.
	_, err = p.queryPolicy(ctx, policyID, tx, false)
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.replacePolicyMembersWithQuerier(ctx, policyID, members, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	// fetch fresh data so returned data will reflect that any pre-existing members re-use existing IDs
	members, err = p.getPolicyMembersWithQuerier(ctx, policyID, tx)
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

	return members, err
}

// RemovePolicyMembers takes in a policy ID and a
// list of members to remove and return the list of remaining users.
func (p *pg) RemovePolicyMembers(ctx context.Context,
	policyID string, members []storage.Member) ([]storage.Member, error) {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil /* use driver default */)
	if err != nil {
		return nil, p.processError(err)
	}

	// Project filtering handled in here. We'll return a 404 right away if we can't find
	// the policy via ID as filtered by projects.
	_, err = p.queryPolicy(ctx, policyID, tx, false)
	if err != nil {
		return nil, p.processError(err)
	}

	// Note: we're not using member_db_id() here, since we want to gracefully
	// ignore "not found" errors.
	for _, member := range members {
		_, err := tx.ExecContext(ctx,
			`DELETE FROM iam_policy_members WHERE policy_id=policy_db_id($1) AND
				member_id=(SELECT db_id from iam_members WHERE name=$2)`,
			policyID, member.Name)
		if err != nil {
			err = p.processError(err)
			switch err {
			case storage.ErrNotFound: // continue
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

	err = p.notifyPolicyChange(ctx, tx)
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	return members, nil
}

func (p *pg) replacePolicyMembersWithQuerier(ctx context.Context, policyID string, members []storage.Member,
	q Querier) error {
	// Cascading drop any existing members.
	_, err := q.ExecContext(ctx,
		`DELETE FROM iam_policy_members WHERE policy_id=policy_db_id($1);`, policyID)
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
func (p *pg) insertOrReusePolicyMemberWithQuerier(ctx context.Context, policyID string, member storage.Member,
	q Querier) error {
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

func (p *pg) getPolicyMembersWithQuerier(ctx context.Context, id string, q Querier) ([]storage.Member, error) {
	rows, err := q.QueryContext(ctx,
		`SELECT m.name FROM iam_policy_members AS pm
			JOIN iam_members AS m ON pm.member_id=m.db_id
			WHERE pm.policy_id=policy_db_id($1) ORDER BY m.name ASC`, id)

	if err != nil {
		return nil, err
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	members := []storage.Member{}
	for rows.Next() {
		var member storage.Member
		if err := rows.Scan(&member.Name); err != nil {
			return nil, p.processError(err)
		}
		members = append(members, member)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return members, nil
}
