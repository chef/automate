package postgres

import (
	"context"
	"fmt"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
)

func (p *Postgres) AddPolicyCookbooks(ctx context.Context,
	cookbooks []backend.PolicyCookbookLock, policyRevisionID, policyName string) error {
	_, err := p.db.ExecContext(ctx,
		`INSERT INTO policy_rev (id, policy_name)
				VALUES ($1, $2) ON CONFLICT DO NOTHING`,
		policyRevisionID, policyName)

	if err != nil {
		return err
	}

	for _, cookbook := range cookbooks {
		_, err = p.db.ExecContext(ctx,
			`INSERT INTO cookbook_lock (policy_id, policy_rev_id, cookbook_name)
				VALUES ($1, $2, $3) ON CONFLICT DO NOTHING`,
			cookbook.PolicyID, policyRevisionID, cookbook.CookbookName)
		if err != nil {
			return err
		}
	}
	return nil
}

const getCookbookLocks = `
  SELECT  cookbook_name
         ,policy_id
	
		FROM cookbook_lock AS cl
		WHERE cl.policy_rev_id = $1
`

const getPolicyRev = `
	SELECT  policy_name
	
		FROM policy_rev AS p
		WHERE p.id = $1
`

type PolicyRev struct {
	PolicyName string `db:"policy_name"`
	ID         string `db:"id"`
}

func (p *Postgres) GetPolicyCookbooks(revisionID string) (backend.PolicyCookbooks, error) {
	cookbooks, err := p.getCookbooks(revisionID)
	if err != nil {
		return backend.PolicyCookbooks{}, err
	}

	policyRevName, err := p.mapper.SelectStr(getPolicyRev, revisionID)
	if err != nil {
		return backend.PolicyCookbooks{}, err
	}

	if len(policyRevName) == 0 {
		return backend.PolicyCookbooks{}, errors.New(errors.PolicyCookbooksNotFound,
			fmt.Sprintf("No policy cookbooks found for revision ID: %s", revisionID))
	}

	return backend.PolicyCookbooks{
		PolicyName:    policyRevName,
		RevisionID:    revisionID,
		CookbookLocks: cookbooks,
	}, nil
}

func (p *Postgres) getCookbooks(revisionID string) ([]*backend.PolicyCookbookLock, error) {
	var cookbooks []*backend.PolicyCookbookLock
	_, err := p.mapper.Select(&cookbooks, getCookbookLocks, revisionID)

	return cookbooks, err
}
