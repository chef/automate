// +build !prod

package postgres

import (
	"context"
	"fmt"
	"strings"

	constants "github.com/chef/automate/components/authz-service/constants/v1"
	storage "github.com/chef/automate/components/authz-service/storage/v1"
)

func (p *pg) Reset(ctx context.Context) error {
	queryArgsArr := make([]string, len(constants.NonDeletablePolicyIDs))
	for i, nonDeletablePolicyID := range constants.NonDeletablePolicyIDs {
		queryArgsArr[i] = "'" + nonDeletablePolicyID + "'"
	}
	queryArgs := strings.Join(queryArgsArr, ",")
	query := fmt.Sprintf("DELETE FROM policies WHERE id NOT IN (%s);", queryArgs) // nolint: gas
	_, err := p.db.ExecContext(ctx, query)
	if err != nil {
		return err
	}

	defaultPolicies, err := storage.DefaultPolicies()
	if err != nil {
		return err
	}

	for _, policy := range defaultPolicies {
		pm := policyMap{
			Subjects: policy.Subjects,
			Action:   policy.Action,
			Resource: policy.Resource,
			Effect:   policy.Effect,
		}

		_, err := p.db.ExecContext(ctx,
			`INSERT INTO policies (id, policy_data, version)
						VALUES ($1, $2, $3)
						ON CONFLICT DO NOTHING`,
			policy.ID.String(), pm, storage.Version)
		if err != nil {
			return err
		}
	}

	return nil
}
