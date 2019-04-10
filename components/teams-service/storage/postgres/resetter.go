// +build !prod

package postgres

import (
	"context"
	"fmt"
	"strings"

	"github.com/chef/automate/components/teams-service/storage"
)

func (p *postgres) Reset(ctx context.Context) error {
	// Delete all except NonDeletableTeams since that is not allowed
	// and an error will occur.
	queryArgsArr := make([]string, len(storage.NonDeletableTeams))
	for i, nonDeletableTeam := range storage.NonDeletableTeams {
		queryArgsArr[i] = "'" + nonDeletableTeam + "'"
	}
	queryArgs := strings.Join(queryArgsArr, ",")
	query := fmt.Sprintf("DELETE FROM teams WHERE name NOT IN (%s);", queryArgs) // nolint: gas
	_, err := p.db.ExecContext(ctx, query)
	if err != nil {
		return err
	}

	_, err = p.db.ExecContext(ctx, "DELETE FROM teams_users_associations")
	return err
}
