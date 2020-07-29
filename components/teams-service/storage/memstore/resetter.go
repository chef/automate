// +build !prod

package memstore

import (
	"context"

	"github.com/chef/automate/components/teams-service/storage"
)

// Reset the memstore to empty
func (m *memstore) Reset(ctx context.Context) error {
	m.teams = make(map[string]storage.Team)
	return m.EnsureAdminsTeam(ctx)
}
