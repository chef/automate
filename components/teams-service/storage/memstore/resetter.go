// +build !prod

package memstore

import (
	"context"

	"github.com/chef/automate/components/teams-service/storage"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Reset the memstore to empty
func (m *memstore) Reset(ctx context.Context) error {
	m.teams = make(map[uuid.UUID]storage.Team)
	m.teamsV2 = make(map[string]storage.Team)
	return m.EnsureAdminsTeam(ctx)
}
