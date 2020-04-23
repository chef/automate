package storage

import (
	"context"
	"errors"
	"time"

	"github.com/chef/automate/lib/stringutils"
)

// Storage is the interface provided by our various storage backends.
type Storage interface {
	GetTeams(context.Context) ([]Team, error)
	StoreTeam(ctx context.Context, id string, name string, projects []string) (Team, error)
	RemoveUsers(ctx context.Context, id string, userIDs []string) ([]string, error)
	GetTeam(ctx context.Context, id string) (Team, error)
	PurgeUserMembership(ctx context.Context, userID string) (teamsUpdated []string, err error)
	AddUsers(ctx context.Context, id string, userIDs []string) ([]string, error)
	GetTeamsForUser(ctx context.Context, userID string) ([]Team, error)
	GetUserIDsForTeam(ctx context.Context, id string) ([]string, error)
	DeleteTeam(ctx context.Context, id string) (Team, error)
	EditTeam(ctx context.Context, id string, name string, projects []string) (Team, error)
	PurgeProject(ctx context.Context, id string) error
}

// Resetter is, if exposed, used for tests to reset the storage backend to a
// pristine state.
type Resetter interface {
	Reset(context.Context) error
}

// Team is the struct ingested and returned by our backend implementations.
type Team struct {
	ID        string
	Name      string
	CreatedAt time.Time
	UpdatedAt time.Time
	Projects  []string
}

// Errors returned from the backend
var (
	// ErrNotFound is returned when a requested team wasn't found
	ErrNotFound = errors.New("not found")

	// ErrConflict is returned when a team there is a clash of team IDs
	ErrConflict = errors.New("conflict")

	// ErrCannotDelete is returned when a request attempts to delete a team that
	// is not allowed to be deleted such as the "admins" team.
	ErrCannotDelete = errors.New("cannot delete")

	// If this id changes, we need a migration to change the id, see 02_add-deletable-column.up.sql.
	AdminsTeamID = "admins"

	// NonDeletableTeams contains all the teams that are not allowed to be deleted.
	// If this id changes, we need a migration to add the new team, see 02_add-deletable-column.up.sql.
	NonDeletableTeams = []string{AdminsTeamID}
)

// IsTeamDeletable returns whether or not an input team is deletable by id.
func IsTeamDeletable(teamID string) bool {
	return !stringutils.SliceContains(NonDeletableTeams, teamID)
}
