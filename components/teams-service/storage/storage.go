package storage

import (
	"context"
	"errors"
	"time"

	"github.com/chef/automate/lib/stringutils"
	uuid "github.com/chef/automate/lib/uuid4"
)

// WARNING
// TODO (tc): The storage interface is still using V1 verbiage, so
// name is really the ID in V2 terms. We'll refactor at GA when V1 is removed.
//
// Storage is the interface provided by our various storage backends.
type Storage interface {
	DeleteTeam(context.Context, uuid.UUID) (Team, error)
	EditTeam(context.Context, Team) (Team, error)
	GetTeams(context.Context) ([]Team, error)
	GetTeam(context.Context, uuid.UUID) (Team, error)
	StoreTeam(ctx context.Context, name string, description string) (Team, error)
	StoreTeamWithProjects(ctx context.Context, name string, description string, projects []string) (Team, error)
	RemoveUsers(ctx context.Context, id uuid.UUID, users []string) (Team, error)
	GetTeamByName(ctx context.Context, teamName string) (Team, error)
	PurgeUserMembership(ctx context.Context, userID string) (teamsUpdated []uuid.UUID, err error)
	AddUsers(ctx context.Context, teamID uuid.UUID, users []string) (Team, error)
	GetTeamsForUser(ctx context.Context, userID string) ([]Team, error)
	GetUserIDsForTeam(ctx context.Context, teamID uuid.UUID) ([]string, error)
	// Added for V2. When deprecating V1, review functions above this comment
	// to see what is still needed and what can be removed.
	DeleteTeamByName(context.Context, string) (Team, error)
	EditTeamByName(context.Context, string, string, []string) (Team, error)
	UpgradeToV2(context.Context) error
}

// Resetter is, if exposed, used for tests to reset the storage backend to a
// pristine state.
type Resetter interface {
	Reset(context.Context) error
}

// Team is the struct ingested and returned by our backend implementations.
type Team struct {
	ID          uuid.UUID
	Name        string
	Description string
	CreatedAt   time.Time
	UpdatedAt   time.Time
	Projects    []string
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

	// If this name changes, we need a migration to change the name, see 02_add-deletable-column.up.sql.
	AdminsTeamName = "admins"

	// NonDeletableTeams contains all the teams that are not allowed to be deleted.
	// If this name changes, we need a migration to add the new team, see 02_add-deletable-column.up.sql.
	NonDeletableTeams = []string{AdminsTeamName}
)

// IsTeamDeletable returns whether or not an input team is deletable by name.
func IsTeamDeletable(teamName string) bool {
	return !stringutils.SliceContains(NonDeletableTeams, teamName)
}
