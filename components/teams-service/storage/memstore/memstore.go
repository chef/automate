package memstore

import (
	"context"
	"time"

	"github.com/chef/automate/components/teams-service/constants"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/postgres"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
)

// WARNING
// TODO (tc): The storage interface is still using V1 verbiage, so
// name is really the ID in V2 terms. We'll refactor at GA when V1 is removed.

type memstore struct {
	teams       map[uuid.UUID]storage.Team
	teamsV2     map[string]storage.Team
	usersByTeam map[uuid.UUID][]string
	logger      logger.Logger
}

// New instantiates a memstore struct
func New(ctx context.Context, logger logger.Logger) (storage.Storage, error) {
	m := &memstore{
		teams:       make(map[uuid.UUID]storage.Team),
		teamsV2:     make(map[string]storage.Team),
		usersByTeam: make(map[uuid.UUID][]string),
		logger:      logger,
	}

	if err := m.EnsureAdminsTeam(ctx); err != nil {
		return nil, err
	}

	return m, nil
}

// StoreTeam saves a created team to the memstore with the default project
func (m *memstore) StoreTeam(ctx context.Context, name, description string) (storage.Team, error) {
	return m.StoreTeamWithProjects(ctx, name, description, []string{})
}

// StoreTeamWithProjects saves a created team to the memstore with the specified teams
func (m *memstore) StoreTeamWithProjects(_ context.Context,
	name, description string, projects []string) (storage.Team, error) {

	if _, ok := m.teamsV2[name]; ok {
		return storage.Team{}, storage.ErrConflict
	}

	now := time.Now()
	id := uuid.Must(uuid.NewV4())
	team := storage.Team{
		ID:          id,
		Name:        name,
		Description: description,
		Projects:    projects,
		CreatedAt:   now,
		UpdatedAt:   now,
	}

	m.teams[id] = team
	m.teamsV2[name] = team
	m.usersByTeam[id] = make([]string, 0)

	return copyTeam(team), nil
}

// GetTeams fetches teams from memory/db
func (m *memstore) GetTeams(ctx context.Context) ([]storage.Team, error) {
	teamArr := []storage.Team{}

	i := 0
	for _, team := range m.teams {
		if projectsIntersect(ctx, team) {
			teamArr = append(teamArr, copyTeam(team))
		}
		i++
	}

	return teamArr, nil
}

// GetTeam fetches a team from memory
func (m *memstore) GetTeam(ctx context.Context, teamID uuid.UUID) (storage.Team, error) {
	team, teamFound := m.teams[teamID]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}

	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	return copyTeam(team), nil
}

// DeleteTeam deletes a team
func (m *memstore) DeleteTeam(ctx context.Context, teamID uuid.UUID) (storage.Team, error) {
	team, teamFound := m.teams[teamID]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	// TODO (tc) will need to implement similar logic in pg adapter after that code lands,
	// but the server test should catch it. I'll do that by added a can_delete field to pg
	// table once all that code is in place.
	if !storage.IsTeamDeletable(team.Name) {
		return storage.Team{}, storage.ErrCannotDelete
	}

	delete(m.teams, teamID)
	delete(m.teamsV2, team.Name)
	// copy not needed since the team is no longer in the store!
	return team, nil
}

func (m *memstore) EditTeam(ctx context.Context, t storage.Team) (storage.Team, error) {
	team, teamFound := m.teams[t.ID]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	if team.Name != t.Name {
		if _, ok := m.teamsV2[t.Name]; ok {
			return storage.Team{}, storage.ErrConflict
		}
	}

	team.Name = t.Name
	team.Description = t.Description
	// persist change
	m.teams[t.ID] = team
	m.teamsV2[t.Name] = team

	return copyTeam(team), nil
}

// AddUsers adds an array of user IDs to a team's map of user IDs
func (m *memstore) AddUsers(ctx context.Context, teamID uuid.UUID, userIDs []string) (storage.Team, error) {
	team, teamFound := m.teams[teamID]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	teamUserIDs, usersFound := m.usersByTeam[teamID]
	if !usersFound {
		return storage.Team{}, storage.ErrNotFound
	}

	for _, userID := range userIDs {
		if !hasUser(teamUserIDs, userID) {
			teamUserIDs = append(teamUserIDs, userID)
		}
	}

	m.usersByTeam[teamID] = teamUserIDs
	return copyTeam(team), nil
}

// RemoveUsers updates team membership by removing all users from a team it can
// find that are currently members.
func (m *memstore) RemoveUsers(ctx context.Context, teamID uuid.UUID, userIDs []string) (storage.Team, error) {
	team, ok := m.teams[teamID]
	if !ok {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	teamUserIDs, usersFound := m.usersByTeam[teamID]
	if !usersFound {
		return storage.Team{}, storage.ErrNotFound
	}

	for _, userID := range userIDs {
		teamUserIDs = deleteUser(teamUserIDs, userID)
	}

	m.usersByTeam[teamID] = teamUserIDs
	return copyTeam(team), nil
}

// GetTeamByName fetches a team from memory by name instead of ID
func (m *memstore) GetTeamByName(ctx context.Context, teamName string) (storage.Team, error) {
	team, teamFound := m.teamsV2[teamName]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}
	return copyTeam(team), nil
}

// PurgeUserMembership finds all teams the given user is a member of and
// removes the user from their membership, returning the id of all teams modified.
func (m *memstore) PurgeUserMembership(ctx context.Context,
	userID string) (teamsUpdated []uuid.UUID, err error) {
	var foundTeams []uuid.UUID
	for _, team := range m.teams {
		teamUserIDs, usersFound := m.usersByTeam[team.ID]
		if !usersFound {
			continue
		}
		if hasUser(teamUserIDs, userID) {
			teamUserIDs = deleteUser(teamUserIDs, userID)
			m.teams[team.ID] = team
			m.usersByTeam[team.ID] = teamUserIDs
			foundTeams = append(foundTeams, team.ID)
		}
	}
	return foundTeams, nil
}

// GetUserIDsForTeam returns the ids of the users comprising the specified team.
func (m *memstore) GetUserIDsForTeam(ctx context.Context, teamID uuid.UUID) ([]string, error) {
	teamUserIDs, usersFound := m.usersByTeam[teamID]
	if !usersFound {
		return nil, storage.ErrNotFound
	}

	return teamUserIDs, nil
}

// GetTeamsForUser returns teams to which the user belongs
func (m *memstore) GetTeamsForUser(ctx context.Context, userID string) ([]storage.Team, error) {
	teamArr := []storage.Team{}

	for _, team := range m.teams {
		if m.teamIncludesUser(team.ID, userID) {
			if projectsIntersect(ctx, team) {
				teamArr = append(teamArr, team)
			}
		}
	}

	return teamArr, nil
}

func (m *memstore) DeleteTeamByName(ctx context.Context, teamName string) (storage.Team, error) {
	team, teamFound := m.teamsV2[teamName]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	// TODO (tc) will need to implement similar logic in pg adapter after that code lands,
	// but the server test should catch it. I'll do that by added a can_delete field to pg
	// table once all that code is in place.
	if !storage.IsTeamDeletable(teamName) {
		return storage.Team{}, storage.ErrCannotDelete
	}

	delete(m.teams, team.ID)
	delete(m.teamsV2, teamName)
	// copy not needed since the team is no longer in the store!
	return team, nil
}

func (m *memstore) EditTeamByName(ctx context.Context,
	name string, description string, projects []string) (storage.Team, error) {

	team, teamFound := m.teamsV2[name]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	team.Description = description
	team.Projects = projects
	// persist change
	m.teams[team.ID] = team
	m.teamsV2[name] = team

	return copyTeam(team), nil
}

func (m *memstore) teamIncludesUser(teamID uuid.UUID, userID string) bool {
	teamUserIDs, usersFound := m.usersByTeam[teamID]
	if !usersFound {
		return false
	}
	return hasUser(teamUserIDs, userID)
}

func (m *memstore) EnsureAdminsTeam(ctx context.Context) error {
	if _, err := m.StoreTeam(ctx, storage.AdminsTeamName,
		"Members of the admins team, by default, have access to all parts of the API."); err != nil {
		if err != storage.ErrConflict {
			return err
		}
	}
	return nil
}

func (m *memstore) UpgradeToV2(ctx context.Context) error {
	return nil
}

// keeps in-memory team safe
func copyTeam(team storage.Team) storage.Team {
	copyTeam := storage.Team{
		ID:          team.ID,
		Name:        team.Name,
		Description: team.Description,
		Projects:    team.Projects,
	}
	return copyTeam
}

func deleteUser(userIDs []string, userID string) []string {
	for i, ID := range userIDs {
		if ID == userID {
			userIDs[i] = userIDs[len(userIDs)-1]
			userIDs = userIDs[:len(userIDs)-1]
			break
		}
	}
	return userIDs
}

func hasUser(userIDs []string, userID string) bool {
	for _, ID := range userIDs {
		if ID == userID {
			return true
		}
	}
	return false
}

func projectsIntersect(ctx context.Context, team storage.Team) bool {
	projectsFilter, err := postgres.ProjectsListFromContext(ctx)
	if err != nil {
		return false
	}

	if len(projectsFilter) == 0 {
		return true
	}

	teamProjects := team.Projects
	if len(teamProjects) == 0 {
		teamProjects = []string{constants.UnassignedProjectID}
	}

	for _, projectFilter := range projectsFilter {
		for _, project := range teamProjects {
			if projectFilter == project {
				return true
			}
		}
	}
	return false
}
