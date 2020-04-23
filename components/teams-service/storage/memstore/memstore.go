package memstore

import (
	"context"
	"time"

	"github.com/chef/automate/components/teams-service/constants"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/postgres"
	"github.com/chef/automate/lib/logger"
)

type memstore struct {
	teams       map[string]storage.Team
	usersByTeam map[string][]string
	logger      logger.Logger
}

// New instantiates a memstore struct
func New(ctx context.Context, logger logger.Logger) (storage.Storage, error) {
	m := &memstore{
		teams:       make(map[string]storage.Team),
		usersByTeam: make(map[string][]string),
		logger:      logger,
	}

	if err := m.EnsureAdminsTeam(ctx); err != nil {
		return nil, err
	}

	return m, nil
}

// StoreTeam saves a created team to the memstore with the specified teams
func (m *memstore) StoreTeam(_ context.Context,
	id string, name string, projects []string) (storage.Team, error) {

	if _, ok := m.teams[id]; ok {
		return storage.Team{}, storage.ErrConflict
	}

	now := time.Now()
	team := storage.Team{
		ID:        id,
		Name:      name,
		Projects:  projects,
		CreatedAt: now,
		UpdatedAt: now,
	}

	m.teams[id] = team
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
func (m *memstore) GetTeam(ctx context.Context, id string) (storage.Team, error) {
	team, teamFound := m.teams[id]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}

	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	return copyTeam(team), nil
}

// DeleteTeam deletes a team
func (m *memstore) DeleteTeam(ctx context.Context, id string) (storage.Team, error) {
	team, teamFound := m.teams[id]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	// TODO (tc) will need to implement similar logic in pg adapter after that code lands,
	// but the server test should catch it. I'll do that by added a can_delete field to pg
	// table once all that code is in place.
	if !storage.IsTeamDeletable(id) {
		return storage.Team{}, storage.ErrCannotDelete
	}

	delete(m.teams, team.ID)
	// copy not needed since the team is no longer in the store!
	return team, nil
}

// EditTeam edits a team's name and/or projects
func (m *memstore) EditTeam(ctx context.Context,
	id string, name string, projects []string) (storage.Team, error) {

	team, teamFound := m.teams[id]
	if !teamFound {
		return storage.Team{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return storage.Team{}, storage.ErrNotFound
	}

	team.Name = name
	team.Projects = projects
	// persist change
	m.teams[team.ID] = team

	return copyTeam(team), nil
}

// AddUsers updates team membership to include the given list of users.
func (m *memstore) AddUsers(ctx context.Context, id string, userIDs []string) ([]string, error) {
	team, teamFound := m.teams[id]
	if !teamFound {
		return []string{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return []string{}, storage.ErrNotFound
	}

	teamUserIDs, usersFound := m.usersByTeam[id]
	if !usersFound {
		return []string{}, storage.ErrNotFound
	}

	for _, userID := range userIDs {
		if !hasUser(teamUserIDs, userID) {
			teamUserIDs = append(teamUserIDs, userID)
		}
	}

	m.usersByTeam[id] = teamUserIDs

	updatedTeamUsers := m.usersByTeam[id]

	return updatedTeamUsers, nil
}

// RemoveUsers updates team membership by removing all users from a team it can
// find that are currently members.
func (m *memstore) RemoveUsers(ctx context.Context, id string, userIDs []string) ([]string, error) {
	team, ok := m.teams[id]
	if !ok {
		return []string{}, storage.ErrNotFound
	}
	if !projectsIntersect(ctx, team) {
		return []string{}, storage.ErrNotFound
	}

	teamUserIDs, usersFound := m.usersByTeam[id]
	if !usersFound {
		return []string{}, storage.ErrNotFound
	}

	for _, userID := range userIDs {
		teamUserIDs = deleteUser(teamUserIDs, userID)
	}

	m.usersByTeam[id] = teamUserIDs
	updatedTeamUsers := m.usersByTeam[id]

	return updatedTeamUsers, nil
}

// PurgeUserMembership finds all teams the given user is a member of and
// removes the user from their membership, returning the id of all teams modified.
func (m *memstore) PurgeUserMembership(ctx context.Context,
	userID string) (teamsUpdated []string, err error) {
	var foundTeams []string
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
func (m *memstore) GetUserIDsForTeam(ctx context.Context, id string) ([]string, error) {
	teamUserIDs, usersFound := m.usersByTeam[id]
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

func (m *memstore) teamIncludesUser(teamID string, userID string) bool {
	teamUserIDs, usersFound := m.usersByTeam[teamID]
	if !usersFound {
		return false
	}
	return hasUser(teamUserIDs, userID)
}

func (m *memstore) EnsureAdminsTeam(ctx context.Context) error {
	if _, err := m.StoreTeam(ctx, storage.AdminsTeamID, "admins", []string{}); err != nil {
		if err != storage.ErrConflict {
			return err
		}
	}
	return nil
}

func (m *memstore) PurgeProject(ctx context.Context, projectID string) error {
	for _, team := range m.teams {
		for i, v := range team.Projects {
			if v == projectID {
				team.Projects = append(team.Projects[:i], team.Projects[i+1:]...)
				break
			}
		}
		m.teams[team.ID] = team
	}
	return nil
}

// keeps in-memory team safe
func copyTeam(team storage.Team) storage.Team {
	copyTeam := storage.Team{
		ID:       team.ID,
		Name:     team.Name,
		Projects: team.Projects,
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
