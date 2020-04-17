package conformance

import (
	"context"
	"os"
	"reflect"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/memstore"
	"github.com/chef/automate/components/teams-service/storage/postgres"
	"github.com/chef/automate/components/teams-service/test"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/test/helpers"
)

type adapterTestFunc func(context.Context, *testing.T, storage.Storage)

// TestStorage tests the memstore storage adapter, via its implemented
// storage.Storage interface. As soon as we have a second adapter, the tests
// should loop through both of those adapters for each test against the
// storage.Storage interface.
func TestStorage(t *testing.T) {
	ctx := context.Background()
	testFuncs := []adapterTestFunc{
		testStoreTeam,
		testStoreTeamConflict,
		testGetTeam,
		testGetTeamNotFound,
		testGetTeamImmutable,
		testGetTeams,
		testDeleteTeam,
		testDeleteTeamNotFound,
		testEditTeam,
		testEditTeamNotFound,
		testEditTeamImmutable,
		testAddUsers,
		testAddUsersNotFound,
		testRemoveUsers,
		testRemoveUsersNotFound,
		testPurgeUserMembership,
		testGetTeamsForUser,
		testGetTeamByName,
		testGetTeamByNameNotFound,
		testPurgeProjectUnassigned,
		testPurgeProjectOnlyProjectToPurge,
		testPurgeProjectOtherProjectsExcludingOneToPurge,
		testPurgeProjectOtherProjectsIncludingOneToPurge,
		testPurgeProjectUniversal,
	}

	// lazy way to randomize tests
	tests := map[int]adapterTestFunc{}
	for i, tc := range testFuncs {
		tests[i] = tc
	}

	// Note: to set up PG locally for running these tests, use
	//     docker run --name teams-postgres -e POSTGRES_USER=postgres -e POSTGRES_DB=teams_test -p 5432:5432 -d postgres:9
	adapters := map[string]storage.Storage{}

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "init logger for postgres storage: %s", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../postgres/migration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	if migrationConfig == nil {
		mem, err := memstore.New(ctx, l)
		require.NoError(t, err)
		adapters["memstore"] = mem
	} else {
		authzCerts := helpers.LoadDevCerts(t, "authz-service")
		authzConnFactory := secureconn.NewFactory(*authzCerts)
		grpcAuthz := authzConnFactory.NewServer()

		mockV2Authz := authz_v2.NewAuthorizationServerMock()
		mockV2Authz.ValidateProjectAssignmentFunc = defaultValidateProjectAssignmentFunc
		authz_v2.RegisterAuthorizationServer(grpcAuthz, mockV2Authz)

		authzServer := grpctest.NewServer(grpcAuthz)
		authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
		require.NoError(t, err)

		authzV2AuthorizationClient := authz_v2.NewAuthorizationClient(authzConn)

		adp, err := postgres.New(l, *migrationConfig, true, authzV2AuthorizationClient)
		require.NoError(t, err)
		adapters["postgres"] = adp

		// If ciMode, run in-memory AND PG
		// else just run PG.
		if os.Getenv("CI") == "true" {
			mem, err := memstore.New(ctx, l)
			require.NoError(t, err)
			adapters["memstore"] = mem
		}
	}

	for name, adp := range adapters {
		t.Run(name, func(t *testing.T) {
			for _, test := range tests {
				// use the function name to identify the test case
				name := strings.Split(runtime.FuncForPC(reflect.ValueOf(test).Pointer()).Name(), ".")[2]
				t.Run(name, func(t *testing.T) {
					r, ok := adp.(storage.Resetter)
					require.True(t, ok)
					if err := r.Reset(ctx); err != nil {
						t.Fatalf("reset adapter: %s", err)
					}

					test(ctx, t, adp)
				})
			}
		})
	}
}

func testGetTeamByName(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	team, err := s.StoreTeam(ctx, id, "team name", []string{})
	require.NoError(t, err, "setup: failed to store team")

	users := []string{"one", "two", "three"}
	team, err = s.AddUsers(ctx, id, users)
	require.NoError(t, err, "setup: failed to add users to test team")

	usersOnTeam, err := s.GetUserIDsForTeam(ctx, id)
	require.Equal(t, len(users), len(usersOnTeam), "setup: user list size mismatch")

	getTeam, err := s.GetTeam(ctx, id)
	require.NoError(t, err)
	// reset timestamps for comparison (TODO)
	team.UpdatedAt = time.Time{}
	getTeam.UpdatedAt = time.Time{}
	team.CreatedAt = time.Time{}
	getTeam.CreatedAt = time.Time{}
	assert.Equal(t, team, getTeam)
}

func testGetTeamByNameNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "foobear"
	_, err := s.GetTeam(ctx, id)
	assert.Equal(t, storage.ErrNotFound, err)
}

func testGetTeamsForUser(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "team-1-with-user"
	team, err := s.StoreTeam(ctx, id, id+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team-1-with-user")

	id2 := "team-2-with-user"
	team2, err := s.StoreTeam(ctx, id2, id2+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team-2-with-user")

	idWithout := "team-without-user"
	_, err = s.StoreTeam(ctx, idWithout, idWithout+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team-with-user")

	initialUsers := []string{"user-for-team-fetch", "two", "three"}

	team, err = s.AddUsers(ctx, id, initialUsers)
	require.NoError(t, err, "setup: failed to add users to team-1-with-user")
	team2, err = s.AddUsers(ctx, id2, initialUsers)
	require.NoError(t, err, "setup: failed to add users to team-2-with-user")
	_, err = s.AddUsers(ctx, idWithout, []string{"two", "three"})
	require.NoError(t, err, "setup: failed to add users to test team-without-user")

	returnedTeams, err := s.GetTeamsForUser(ctx, "user-for-team-fetch")
	require.NoError(t, err)
	expectedFetchedTeams := []storage.Team{team, team2}
	ids := []string{returnedTeams[0].ID, returnedTeams[1].ID}
	expectedIDs := []string{expectedFetchedTeams[0].ID, expectedFetchedTeams[1].ID}

	assert.ElementsMatch(t, ids, expectedIDs) // Comparing the objects wholly will fail due to updated_at :(
}

func testPurgeUserMembership(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "team-1-with-user"
	team, err := s.StoreTeam(ctx, id, id+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team-1-with-user")

	id2 := "team-2-with-user"
	team2, err := s.StoreTeam(ctx, id2, id2+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team-2-with-user")

	idWithout := "team-without-user"
	teamWithout, err := s.StoreTeam(ctx, idWithout, idWithout+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team-with-user")

	initialUsers := []string{"user-id-to-purge", "two", "three"}

	team, err = s.AddUsers(ctx, id, initialUsers)
	require.NoError(t, err, "setup: failed to add users to team-1-with-user")
	team2, err = s.AddUsers(ctx, id2, initialUsers)
	require.NoError(t, err, "setup: failed to add users to team-2-with-user")
	teamWithout, err = s.AddUsers(ctx, idWithout, []string{"two", "three"})
	require.NoError(t, err, "setup: failed to add users to test team-without-user")

	updatedTeams, err := s.PurgeUserMembership(ctx, "user-id-to-purge")
	require.NoError(t, err, "purge failed")

	team, err = s.GetTeam(ctx, team.ID)
	require.NoError(t, err, "setup: failed to fetch team-1-with-user")
	team2, err = s.GetTeam(ctx, team2.ID)
	require.NoError(t, err, "setup: failed to fetch team-12with-user")
	teamWithout, err = s.GetTeam(ctx, idWithout)
	require.NoError(t, err, "setup: failed to fetch test team-without-user")

	expectedChangedTeams := []storage.Team{team, team2}

	for _, tm := range expectedChangedTeams {
		assert.Contains(t, updatedTeams, tm.ID, "team %q should have been updated", tm)
		userIDs, err := s.GetUserIDsForTeam(ctx, tm.ID)
		require.NoError(t, err)
		for _, user := range []string{"two", "three"} {
			assert.Contains(t, userIDs, user, "user %q should not have been removed", user)
		}
		assert.NotContains(t, userIDs, "user-id-to-purge", "user %q should have been removed", "user-id-to-purge")
	}

	assert.NotContains(t, updatedTeams, idWithout, "team %q should not have been updated", teamWithout)
	userIDs, err := s.GetUserIDsForTeam(ctx, idWithout)
	require.NoError(t, err)
	assert.Equal(t, 2, len(userIDs), "number of users removed does not match")
	for _, user := range []string{"two", "three"} {
		assert.Contains(t, userIDs, user, "user %q should not have been removed", user)
	}
	assert.NotContains(t, userIDs, "user-id-to-purge", "user %q should not be present", "user-id-to-purge")
}

func testDeleteTeamNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.DeleteTeam(ctx, "not-found")
	assert.Error(t, err, "delete nonexistent team, expected error")
	assert.Equal(t, storage.ErrNotFound, err)
}

func testDeleteTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	team, err := s.StoreTeam(ctx, id, id+" Name", []string{})
	require.NoError(t, err, "failed to create test team, as setup for testing delete")
	require.NotNil(t, team)

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "setup: failed to read back teams")
	require.Equal(t, len(storage.NonDeletableTeams)+1, len(teams))

	_, err = s.DeleteTeam(ctx, id)
	require.NoError(t, err)

	teams, err = s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	require.Equal(t, len(storage.NonDeletableTeams), len(teams))
}

func testEditTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	team, err := s.StoreTeam(ctx, id, id+" Name", []string{})
	require.NoError(t, err, "failed to create 'test-team', as setup for testing edit")

	newName := "new name"
	newProjects := []string{"new-project"}
	team, err = s.EditTeam(ctx, id, newName, newProjects)
	require.NoError(t, err, "failed to edit the name of existing team with ID %v", id)
	assert.Equal(t, newName, team.Name,
		"edit of team ID %s returned success, but the name is incorrect", id)
	assert.Equal(t, newProjects, team.Projects,
		"edit of team ID %s returned success, but the projects are incorrect", id)

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	require.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))

	result, err := s.GetTeam(ctx, id)
	assert.Equal(t, newName, result.Name)
	assert.Equal(t, newProjects, result.Projects)
}

func testEditTeamImmutable(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	name := "test team"
	team, err := s.StoreTeam(ctx, id, name, []string{})
	require.NoError(t, err, "setup: failed to store team")

	newName := "new name"
	team, err = s.EditTeam(ctx, id, newName, []string{})
	require.NoError(t, err, "setup: failed to edit team")
	require.Equal(t, newName, team.Name, "setup: team name change failed")

	users := []string{"one", "two", "three"}
	_, err = s.AddUsers(ctx, team.ID, users)
	require.NoError(t, err, "setup: failed to add users to test team")

	result, err := s.GetTeam(ctx, id)
	require.NoError(t, err, "setup: failed to read back teams")

	userIDs, err := s.GetUserIDsForTeam(ctx, result.ID)
	require.NoError(t, err)
	require.Equal(t, len(users), len(userIDs))
}

func testEditTeamNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.EditTeam(ctx, "not-found", "new name", []string{})
	assert.Equal(t, storage.ErrNotFound, err)
}

func testAddUsers(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	team, err := s.StoreTeam(ctx, id, id+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team")

	users := []string{"one", "two", "three"}
	team, err = s.AddUsers(ctx, id, users)
	assert.NotNil(t, team)

	userIDs, err := s.GetUserIDsForTeam(ctx, id)
	require.NoError(t, err)

	assert.Equal(t, len(users), len(userIDs), "number of users added does not match")
	for _, user := range users {
		assert.Contains(t, userIDs, user, "user %q not added", user)
	}

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))

	_, err = s.GetTeam(ctx, id)
	require.NoError(t, err, "failed to read back team")

	userIDs, err = s.GetUserIDsForTeam(ctx, id)
	require.NoError(t, err)
	assert.ElementsMatch(t, users, userIDs)
}

func testAddUsersNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.AddUsers(ctx, "not-found", []string{"user-1"})
	assert.Equal(t, storage.ErrNotFound, err)
}

func testRemoveUsers(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	_, err := s.StoreTeam(ctx, id, id+" Name", []string{})
	require.NoError(t, err, "setup: failed to create test team")

	one, two, three := "one", "two", "three"
	four, five := "four", "five"
	initialUsers := []string{one, two, three, four, five}

	_, err = s.AddUsers(ctx, id, initialUsers)
	require.NoError(t, err, "setup: failed to add users to test team")

	targetUsers := []string{two, four, five}
	_, err = s.RemoveUsers(ctx, id, targetUsers)

	userIDs, err := s.GetUserIDsForTeam(ctx, id)
	require.NoError(t, err)
	assert.Equal(t, 2, len(userIDs), "number of users removed does not match")
	for _, user := range []string{one, three} {
		assert.Contains(t, userIDs, user, "user %q should not have been removed", user)
	}
	for _, user := range targetUsers {
		assert.NotContains(t, userIDs, user, "user %q should have been removed", user)
	}

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))

	_, err = s.GetTeam(ctx, id)
	require.NoError(t, err, "failed to read back team")

	userIDs, err = s.GetUserIDsForTeam(ctx, id)
	require.NoError(t, err)
	assert.ElementsMatch(t, []string{one, three}, userIDs)
}

func testRemoveUsersNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.RemoveUsers(ctx, "not-found", []string{"user-1"})
	assert.Equal(t, storage.ErrNotFound, err)
}

func testGetTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	team, err := s.StoreTeam(ctx, "id", "name", []string{})
	require.NoError(t, err, "setup: failed to store team")
	one, two, three := "one", "two", "three"
	users := []string{one, two, three}
	team, err = s.AddUsers(ctx, team.ID, users)
	require.NoError(t, err, "setup: failed to add users to test team")
	userIDs, err := s.GetUserIDsForTeam(ctx, team.ID)
	require.Equal(t, len(users), len(userIDs), "setup: user list size mismatch")

	getTeam, err := s.GetTeam(ctx, team.ID)
	getTeamUserIDs, err := s.GetUserIDsForTeam(ctx, getTeam.ID)

	require.NoError(t, err)
	// Updated_at has changed because of the added users...
	assert.WithinDuration(t, team.CreatedAt, getTeam.CreatedAt, 50*time.Millisecond)
	assert.Equal(t, team.Name, getTeam.Name)
	assert.Equal(t, team.ID, getTeam.ID)
	assert.Equal(t, userIDs, getTeamUserIDs)
}

func testGetTeamNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.GetTeam(ctx, "not-found")
	assert.Equal(t, storage.ErrNotFound, err)
}

func testGetTeamImmutable(ctx context.Context, t *testing.T, s storage.Storage) {
	team, err := s.StoreTeam(ctx, "id", "name", []string{})
	require.NoError(t, err, "setup: failed to store team")
	one, two, three := "one", "two", "three"
	users := []string{one, two, three}
	team, err = s.AddUsers(ctx, team.ID, users)
	require.NoError(t, err, "setup: failed to add users to test team")
	userIDs, err := s.GetUserIDsForTeam(ctx, team.ID)
	require.NoError(t, err)
	require.Equal(t, len(users), len(userIDs), "setup: user list size mismatch")
}

func testGetTeams(ctx context.Context, t *testing.T, s storage.Storage) {
	tl, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to get teams")
	assert.Equal(t, len(storage.NonDeletableTeams), len(tl), "GetTeams called with no teams stored")

	id := "test-team"
	name := "Test Team Name"
	_, err = s.StoreTeam(ctx, id, name, []string{})
	require.NoError(t, err, "failed to create test team, as setup for testing edit")

	tl, err = s.GetTeams(ctx)
	require.NoError(t, err, "failed to get teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(tl), "GetTeams called with one team stored")
}

func testStoreTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	name := "Test Team Name"
	team, err := s.StoreTeam(ctx, id, name, []string{})
	require.NoError(t, err, "failed to store team")
	assert.Equal(t, id, team.ID, "Stored team id mismatch")
	assert.Equal(t, name, team.Name, "Stored team name mismatch")

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))

	result, err := s.GetTeam(ctx, id)
	require.NoError(t, err, "failed to read back team")
	assert.Equal(t, id, result.ID, "Stored team id mismatch")
	assert.Equal(t, name, result.Name, "Stored team name mismatch")
}

func testStoreTeamConflict(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	name := "Test Team Name"
	team, err := s.StoreTeam(ctx, id, name, []string{})
	require.NoError(t, err, "failed to store id")
	assert.Equal(t, id, team.ID, "Stored team id mismatch")
	assert.Equal(t, name, team.Name, "Stored team name mismatch")

	_, err = s.StoreTeam(ctx, id, name, []string{})
	assert.Error(t, err, "Tried to store a new team with an existing team name '%v', should have gotten an error", name)

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))
}

func testPurgeProjectUnassigned(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	name := "Test Team Name"
	projectToPurge := "project-to-purge"
	projects := []string{}
	resp, err := s.StoreTeam(ctx, id, name, projects)
	assert.NoError(t, err, "failed to store team")
	assert.ElementsMatch(t, projects, resp.Projects)

	err = s.PurgeProject(ctx, projectToPurge)
	assert.NoError(t, err, "failed to purge project")

	purgeCheck, err := s.GetTeam(ctx, id)
	assert.NoError(t, err, "failed to get team")
	assert.ElementsMatch(t, []string{}, purgeCheck.Projects)
}

func testPurgeProjectOnlyProjectToPurge(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	name := "Test Team Name"
	projectToPurge := "project-to-purge"
	projects := []string{projectToPurge}
	resp, err := s.StoreTeam(ctx, id, name, projects)
	assert.NoError(t, err, "failed to store team")
	assert.ElementsMatch(t, projects, resp.Projects)

	err = s.PurgeProject(ctx, projectToPurge)
	assert.NoError(t, err, "failed to purge project")

	purgeCheck, err := s.GetTeam(ctx, id)
	assert.NoError(t, err, "failed to get team")
	assert.ElementsMatch(t, []string{}, purgeCheck.Projects)
}

func testPurgeProjectOtherProjectsExcludingOneToPurge(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	name := "Test Team Name"
	projectToPurge := "project-to-purge"
	projects := []string{"otherproject", "otherproject2"}
	resp, err := s.StoreTeam(ctx, id, name, projects)
	assert.NoError(t, err, "failed to store team")
	assert.ElementsMatch(t, projects, resp.Projects)

	err = s.PurgeProject(ctx, projectToPurge)
	assert.NoError(t, err, "failed to purge project")

	purgeCheck, err := s.GetTeam(ctx, id)
	assert.NoError(t, err, "failed to get team")
	assert.ElementsMatch(t, projects, purgeCheck.Projects)
}

func testPurgeProjectOtherProjectsIncludingOneToPurge(ctx context.Context, t *testing.T, s storage.Storage) {
	id := "test-team"
	name := "Test Team Name"
	projectToPurge := "project-to-purge"
	projects := []string{"otherproject", projectToPurge, "otherproject2"}
	resp, err := s.StoreTeam(ctx, id, name, projects)
	assert.NoError(t, err, "failed to store team")
	assert.ElementsMatch(t, projects, resp.Projects)

	err = s.PurgeProject(ctx, projectToPurge)
	assert.NoError(t, err, "failed to purge project")

	purgeCheck, err := s.GetTeam(ctx, id)
	assert.NoError(t, err, "failed to get team")
	assert.ElementsMatch(t, []string{"otherproject", "otherproject2"}, purgeCheck.Projects)
}

func testPurgeProjectUniversal(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team Name"
	projectToPurge := "project-to-purge"

	id1 := "other_projects"
	projects1 := []string{"otherproject", "otherproject2"}
	resp1, err := s.StoreTeam(ctx, id1, name, projects1)
	assert.NoError(t, err, "failed to store team1")
	assert.ElementsMatch(t, projects1, resp1.Projects)

	id2 := "unassigned"
	projects2 := []string{}
	resp2, err := s.StoreTeam(ctx, id2, name, projects2)
	assert.NoError(t, err, "failed to store team2")
	assert.ElementsMatch(t, projects2, resp2.Projects)

	id3 := "project-to-purge_and_others"
	projects3 := []string{"otherproject", projectToPurge, "otherproject2"}
	resp3, err := s.StoreTeam(ctx, id3, name, projects3)
	assert.NoError(t, err, "failed to store team3")
	assert.ElementsMatch(t, projects3, resp3.Projects)

	id4 := "project-to-purge_only"
	projects4 := []string{projectToPurge}
	resp4, err := s.StoreTeam(ctx, id4, name, projects4)
	assert.NoError(t, err, "failed to store team4")
	assert.ElementsMatch(t, projects4, resp4.Projects)

	err = s.PurgeProject(ctx, projectToPurge)
	assert.NoError(t, err, "failed to purge project")

	// unchanged projects
	purgeCheck1, err := s.GetTeam(ctx, id1)
	assert.NoError(t, err)
	assert.ElementsMatch(t, projects1, purgeCheck1.Projects)

	purgeCheck2, err := s.GetTeam(ctx, id2)
	assert.NoError(t, err)
	assert.ElementsMatch(t, projects2, purgeCheck2.Projects)

	// project removed
	purgeCheck3, err := s.GetTeam(ctx, id3)
	assert.NoError(t, err)
	assert.ElementsMatch(t, []string{"otherproject", "otherproject2"}, purgeCheck3.Projects)

	purgeCheck4, err := s.GetTeam(ctx, id4)
	assert.NoError(t, err)
	assert.ElementsMatch(t, []string{}, purgeCheck4.Projects)
}

func defaultValidateProjectAssignmentFunc(context.Context,
	*authz_v2.ValidateProjectAssignmentReq) (*authz_v2.ValidateProjectAssignmentResp, error) {
	return &authz_v2.ValidateProjectAssignmentResp{}, nil
}
