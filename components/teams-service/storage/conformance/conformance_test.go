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

	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/memstore"
	"github.com/chef/automate/components/teams-service/storage/postgres"
	"github.com/chef/automate/components/teams-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/teams-service/test"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
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
		testEditTeamConflict,
		testAddUsers,
		testAddUsersNotFound,
		testRemoveUsers,
		testRemoveUsersNotFound,
		testPurgeUserMembership,
		testGetTeamsForUser,
		testGetTeamByName,
		testGetTeamByNameNotFound,
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

	dataMigrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../../storage/postgres/datamigration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg data config for tests: %s", err.Error())
	}
	var convertedConfig *datamigration.Config
	if dataMigrationConfig != nil {
		casted := datamigration.Config(*dataMigrationConfig)
		convertedConfig = &casted
	} else {
		convertedConfig = nil
	}

	if migrationConfig == nil {
		mem, err := memstore.New(ctx, l)
		require.NoError(t, err)
		adapters["memstore"] = mem
	} else {
		adp, err := postgres.New(l, *migrationConfig, *convertedConfig, true)
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
	teamName := "Test Team"

	team, err := s.StoreTeam(ctx, teamName, "description")
	require.NoError(t, err, "setup: failed to store team")
	users := []string{"one", "two", "three"}
	team, err = s.AddUsers(ctx, team.ID, users)
	require.NoError(t, err, "setup: failed to add users to test team")
	usersOnTeam, err := s.GetUserIDsForTeam(ctx, team.ID)
	require.Equal(t, len(users), len(usersOnTeam), "setup: user list size mismatch")

	getTeam, err := s.GetTeamByName(ctx, teamName)

	require.NoError(t, err)
	// reset timestamps for comparison (TODO)
	team.UpdatedAt = time.Time{}
	getTeam.UpdatedAt = time.Time{}
	team.CreatedAt = time.Time{}
	getTeam.CreatedAt = time.Time{}
	assert.Equal(t, team, getTeam)
}

func testGetTeamByNameNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Foobear"
	_, err := s.GetTeamByName(ctx, name)
	assert.Equal(t, storage.ErrNotFound, err)
}

func testGetTeamsForUser(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Team1 With User"
	team, err := s.StoreTeam(ctx, name, name+" Description")
	require.NoError(t, err, "setup: failed to create test team")

	name2 := "Team 2 With User"
	team2, err := s.StoreTeam(ctx, name2, name2+" Description")
	require.NoError(t, err, "setup: failed to create test team 2")

	nameWithout := "Team Without User"
	teamWithout, err := s.StoreTeam(ctx, nameWithout, nameWithout+" Description")
	require.NoError(t, err, "setup: failed to create test team 2")

	initialUsers := []string{"user-for-team-fetch", "two", "three"}

	team, err = s.AddUsers(ctx, team.ID, initialUsers)
	require.NoError(t, err, "setup: failed to add users to test team 1")
	team2, err = s.AddUsers(ctx, team2.ID, initialUsers)
	require.NoError(t, err, "setup: failed to add users to test team 2")
	teamWithout, err = s.AddUsers(ctx, teamWithout.ID, []string{"two", "three"})
	require.NoError(t, err, "setup: failed to add users to test team without user")

	returnedTeams, err := s.GetTeamsForUser(ctx, "user-for-team-fetch")
	require.NoError(t, err)
	expectedFetchedTeams := []storage.Team{team, team2}
	ids := []uuid.UUID{returnedTeams[0].ID, returnedTeams[1].ID}
	expectedIDs := []uuid.UUID{expectedFetchedTeams[0].ID, expectedFetchedTeams[1].ID}

	assert.ElementsMatch(t, ids, expectedIDs) // Comparing the objects wholly will fail due to updated_at :(
}

func testPurgeUserMembership(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Team1 With User"
	team, err := s.StoreTeam(ctx, name, name+" Description")
	require.NoError(t, err, "setup: failed to create test team")

	name2 := "Team 2 With User"
	team2, err := s.StoreTeam(ctx, name2, name2+" Description")
	require.NoError(t, err, "setup: failed to create test team 2")

	nameWithout := "Team Without User"
	teamWithout, err := s.StoreTeam(ctx, nameWithout, nameWithout+" Description")
	require.NoError(t, err, "setup: failed to create test team 2")

	initialUsers := []string{"user-id-to-purge", "two", "three"}

	team, err = s.AddUsers(ctx, team.ID, initialUsers)
	require.NoError(t, err, "setup: failed to add users to test team 1")
	team2, err = s.AddUsers(ctx, team2.ID, initialUsers)
	require.NoError(t, err, "setup: failed to add users to test team 2")
	teamWithout, err = s.AddUsers(ctx, teamWithout.ID, []string{"two", "three"})
	require.NoError(t, err, "setup: failed to add users to test team without user")

	updatedTeams, err := s.PurgeUserMembership(ctx, "user-id-to-purge")
	require.NoError(t, err, "purge failed")

	team, err = s.GetTeam(ctx, team.ID)
	require.NoError(t, err, "setup: failed to fetch test team 1")
	team2, err = s.GetTeam(ctx, team2.ID)
	require.NoError(t, err, "setup: failed to fetch test team 2")
	teamWithout, err = s.GetTeam(ctx, teamWithout.ID)
	require.NoError(t, err, "setup: failed to fetch test team without user")

	expectedChangedTeams := []storage.Team{team, team2}

	for _, tm := range expectedChangedTeams {
		assert.Contains(t, updatedTeams, tm.ID, "team %q should have been updated", tm)
		userIDs, err := s.GetUserIDsForTeam(ctx, team.ID)
		require.NoError(t, err)
		for _, user := range []string{"two", "three"} {
			assert.Contains(t, userIDs, user, "user %q should not have been removed", user)
		}
		assert.NotContains(t, userIDs, "user-id-to-purge", "user %q should have been removed", "user-id-to-purge")
	}

	assert.NotContains(t, updatedTeams, teamWithout.ID, "team %q should not have been updated", teamWithout)
	userIDs, err := s.GetUserIDsForTeam(ctx, teamWithout.ID)
	require.NoError(t, err)
	assert.Equal(t, 2, len(userIDs), "number of users removed does not match")
	for _, user := range []string{"two", "three"} {
		assert.Contains(t, userIDs, user, "user %q should not have been removed", user)
	}
	assert.NotContains(t, userIDs, "user-id-to-purge", "user %q should not be present", "user-id-to-purge")
}

func testDeleteTeamNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.DeleteTeam(ctx, uuid.Must(uuid.NewV4()))
	assert.Error(t, err, "delete nonexistent team, expected error")
	assert.Equal(t, storage.ErrNotFound, err)
}

func testDeleteTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team"
	team, err := s.StoreTeam(ctx, name, name+" Description")
	require.NoError(t, err, "failed to create test team, as setup for testing delete")
	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "setup: failed to read back teams")
	require.Equal(t, len(storage.NonDeletableTeams)+1, len(teams))

	_, err = s.DeleteTeam(ctx, team.ID)
	require.NoError(t, err)

	teams, err = s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	require.Equal(t, len(storage.NonDeletableTeams), len(teams))
}

func testEditTeamConflict(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team"
	descr := name + " Descr"
	_, err := s.StoreTeam(ctx, name, descr)
	require.NoError(t, err, "failed to create test team, as setup for testing edit")

	otherName := "Other Name"
	otherDescr := otherName + " Descr"
	sameNameTeam, err := s.StoreTeam(ctx, otherName, otherDescr)
	require.NoError(t, err, "failed to create 'other' test team, as setup for testing edit")

	sameNameTeam.Name = name
	_, err = s.EditTeam(ctx, sameNameTeam)
	assert.Equal(t, storage.ErrConflict, err,
		"tried to change a team's name to another existing team's name, should have failed but didn't.")

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	require.Equal(t, 2+len(storage.NonDeletableTeams), len(teams))

	result, err := s.GetTeamByName(ctx, name)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, name, result.Name)
	assert.Equal(t, descr, result.Description)

	result, err = s.GetTeamByName(ctx, otherName)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, otherName, result.Name)
	assert.Equal(t, otherDescr, result.Description)
}

func testEditTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team"
	team, err := s.StoreTeam(ctx, name, name+" Description")
	require.NoError(t, err, "failed to create test team, as setup for testing edit")

	newName := "New name"
	newDescription := "New description"
	team.Name = newName
	team.Description = newDescription
	team, err = s.EditTeam(ctx, team)
	require.NoError(t, err, "failed to edit the name of existing team with ID %v", team.ID)
	assert.Equal(t, newName, team.Name,
		"edit of team ID %s returned success, but the name is incorrect", team.ID)
	assert.Equal(t, newDescription, team.Description,
		"edit of team ID %s returned success, but the description is incorrect", team.ID)

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	require.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))

	result, err := s.GetTeamByName(ctx, newName)
	assert.Equal(t, newName, result.Name)
	assert.Equal(t, newDescription, result.Description)
}

func testEditTeamImmutable(ctx context.Context, t *testing.T, s storage.Storage) {
	team, err := s.StoreTeam(ctx, "name", "description")
	require.NoError(t, err, "setup: failed to store team")
	team.Name = "new name"
	team, err = s.EditTeam(ctx, team)
	require.NoError(t, err, "setup: failed to edit team")
	require.Equal(t, "new name", team.Name, "setup: team name change failed")
	users := []string{"one", "two", "three"}
	_, err = s.AddUsers(ctx, team.ID, users)
	require.NoError(t, err, "setup: failed to add users to test team")
	result, err := s.GetTeamByName(ctx, "new name")
	require.NoError(t, err, "setup: failed to read back teams")
	userIDs, err := s.GetUserIDsForTeam(ctx, result.ID)
	require.NoError(t, err)
	require.Equal(t, len(users), len(userIDs))
}

func testEditTeamNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	team := storage.Team{
		ID: uuid.Must(uuid.NewV4()),
	}
	_, err := s.EditTeam(ctx, team)
	assert.Equal(t, storage.ErrNotFound, err)
}

func testAddUsers(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team"
	team, err := s.StoreTeam(ctx, name, name+" Description")
	require.NoError(t, err, "setup: failed to create test team")

	users := []string{"one", "two", "three"}
	newTeam, err := s.AddUsers(ctx, team.ID, users)
	assert.NotNil(t, newTeam)
	userIDs, err := s.GetUserIDsForTeam(ctx, newTeam.ID)
	require.NoError(t, err)

	assert.Equal(t, len(users), len(userIDs), "number of users added does not match")
	for _, user := range users {
		assert.Contains(t, userIDs, user, "user %q not added", user)
	}

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))
	result, err := s.GetTeamByName(ctx, name)
	require.NoError(t, err, "failed to read back team")
	userIDs, err = s.GetUserIDsForTeam(ctx, result.ID)
	require.NoError(t, err)
	assert.ElementsMatch(t, users, userIDs)
}

func testAddUsersNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.AddUsers(ctx, uuid.Must(uuid.NewV4()), []string{"user-1"})
	assert.Equal(t, storage.ErrNotFound, err)
}

func testRemoveUsers(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team"
	team, err := s.StoreTeam(ctx, name, name+" Description")
	require.NoError(t, err, "setup: failed to create test team")

	one, two, three := "one", "two", "three"
	four, five := "four", "five"
	initialUsers := []string{one, two, three, four, five}

	team, err = s.AddUsers(ctx, team.ID, initialUsers)
	require.NoError(t, err, "setup: failed to add users to test team")

	targetUsers := []string{two, four, five}
	newTeam, err := s.RemoveUsers(ctx, team.ID, targetUsers)

	userIDs, err := s.GetUserIDsForTeam(ctx, newTeam.ID)
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

	result, err := s.GetTeamByName(ctx, name)
	require.NoError(t, err, "failed to read back team")
	userIDs, err = s.GetUserIDsForTeam(ctx, result.ID)
	require.NoError(t, err)
	assert.ElementsMatch(t, []string{one, three}, userIDs)
}

func testRemoveUsersNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	_, err := s.RemoveUsers(ctx, uuid.Must(uuid.NewV4()), []string{"user-1"})
	assert.Equal(t, storage.ErrNotFound, err)
}

func testGetTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	team, err := s.StoreTeam(ctx, "name", "description")
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
	assert.Equal(t, team.Description, getTeam.Description)
	assert.Equal(t, team.ID, getTeam.ID)
	assert.Equal(t, userIDs, getTeamUserIDs)
}

func testGetTeamNotFound(ctx context.Context, t *testing.T, s storage.Storage) {
	id := uuid.Must(uuid.NewV4())
	_, err := s.GetTeam(ctx, id)
	assert.Equal(t, storage.ErrNotFound, err)
}

func testGetTeamImmutable(ctx context.Context, t *testing.T, s storage.Storage) {
	team, err := s.StoreTeam(ctx, "name", "description")
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

	name := "Test Team"
	description := "Test Team Description"
	_, err = s.StoreTeam(ctx, name, description)
	require.NoError(t, err, "failed to create test team, as setup for testing edit")

	tl, err = s.GetTeams(ctx)
	require.NoError(t, err, "failed to get teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(tl), "GetTeams called with one team stored")
}

func testStoreTeam(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team"
	description := "Test Team Description"
	team, err := s.StoreTeam(ctx, name, description)
	require.NoError(t, err, "failed to store team")
	assert.Equal(t, name, team.Name, "Stored team name mismatch")
	assert.Equal(t, description, team.Description, "Stored team description mismatch")

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))

	result, err := s.GetTeamByName(ctx, name)
	require.NoError(t, err, "failed to read back team")
	assert.Equal(t, name, result.Name, "Stored team name mismatch")
	assert.Equal(t, description, result.Description, "Stored team description mismatch")
}

func testStoreTeamConflict(ctx context.Context, t *testing.T, s storage.Storage) {
	name := "Test Team"
	description := "Test Team Description"
	team, err := s.StoreTeam(ctx, name, description)
	require.NoError(t, err, "failed to store team")
	assert.Equal(t, name, team.Name, "Stored team name mismatch")
	assert.Equal(t, description, team.Description, "Stored team description mismatch")

	_, err = s.StoreTeam(ctx, name, description)
	assert.Error(t, err, "Tried to store a new team with an existing team name '%v', should have gotten an error", name)

	teams, err := s.GetTeams(ctx)
	require.NoError(t, err, "failed to read back teams")
	assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teams))
}
