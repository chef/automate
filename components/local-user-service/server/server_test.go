package server_test

import (
	"context"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/zap"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	teams_api "github.com/chef/automate/api/interservice/teams/v1"
	"github.com/chef/automate/components/local-user-service/server"
	"github.com/chef/automate/components/local-user-service/users"
	usersMock "github.com/chef/automate/components/local-user-service/users/mock"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
	uuid "github.com/chef/automate/lib/uuid4"
)

func setupUsersFile(t *testing.T, file string) (*server.Server, map[string]users.ShowUser) {
	return setupWithAdapter(t, file, "", "")
}

func setupRolesFile(t *testing.T, file, teamsAddress string) (*server.Server, map[string]users.ShowUser) {
	return setupWithAdapter(t, "", file, teamsAddress)
}

func setupWithAdapter(t *testing.T, usersFile, rolesFile, teamsAddress string) (*server.Server, map[string]users.ShowUser) {
	t.Helper()
	ctx := context.Background()
	serviceCerts := helpers.LoadDevCerts(t, "local-user-service")
	l, err := zap.NewProductionConfig().Build()
	require.NoError(t, err)

	users := map[string]users.ShowUser{}
	config := server.Config{
		ServiceCerts:    serviceCerts,
		Logger:          l,
		Users:           &usersMock.Config{Users: users},
		A1UserData:      usersFile,
		A1UserRolesData: rolesFile,
		TeamsAddress:    teamsAddress,
	}
	serv, err := server.NewServer(ctx, config)
	require.NoError(t, err)
	return serv, users
}

func setupDataFile(t *testing.T, data string) (string, func()) {
	t.Helper()
	tmp, err := ioutil.TempDir(os.TempDir(), "data")
	require.NoError(t, err)
	dataFile := filepath.Join(tmp, "a1_user_data.json")
	require.NoError(t, ioutil.WriteFile(dataFile, []byte(data), 0x755))

	return dataFile, func() { os.RemoveAll(tmp) }
}

// Note: where there's an assert.FileExists in testify, there's no
// FileDoesNotExist. This helper can be used with both assert.True and
// assert.False.
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return !os.IsNotExist(err)
}

func TestA1MigrationFileDoesNotExist(t *testing.T) {
	tmp, err := ioutil.TempDir(os.TempDir(), "nope-does-not-exist")
	require.NoError(t, err)
	nonExistent := filepath.Join(tmp, "a1_user_data.json")
	serv, _ := setupUsersFile(t, nonExistent)
	err = serv.MigrateA1Users(context.Background())
	require.NoError(t, err)
}

func TestA1MigrationFileIsGibberish(t *testing.T) {
	dataFile, cleanup := setupDataFile(t, "gibberish")
	defer cleanup()

	serv, _ := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.Error(t, err)

	assert.True(t, fileExists(dataFile))
}

func TestA1MigrationFileIsMissingHashedPassField(t *testing.T) {
	dataFile, cleanup := setupDataFile(t, `[{"name": "alice"}]`)
	defer cleanup()

	serv, _ := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.Error(t, err)

	assert.True(t, fileExists(dataFile))
}

func TestA1MigrationFileIsMissingNameField(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"}]`)
	defer cleanup()

	serv, _ := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.Error(t, err)

	assert.True(t, fileExists(dataFile))
}

func TestA1MigrationFileIsMinimallyComplete(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"name":"alice", "first_name": null, "last_name": null,
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"}]`)
	defer cleanup()

	serv, adp := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	user, ok := adp["alice"]
	require.True(t, ok)
	assert.Equal(t, "alice", user.Email)
	assert.Equal(t, "alice", user.Name)

	assert.False(t, fileExists(dataFile))
}

func TestA1MigrationWithFirstName(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"name":"alice", "first_name": "Alice", "last_name": null,
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"}]`)
	defer cleanup()

	serv, adp := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	user, ok := adp["alice"]
	require.True(t, ok)
	assert.Equal(t, "alice", user.Email)
	assert.Equal(t, "Alice", user.Name)

	assert.False(t, fileExists(dataFile))
}

func TestA1MigrationWithLastName(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"name":"alice", "first_name": null, "last_name": "Schmidt",
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"}]`)
	defer cleanup()

	serv, adp := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	user, ok := adp["alice"]
	require.True(t, ok)
	assert.Equal(t, "alice", user.Email)
	assert.Equal(t, "Schmidt", user.Name)

	assert.False(t, fileExists(dataFile))
}

func TestA1MigrationWithBothFirstAndLastName(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"name":"alice", "first_name": "Alice", "last_name": "Schmidt",
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"}]`)
	defer cleanup()

	serv, adp := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	user, ok := adp["alice"]
	require.True(t, ok)
	assert.Equal(t, "alice", user.Email)
	assert.Equal(t, "Alice Schmidt", user.Name)

	assert.False(t, fileExists(dataFile))
}

func TestA1MigrationWithTwoUsers(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"name":"alice", "first_name": "Alice", "last_name": "Schmidt",
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"},
{"name":"bob", "first_name": "Bobby", "last_name": "Tables",
"hashed_pass": "$2a$12$dYDC4DZbFKL/WNVJPJvBnOa5MzgEHOt8XbW07x.Bw9mV4Y6qs10Py"}]`)
	defer cleanup()

	serv, adp := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	user, ok := adp["alice"]
	require.True(t, ok)
	assert.Equal(t, "alice", user.Email)
	assert.Equal(t, "Alice Schmidt", user.Name)
	user, ok = adp["bob"]
	require.True(t, ok)
	assert.Equal(t, "bob", user.Email)
	assert.Equal(t, "Bobby Tables", user.Name)

	assert.False(t, fileExists(dataFile))
}

func TestA1MigrationSkipsBuilderUserButMigratesOtherUsers(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"name":"alice", "first_name": "Alice", "last_name": "Schmidt",
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"},
{"name":"builder", "first_name": null, "last_name": null,
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"},
{"name":"bob", "first_name": "Bobby", "last_name": "Tables",
"hashed_pass": "$2a$12$dYDC4DZbFKL/WNVJPJvBnOa5MzgEHOt8XbW07x.Bw9mV4Y6qs10Py"}]`)
	defer cleanup()

	serv, adp := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	user, ok := adp["alice"]
	require.True(t, ok)
	assert.Equal(t, "alice", user.Email)
	assert.Equal(t, "Alice Schmidt", user.Name)
	user, ok = adp["bob"]
	require.True(t, ok)
	assert.Equal(t, "bob", user.Email)
	assert.Equal(t, "Bobby Tables", user.Name)
	user, ok = adp["builder"]
	require.True(t, ok)
	assert.Equal(t, "builder", user.Email)
	assert.Equal(t, "builder", user.Name)

	assert.Equal(t, 3, len(adp), "no other users exist")

	assert.False(t, fileExists(dataFile))
}

func TestA1MigrationOnSuccessMovesFile(t *testing.T) {
	data := `[{"name":"alice", "first_name": null, "last_name": null,
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"}]`
	dataFile, cleanup := setupDataFile(t, data)
	defer cleanup()

	serv, _ := setupUsersFile(t, dataFile)
	err := serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	doneFile := dataFile + ".done"
	f, err := os.Open(doneFile)
	require.NoError(t, err)
	doneData, err := ioutil.ReadAll(f)
	require.NoError(t, err)
	assert.Equal(t, data, string(doneData))

	assert.False(t, fileExists(dataFile))
}

// If an error has happened during the migration, the service would be
// restarted. Since the file is still in place, it would attempt to migrate
// all users again -- but the previously (successfully) migrated users already
// exist. This test assures that the migration logic
// a) skips the existing users, and
// b) keeps going with the rest
func TestA1MigrationAfterFailure(t *testing.T) {
	dataFile, cleanup := setupDataFile(t,
		`[{"name":"alice", "first_name": "Alice", "last_name": "Schmidt",
"hashed_pass": "$2a$12$UBigIaaNCKZqkCKRmRta0.5/zi.X4lzxvXzqJL27iOaD5eslhZx1G"},
{"name":"bob", "first_name": "Bobby", "last_name": "Tables",
"hashed_pass": "$2a$12$dYDC4DZbFKL/WNVJPJvBnOa5MzgEHOt8XbW07x.Bw9mV4Y6qs10Py"}]`)
	defer cleanup()

	serv, adp := setupUsersFile(t, dataFile)

	// Make believe that alice was "already migrated"
	// Note: this could happen if something went wrong adding bob -- the service
	// will have given up, restarted, and would loop over the data file a second
	// time.

	id, err := uuid.NewV4()
	require.NoError(t, err)
	adp["alice"] = users.ShowUser{
		ID:    id.String(),
		Name:  "Alice Schmidt",
		Email: "alice",
	}
	err = serv.MigrateA1Users(context.Background())
	require.NoError(t, err)

	_, ok := adp["alice"]
	require.True(t, ok)
	_, ok = adp["bob"]
	require.True(t, ok)

	assert.Equal(t, 2, len(adp))

	assert.False(t, fileExists(dataFile))
}

func TestA1RoleMigrationFileExistence(t *testing.T) {
	t.Run("when file doesn't exist, does not error", func(t *testing.T) {
		tmp, err := ioutil.TempDir(os.TempDir(), "nope-does-not-exist")
		require.NoError(t, err)
		nonExistent := filepath.Join(tmp, "a1_user_roles_data.json")
		serv, _ := setupRolesFile(t, nonExistent, "")
		require.NoError(t, serv.MigrateA1UserRoles(context.Background()))
	})

	t.Run("when file is gibbery, returns error", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, "gibberish")
		defer cleanup()

		serv, _ := setupRolesFile(t, dataFile, "")
		require.Error(t, serv.MigrateA1UserRoles(context.Background()))
		assert.True(t, fileExists(dataFile))
	})
}

func TestA1RoleMigrationNothingToDo(t *testing.T) {
	t.Run("when user has no roles", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"alice","roles":[]}]`)
		defer cleanup()

		serv, _ := setupRolesFile(t, dataFile, "")
		require.NoError(t, serv.MigrateA1UserRoles(context.Background()))
		assert.False(t, fileExists(dataFile))
	})

	t.Run("when user has no admin role", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"alice","roles":["committer", "shipper"]}]`)
		defer cleanup()

		serv, _ := setupRolesFile(t, dataFile, "")
		require.NoError(t, serv.MigrateA1UserRoles(context.Background()))
		assert.False(t, fileExists(dataFile))
	})
	t.Run("builder user is skipped", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"builder","roles":["admin"]}]`)
		defer cleanup()

		serv, _ := setupRolesFile(t, dataFile, "")
		require.NoError(t, serv.MigrateA1UserRoles(context.Background()))
		assert.False(t, fileExists(dataFile))
	})
}

func TestA1RoleMigrationHappyPath(t *testing.T) {
	t.Run("when admins team exists and user has admin role", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"alice","roles":["admin"]}]`)
		defer cleanup()

		teams, mock := setupTeamsMock(t)
		defer teams.Close()

		adminsTeamID, err := uuid.NewV4()
		require.NoError(t, err)

		// user ID needed when we create in-memory user record below, and to setup
		// the mock's AddUsers handler
		id, err := uuid.NewV4()
		require.NoError(t, err)

		mock.GetTeamByNameFunc = adminsTeam(adminsTeamID)
		mock.AddUsersFunc = func(_ context.Context,
			req *teams_api.AddUsersReq) (*teams_api.AddUsersResp, error) {
			if len(req.UserIds) != 1 || req.UserIds[0] != id.String() {
				return nil, errors.New("unexpected argument")
			}
			return &teams_api.AddUsersResp{Team: &teams_api.Team{Id: adminsTeamID.String()}}, nil
		}

		serv, adp := setupRolesFile(t, dataFile, teams.URL)
		adp["alice"] = users.ShowUser{
			ID:    id.String(),
			Email: "alice",
			Name:  "Alice Schmidt",
		}

		require.NoError(t, serv.MigrateA1UserRoles(context.Background()))
		assert.False(t, fileExists(dataFile))
	})

	t.Run("when admins team exists and multiple users have admin role (among others)", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t,
			`[{"name":"admin","roles":["admin"]},
{"name":"alice","roles":["admin", "committer"]},
{"name":"bob","roles":["shipper", "committer"]},
{"name":"cathy","roles":["shipper", "admin"]}]`)
		defer cleanup()

		teams, mock := setupTeamsMock(t)
		defer teams.Close()

		adminsTeamID, err := uuid.NewV4()
		require.NoError(t, err)

		const (
			admin = iota
			alice
			bob
			cathy
		)

		// user IDs needed when we create in-memory user record below, and to setup
		// the mock's AddUsers handler
		ids := make([]string, 4)
		for i := range ids {
			id, err := uuid.NewV4()
			require.NoError(t, err)
			ids[i] = id.String()
		}

		mock.GetTeamByNameFunc = adminsTeam(adminsTeamID)
		mock.AddUsersFunc = func(_ context.Context,
			req *teams_api.AddUsersReq) (*teams_api.AddUsersResp, error) {

			set := map[string]bool{}
			for _, id := range req.UserIds {
				set[id] = true
			}
			// we want admin, alice, and cathy, but NOT bob
			if !set[ids[admin]] || !set[ids[alice]] || !set[ids[cathy]] || set[ids[bob]] {
				return nil, errors.New("unexpected arguments")
			}
			return &teams_api.AddUsersResp{Team: &teams_api.Team{Id: adminsTeamID.String()}}, nil
		}

		serv, adp := setupRolesFile(t, dataFile, teams.URL)
		adp["admin"] = users.ShowUser{
			ID:    ids[admin],
			Email: "admin",
			Name:  "Local Admin User",
		}
		adp["alice"] = users.ShowUser{
			ID:    ids[alice],
			Email: "alice",
			Name:  "Alice Schmidt",
		}
		adp["bob"] = users.ShowUser{
			ID:    ids[bob],
			Email: "bob",
			Name:  "Bobby Tables",
		}
		adp["cathy"] = users.ShowUser{
			ID:    ids[cathy],
			Email: "cathy",
			Name:  "Cathy",
		}

		require.NoError(t, serv.MigrateA1UserRoles(context.Background()))
		assert.False(t, fileExists(dataFile))
	})

	t.Run("when admins team does not exist and user has admin role", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"alice","roles":["admin"]}]`)
		defer cleanup()

		teams, mock := setupTeamsMock(t)
		defer teams.Close()

		adminsTeamID, err := uuid.NewV4()
		require.NoError(t, err)

		// user ID needed when we create in-memory user record below, and to setup
		// the mock's AddUsers handler
		id, err := uuid.NewV4()
		require.NoError(t, err)

		mock.GetTeamByNameFunc = adminsTeamNotFound
		mock.CreateTeamFunc = createAdminsTeam(adminsTeamID)
		mock.AddUsersFunc = func(_ context.Context,
			req *teams_api.AddUsersReq) (*teams_api.AddUsersResp, error) {
			if len(req.UserIds) != 1 || req.UserIds[0] != id.String() {
				return nil, errors.New("unexpected argument")
			}
			return &teams_api.AddUsersResp{Team: &teams_api.Team{Id: adminsTeamID.String()}}, nil
		}

		serv, adp := setupRolesFile(t, dataFile, teams.URL)
		adp["alice"] = users.ShowUser{
			ID:    id.String(),
			Email: "alice",
			Name:  "Alice Schmidt",
		}

		require.NoError(t, serv.MigrateA1UserRoles(context.Background()))
		assert.False(t, fileExists(dataFile))
	})
}

func TestA1RoleMigrationTeamsAPIFailures(t *testing.T) {
	// Note: These tests don't setup the users (mock) store of l-u-s, since we
	// happen to know that the users won't be looked up if the team query/creation
	// fails. If this fact changes, a similar setup as in the last subtest of this
	// set has to be added in the other tests.
	t.Run("when admins team lookup fails", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"alice","roles":["admin"]}]`)
		defer cleanup()

		teams, mock := setupTeamsMock(t)
		defer teams.Close()

		mock.GetTeamByNameFunc = func(context.Context,
			*teams_api.GetTeamByNameReq) (*teams_api.GetTeamByNameResp, error) {
			return nil, errors.New("unknown failure")
		}

		serv, _ := setupRolesFile(t, dataFile, teams.URL)
		assert.Error(t, serv.MigrateA1UserRoles(context.Background()))
		assert.True(t, fileExists(dataFile))
	})

	t.Run("when admins team creation fails", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"alice","roles":["admin"]}]`)
		defer cleanup()

		teams, mock := setupTeamsMock(t)
		defer teams.Close()

		// a NotFound here triggers the creation of the admins team
		mock.GetTeamByNameFunc = adminsTeamNotFound
		mock.CreateTeamFunc = func(context.Context,
			*teams_api.CreateTeamReq) (*teams_api.CreateTeamResp, error) {
			return nil, errors.New("unknown failure")
		}

		serv, _ := setupRolesFile(t, dataFile, teams.URL)
		assert.Error(t, serv.MigrateA1UserRoles(context.Background()))
		assert.True(t, fileExists(dataFile))
	})

	t.Run("when adding the user to the admins team fails", func(t *testing.T) {
		dataFile, cleanup := setupDataFile(t, `[{"name":"alice","roles":["admin"]}]`)
		defer cleanup()

		teams, mock := setupTeamsMock(t)
		defer teams.Close()

		adminsTeamID, err := uuid.NewV4()
		require.NoError(t, err)

		// a NotFound here triggers the creation of the admins team
		mock.GetTeamByNameFunc = adminsTeamNotFound
		mock.CreateTeamFunc = func(_ context.Context,
			req *teams_api.CreateTeamReq) (*teams_api.CreateTeamResp, error) {
			if req.Name != "admins" {
				return nil, errors.New("unexpected argument")
			}
			return &teams_api.CreateTeamResp{
				Team: &teams_api.Team{
					Id:   adminsTeamID.String(),
					Name: "admins",
				}}, nil
		}

		mock.AddUsersFunc = func(context.Context,
			*teams_api.AddUsersReq) (*teams_api.AddUsersResp, error) {
			return nil, errors.New("unknown failure")
		}

		id, err := uuid.NewV4()
		require.NoError(t, err)

		serv, adp := setupRolesFile(t, dataFile, teams.URL)
		adp["alice"] = users.ShowUser{
			ID:    id.String(),
			Email: "alice",
			Name:  "Alice Schmidt",
		}
		assert.Error(t, serv.MigrateA1UserRoles(context.Background()))
		assert.True(t, fileExists(dataFile))
	})
}

func setupTeamsMock(t *testing.T) (*grpctest.Server, *teams_api.TeamsV1ServerMock) {
	serviceCertsTS := helpers.LoadDevCerts(t, "teams-service")
	mockTeams := teams_api.NewTeamsV1ServerMock()
	connFactoryTS := secureconn.NewFactory(*serviceCertsTS)
	gTS := connFactoryTS.NewServer()
	teams_api.RegisterTeamsV1Server(gTS, mockTeams)
	teams := grpctest.NewServer(gTS)
	return teams, mockTeams
}

func adminsTeamNotFound(context.Context, *teams_api.GetTeamByNameReq) (*teams_api.GetTeamByNameResp, error) {
	return nil, status.Error(codes.NotFound, "team not found")
}

func adminsTeam(id uuid.UUID) func(context.Context,
	*teams_api.GetTeamByNameReq) (*teams_api.GetTeamByNameResp, error) {
	return func(_ context.Context,
		req *teams_api.GetTeamByNameReq) (*teams_api.GetTeamByNameResp, error) {
		if req.Name != "admins" {
			return nil, errors.New("unexpected argument")
		}
		return &teams_api.GetTeamByNameResp{Team: &teams_api.Team{Id: id.String()}}, nil
	}
}

func createAdminsTeam(id uuid.UUID) func(context.Context,
	*teams_api.CreateTeamReq) (*teams_api.CreateTeamResp, error) {
	return func(_ context.Context, req *teams_api.CreateTeamReq) (*teams_api.CreateTeamResp, error) {
		if req.Name != "admins" {
			return nil, errors.New("unexpected argument")
		}
		return &teams_api.CreateTeamResp{
			Team: &teams_api.Team{
				Id:   id.String(),
				Name: "admins",
			}}, nil
	}
}
