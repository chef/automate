package server_test

import (
	"context"
	"errors"
	"os"
	"testing"

	version_api "github.com/chef/automate/api/external/common/version"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/teams"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/pcmp/prequire"
	"github.com/chef/automate/lib/tls/test/helpers"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"

	"github.com/chef/automate/components/teams-service/constants"
	team_serv "github.com/chef/automate/components/teams-service/server"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/components/teams-service/test"
)

func TestTeamsGRPC(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../storage/postgres/migration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	if migrationConfig == nil {
		serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx, t, l, nil)
		runAllServerTests(t, serv, serviceRef, authzMock, teams.NewTeamsServiceClient(conn), close)
	} else {
		serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx,
			t, l, migrationConfig)
		runAllServerTests(t, serv, serviceRef, authzMock, teams.NewTeamsServiceClient(conn), close)

		// If ciMode, run in-memory AND PG
		// else just run PG.
		if os.Getenv("CI") == "true" {
			serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx, t, l, nil)
			runAllServerTests(t, serv, serviceRef, authzMock, teams.NewTeamsServiceClient(conn), close)
		}
	}
}

func runAllServerTests(
	t *testing.T, serv *team_serv.TeamServer, serviceRef *service.Service,
	authzMock *authz.PoliciesServiceServerMock, cl teams.TeamsServiceClient, close func()) {

	t.Helper()
	defer close()

	t.Run("GetVersion", func(t *testing.T) {
		version.Version = "20200417212701"
		version.BuildTime = "20200417212701"
		version.GitSHA = "eaf1f3553eb64fb9f393366e8ba4ee61e515727e"

		resp, err := cl.GetVersion(context.Background(), &version_api.VersionInfoRequest{})
		require.NoError(t, err)

		expectedVersion := &version_api.VersionInfo{
			Name:    "teams-service",
			Version: "20200417212701",
			Built:   "20200417212701",
			Sha:     "eaf1f3553eb64fb9f393366e8ba4ee61e515727e",
		}

		prequire.Equal(t, expectedVersion, resp)
	})

	t.Run("GetTeam", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when the team does not exist", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetTeam(ctx, &teams.GetTeamReq{
				Id: "test team",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when querying for the admins team", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetTeam(ctx, &teams.GetTeamReq{
				Id: storage.AdminsTeamID,
			})

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, storage.AdminsTeamID, resp.Team.Id)
			assert.Equal(t, "admins",
				resp.Team.Name)
		})

		t.Run("when the team exists", func(t *testing.T) {
			ctx := context.Background()
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "i can be the very best...",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			resp, err := cl.GetTeam(ctx, &teams.GetTeamReq{
				Id: initResp.Team.Id,
			})

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, initResp.Team.Id, resp.Team.Id)
			assert.Equal(t, initResp.Team.Name, resp.Team.Name)

			cleanupTeam(t, cl, initResp.Team.Id)
		})
	})

	t.Run("ListTeams", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when the list is successfully returned", func(t *testing.T) {
			ctx := context.Background()
			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "montag",
				Name:     "he is a dag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "does not matter",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)

			list, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			require.NotNil(t, list)
			assert.Contains(t, list.Teams, resp1.Team)
			assert.Contains(t, list.Teams, resp2.Team)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(list.Teams))

			cleanupTeam(t, cl, resp1.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when the list is successfully returned and filtered by projects", func(t *testing.T) {
			ctx := context.Background()
			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "montag",
				Name:     "he is a dag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "does not matter",
				Projects: []string{"project2"},
			})
			require.NoError(t, err)

			ctx = insertProjectsIntoNewContext([]string{"project1"})
			list, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			require.NotNil(t, list)
			assert.Contains(t, list.Teams, resp1.Team)
			assert.Equal(t, 1, len(list.Teams))

			cleanupTeam(t, cl, resp1.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when the list is successfully returned and filtered by *", func(t *testing.T) {
			ctx := context.Background()
			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "montag",
				Name:     "he is a dag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "does not matter",
				Projects: []string{"project2"},
			})
			require.NoError(t, err)

			ctx = insertProjectsIntoNewContext([]string{"*"})
			list, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			require.NotNil(t, list)
			assert.Contains(t, list.Teams, resp1.Team)
			assert.Contains(t, list.Teams, resp2.Team)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(list.Teams))

			cleanupTeam(t, cl, resp1.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when the list is successfully returned and filtered by (unassigned)", func(t *testing.T) {
			ctx := context.Background()
			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:   "montag",
				Name: "he is a dag",
			})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "does not matter",
				Projects: []string{"other_project"},
			})
			require.NoError(t, err)

			ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectID})
			list, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			require.NotNil(t, list)
			assert.Contains(t, list.Teams, resp1.Team)
			assert.Equal(t, 1+len(storage.NonDeletableTeams), len(list.Teams))

			cleanupTeam(t, cl, resp1.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when there is only the non-deletable teams", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, len(storage.NonDeletableTeams), len(resp.Teams))
		})
	})

	t.Run("CreateTeam", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when a valid team is submitted", func(t *testing.T) {
			ctx := context.Background()
			req := &teams.CreateTeamReq{
				Id:       "gotta-catch-em-all",
				Name:     "Corgis Inc.",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			team := resp.Team
			assert.Equal(t, req.Id, team.Id)
			assert.Equal(t, req.Name, team.Name)

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when no projects are passed", func(t *testing.T) {
			ctx := context.Background()
			req := &teams.CreateTeamReq{
				Id:       "gotta-catch-em-all",
				Name:     "Corgis Inc.",
				Projects: []string{},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			team := resp.Team
			assert.Equal(t, req.Id, team.Id)
			assert.Equal(t, req.Name, team.Name)
			assert.Equal(t, 0, len(team.Projects))

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when the team exists", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "some-name",
				Name:     "montag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "some-name",
				Name:     "does not matter",
				Projects: []string{"project1", "project2"},
			})
			assert.Nil(t, resp2)
			grpctest.AssertCode(t, codes.AlreadyExists, err)

			cleanupTeam(t, cl, resp.Team.Id)
		})
	})

	t.Run("DeleteTeam", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when an existing team is deleted", func(t *testing.T) {
			ctx := context.Background()
			teamToDeleteName := "First Name"

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:"+teamToDeleteName {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected team name passed to PurgeSubjectFromPolicies")
			}

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       teamToDeleteName,
				Name:     "montag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "Other Name",
				Name:     "does not matter",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp2)

			teamListBefore, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp1.Team.Id})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Team.Id, resp.Team.Id)
			assert.Equal(t, resp1.Team.Name, resp.Team.Name)

			teamListAfter, err3 := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err3)
			assert.Equal(t, len(storage.NonDeletableTeams), len(teamListAfter.Teams)-1)
			assert.Contains(t, teamListAfter.Teams, resp2.Team)

			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when an existing team is deleted and is in the project filter", func(t *testing.T) {
			ctx := context.Background()
			teamToDeleteName := "First Name"

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:"+teamToDeleteName {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected team name passed to PurgeSubjectFromPolicies")
			}

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       teamToDeleteName,
				Name:     "montag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "Other Name",
				Name:     "does not matter",
				Projects: []string{"project1"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp2)

			teamListBefore, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			ctx = insertProjectsIntoNewContext([]string{"project2"})
			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp1.Team.Id})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Team.Id, resp.Team.Id)
			assert.Equal(t, resp1.Team.Name, resp.Team.Name)

			teamListAfter, err3 := cl.ListTeams(context.Background(), &teams.ListTeamsReq{})
			require.NoError(t, err3)
			assert.Equal(t, len(storage.NonDeletableTeams), len(teamListAfter.Teams)-1)
			assert.Contains(t, teamListAfter.Teams, resp2.Team)

			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when an existing team is deleted and the project filter is *", func(t *testing.T) {
			ctx := context.Background()
			teamToDeleteName := "First Name"

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:"+teamToDeleteName {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected team name passed to PurgeSubjectFromPolicies")
			}

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       teamToDeleteName,
				Name:     "montag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "Other Name",
				Name:     "does not matter",
				Projects: []string{"project1"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp2)

			teamListBefore, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			ctx = insertProjectsIntoNewContext([]string{"*"})
			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp1.Team.Id})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Team.Id, resp.Team.Id)
			assert.Equal(t, resp1.Team.Name, resp.Team.Name)

			teamListAfter, err3 := cl.ListTeams(context.Background(), &teams.ListTeamsReq{})
			require.NoError(t, err3)
			assert.Equal(t, len(storage.NonDeletableTeams), len(teamListAfter.Teams)-1)
			assert.Contains(t, teamListAfter.Teams, resp2.Team)

			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when an existing team is deleted and the project filter is (unassigned)", func(t *testing.T) {
			ctx := context.Background()
			teamToDeleteName := "First Name"

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:"+teamToDeleteName {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected team name passed to PurgeSubjectFromPolicies")
			}

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       teamToDeleteName,
				Name:     "montag",
				Projects: []string{},
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "Other Name",
				Name:     "does not matter",
				Projects: []string{"project1"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp2)

			teamListBefore, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectID})
			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp1.Team.Id})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Team.Id, resp.Team.Id)
			assert.Equal(t, resp1.Team.Name, resp.Team.Name)

			teamListAfter, err3 := cl.ListTeams(context.Background(), &teams.ListTeamsReq{})
			require.NoError(t, err3)
			assert.Equal(t, len(storage.NonDeletableTeams), len(teamListAfter.Teams)-1)
			assert.Contains(t, teamListAfter.Teams, resp2.Team)

			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when an existing team is filtered by projects return NotFound", func(t *testing.T) {
			ctx := context.Background()
			teamToDeleteName := "First Name"

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       teamToDeleteName,
				Name:     "montag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp1)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "Other Name",
				Name:     "does not matter",
				Projects: []string{"project1"},
			})
			require.NoError(t, err)
			require.NotNil(t, resp2)

			teamListBefore, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			ctx = insertProjectsIntoNewContext([]string{"project2"})
			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp2.Team.Id})
			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err2)

			cleanupTeam(t, cl, resp1.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when an existing team is deleted but the deletion of their policy membership fails", func(t *testing.T) {
			ctx := context.Background()
			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				return nil, errors.New("test failure of PurgeSubjectFromPolicies")
			}

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "First Name",
				Name:     "montag",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "Other Name",
				Name:     "does not matter",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)

			teamListBefore, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err)
			require.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp1.Team.Id})
			require.Nil(t, resp)
			require.NotNil(t, err2)
			grpctest.AssertCode(t, codes.Internal, err2)

			teamListAfter, err3 := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err3)
			assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teamListAfter.Teams))
			assert.Contains(t, teamListAfter.Teams, resp2.Team)

			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when the team to delete is not found", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: "some-wrong-id"})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when attempting to delete a team that is not allowed to be deleted", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: storage.AdminsTeamID})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	})

	t.Run("UpdateTeam", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when a valid team update request is submitted", func(t *testing.T) {
			ctx := context.Background()
			id := "gotta-catch-em-all"
			req := &teams.CreateTeamReq{
				Id:       id,
				Name:     "Corgis Inc.",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			newName := "Gotta Catch Only The Most Special"
			updateReq := &teams.UpdateTeamReq{
				Id:       id,
				Name:     newName,
				Projects: []string{"project2", "project3"},
			}
			updatedTeamResp, err := cl.UpdateTeam(ctx, updateReq)
			require.NoError(t, err, "update team")
			require.NotNil(t, resp)
			assert.Equal(t, updateReq.Id, updatedTeamResp.Team.Id)
			assert.Equal(t, updateReq.Name, updatedTeamResp.Team.Name)
			assert.Equal(t, updateReq.Projects, updatedTeamResp.Team.Projects)

			teamsList, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err, "reading back teams")

			require.Equal(t, 2, len(teamsList.Teams))
			var updatedTeam *teams.Team
			if teamsList.Teams[0].Id != storage.AdminsTeamID {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when a valid team update request is submitted with the project filter", func(t *testing.T) {
			ctx := context.Background()
			id := "gotta-catch-em-all"
			req := &teams.CreateTeamReq{
				Id:       id,
				Name:     "Corgis Inc.",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			newName := "Gotta Catch Only The Most Special"
			updateReq := &teams.UpdateTeamReq{
				Id:       id,
				Name:     newName,
				Projects: []string{"project2", "project3"},
			}
			updatedTeamResp, err := cl.UpdateTeam(insertProjectsIntoNewContext([]string{"project2"}), updateReq)
			require.NoError(t, err, "update team")
			require.NotNil(t, resp)
			assert.Equal(t, updateReq.Id, updatedTeamResp.Team.Id)
			assert.Equal(t, updateReq.Name, updatedTeamResp.Team.Name)
			assert.Equal(t, updateReq.Projects, updatedTeamResp.Team.Projects)

			teamsList, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err, "reading back teams")

			require.Equal(t, 2, len(teamsList.Teams))
			var updatedTeam *teams.Team
			if teamsList.Teams[0].Id != storage.AdminsTeamID {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when a valid team update request is submitted with the project filter of *", func(t *testing.T) {
			ctx := context.Background()
			id := "gotta-catch-em-all"
			req := &teams.CreateTeamReq{
				Id:       id,
				Name:     "Corgis Inc.",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			newName := "Gotta Catch Only The Most Special"
			updateReq := &teams.UpdateTeamReq{
				Id:       id,
				Name:     newName,
				Projects: []string{"project2", "project3"},
			}
			updatedTeamResp, err := cl.UpdateTeam(insertProjectsIntoNewContext([]string{"*"}), updateReq)
			require.NoError(t, err, "update team")
			require.NotNil(t, resp)
			assert.Equal(t, updateReq.Id, updatedTeamResp.Team.Id)
			assert.Equal(t, updateReq.Name, updatedTeamResp.Team.Name)
			assert.Equal(t, updateReq.Projects, updatedTeamResp.Team.Projects)

			teamsList, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err, "reading back teams")

			require.Equal(t, 2, len(teamsList.Teams))
			var updatedTeam *teams.Team
			if teamsList.Teams[0].Id != storage.AdminsTeamID {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when a valid team update request is submitted with the project filter of (unassigned)", func(t *testing.T) {
			ctx := context.Background()
			id := "gotta-catch-em-all"
			req := &teams.CreateTeamReq{
				Id:       id,
				Name:     "Corgis Inc.",
				Projects: []string{},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			newName := "Gotta Catch Only The Most Special"
			updateReq := &teams.UpdateTeamReq{
				Id:       id,
				Name:     newName,
				Projects: []string{"project2", "project3"},
			}
			updatedTeamResp, err := cl.UpdateTeam(insertProjectsIntoNewContext([]string{constants.UnassignedProjectID}), updateReq)
			require.NoError(t, err, "update team")
			require.NotNil(t, resp)
			assert.Equal(t, updateReq.Id, updatedTeamResp.Team.Id)
			assert.Equal(t, updateReq.Name, updatedTeamResp.Team.Name)
			assert.Equal(t, updateReq.Projects, updatedTeamResp.Team.Projects)

			teamsList, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
			require.NoError(t, err, "reading back teams")

			require.Equal(t, 2, len(teamsList.Teams))
			var updatedTeam *teams.Team
			if teamsList.Teams[0].Id != storage.AdminsTeamID {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when a valid team update request is submitted but is excluded by the project filter", func(t *testing.T) {
			ctx := context.Background()
			id := "gotta-catch-em-all"
			req := &teams.CreateTeamReq{
				Id:       id,
				Name:     "Corgis Inc.",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			newName := "Gotta Catch Only The Most Special"
			updateReq := &teams.UpdateTeamReq{
				Id:       id,
				Name:     newName,
				Projects: []string{"project2", "project3"},
			}
			updatedTeamResp, err := cl.UpdateTeam(insertProjectsIntoNewContext([]string{"project3"}), updateReq)
			require.Nil(t, updatedTeamResp)
			grpctest.AssertCode(t, codes.NotFound, err)

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when the team exists but all projects are removed", func(t *testing.T) {
			ctx := context.Background()
			id := "gotta-catch-em-all"
			req := &teams.CreateTeamReq{
				Id:       id,
				Name:     "Corgis Inc.",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			updateReq := &teams.UpdateTeamReq{
				Id:       id,
				Name:     "Corgis Inc.",
				Projects: []string{},
			}
			updatedTeamResp, err := cl.UpdateTeam(ctx, updateReq)
			require.NoError(t, err)
			require.NotNil(t, resp)

			team := updatedTeamResp.Team
			assert.Equal(t, req.Id, team.Id)
			assert.Equal(t, req.Name, team.Name)
			assert.Equal(t, 0, len(team.Projects))

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when team to update does not exist", func(t *testing.T) {
			ctx := context.Background()
			updateReq := &teams.UpdateTeamReq{
				Id:       "not-found-id",
				Name:     "Corgis Inc.",
				Projects: []string{"project1", "project2"},
			}
			updatedTeam, err := cl.UpdateTeam(ctx, updateReq)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.NotFound, err)
		})
	})

	t.Run("AddTeamMembers", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("successfully adds user", func(t *testing.T) {
			tests := []struct {
				users []string
				desc  string
			}{
				{[]string{"6ed95714-9466-463b-80da-0513ecb42a08"}, "single user"},
				{[]string{
					"299ea25b-62d4-4660-965a-e25870298792",
					"d1f642c8-8907-4e8b-a9a0-b998a44dc4bf",
				}, "multiple users"},
			}
			for _, test := range tests {
				t.Run("when provided valid team and "+test.desc, func(t *testing.T) {
					ctx := context.Background()

					// arrange
					req := &teams.CreateTeamReq{
						Name:     "Gotta Catch Em All",
						Id:       "corgis-inc",
						Projects: []string{"project1", "project2"},
					}
					resp, err := cl.CreateTeam(ctx, req)
					require.NoError(t, err)

					addReq := &teams.AddTeamMembersReq{
						Id:      req.Id,
						UserIds: test.users,
					}

					// act
					resp2, err := cl.AddTeamMembers(ctx, addReq)

					// assert
					require.NoError(t, err)
					require.NotNil(t, resp2)
					assert.Equal(t, len(addReq.UserIds), len(resp2.UserIds))
					assert.ElementsMatch(t, addReq.UserIds, resp2.UserIds)

					cleanupTeam(t, cl, resp.Team.Id)
				})
			}
		})

		t.Run("successfully adds user when the project filter matches", func(t *testing.T) {
			tests := []struct {
				users []string
				desc  string
			}{
				{[]string{"6ed95714-9466-463b-80da-0513ecb42a08"}, "single user"},
				{[]string{
					"299ea25b-62d4-4660-965a-e25870298792",
					"d1f642c8-8907-4e8b-a9a0-b998a44dc4bf",
				}, "multiple users"},
			}
			for _, test := range tests {
				t.Run("when provided valid team and "+test.desc, func(t *testing.T) {
					ctx := context.Background()

					// arrange
					req := &teams.CreateTeamReq{
						Name:     "Gotta Catch Em All",
						Id:       "corgis-inc",
						Projects: []string{"project1", "project2"},
					}
					resp, err := cl.CreateTeam(ctx, req)
					require.NoError(t, err)

					addReq := &teams.AddTeamMembersReq{
						Id:      resp.GetTeam().GetId(),
						UserIds: test.users,
					}

					// act
					resp2, err := cl.AddTeamMembers(insertProjectsIntoNewContext([]string{"project1"}), addReq)

					// assert
					require.NoError(t, err)
					require.NotNil(t, resp2)
					assert.Equal(t, len(addReq.UserIds), len(resp2.UserIds))
					assert.ElementsMatch(t, addReq.UserIds, resp2.UserIds)

					cleanupTeam(t, cl, resp.Team.Id)
				})
			}
		})

		t.Run("successfully adds user with a project filter of *", func(t *testing.T) {
			tests := []struct {
				users []string
				desc  string
			}{
				{[]string{"6ed95714-9466-463b-80da-0513ecb42a08"}, "single user"},
				{[]string{
					"299ea25b-62d4-4660-965a-e25870298792",
					"d1f642c8-8907-4e8b-a9a0-b998a44dc4bf",
				}, "multiple users"},
			}
			for _, test := range tests {
				t.Run("when provided valid team and "+test.desc, func(t *testing.T) {
					ctx := context.Background()

					// arrange
					req := &teams.CreateTeamReq{
						Name:     "Gotta Catch Em All",
						Id:       "corgis-inc",
						Projects: []string{"project1", "project2"},
					}
					resp, err := cl.CreateTeam(ctx, req)
					require.NoError(t, err)

					addReq := &teams.AddTeamMembersReq{
						Id:      resp.GetTeam().GetId(),
						UserIds: test.users,
					}

					// act
					resp2, err := cl.AddTeamMembers(insertProjectsIntoNewContext([]string{"*"}), addReq)

					// assert
					require.NoError(t, err)
					require.NotNil(t, resp2)
					assert.Equal(t, len(addReq.UserIds), len(resp2.UserIds))
					assert.ElementsMatch(t, addReq.UserIds, resp2.UserIds)

					cleanupTeam(t, cl, resp.Team.Id)
				})
			}
		})

		t.Run("successfully adds user with a project filter of (unassigned)", func(t *testing.T) {
			tests := []struct {
				users []string
				desc  string
			}{
				{[]string{"6ed95714-9466-463b-80da-0513ecb42a08"}, "single user"},
				{[]string{
					"299ea25b-62d4-4660-965a-e25870298792",
					"d1f642c8-8907-4e8b-a9a0-b998a44dc4bf",
				}, "multiple users"},
			}
			for _, test := range tests {
				t.Run("when provided valid team and "+test.desc, func(t *testing.T) {
					ctx := context.Background()

					// arrange
					req := &teams.CreateTeamReq{
						Name:     "Gotta Catch Em All",
						Id:       "corgis-inc",
						Projects: []string{},
					}
					resp, err := cl.CreateTeam(ctx, req)
					require.NoError(t, err)

					addReq := &teams.AddTeamMembersReq{
						Id:      resp.GetTeam().GetId(),
						UserIds: test.users,
					}

					// act
					resp2, err := cl.AddTeamMembers(insertProjectsIntoNewContext([]string{constants.UnassignedProjectID}), addReq)

					// assert
					require.NoError(t, err)
					require.NotNil(t, resp2)
					assert.Equal(t, len(addReq.UserIds), len(resp2.UserIds))
					assert.ElementsMatch(t, addReq.UserIds, resp2.UserIds)

					cleanupTeam(t, cl, resp.Team.Id)
				})
			}
		})

		t.Run("successfully adds user to team with existing users", func(t *testing.T) {
			tests := []struct {
				users []string
				desc  string
			}{
				{[]string{"6ed95714-9466-463b-80da-0513ecb42a08"}, "single user"},
				{[]string{
					"299ea25b-62d4-4660-965a-e25870298792",
					"d1f642c8-8907-4e8b-a9a0-b998a44dc4bf",
				}, "multiple users"},
			}
			for _, test := range tests {
				t.Run("when provided valid team and "+test.desc, func(t *testing.T) {
					ctx := context.Background()

					// arrange
					req := &teams.CreateTeamReq{
						Name:     "Gotta Catch Em All",
						Id:       "corgis-inc",
						Projects: []string{},
					}
					resp, err := cl.CreateTeam(ctx, req)
					require.NoError(t, err)

					targetMemberID := "88f13b6b-b20b-4335-9fd6-2c09edf45cf9"
					resp1, err := cl.AddTeamMembers(insertProjectsIntoNewContext([]string{constants.UnassignedProjectID}), &teams.AddTeamMembersReq{
						Id:      req.Id,
						UserIds: []string{targetMemberID},
					})
					require.NoError(t, err)
					require.Equal(t, 1, len(resp1.UserIds))

					addReq := &teams.AddTeamMembersReq{
						Id:      req.Id,
						UserIds: test.users,
					}
					// act
					resp2, err := cl.AddTeamMembers(insertProjectsIntoNewContext([]string{constants.UnassignedProjectID}), addReq)

					// assert
					require.NoError(t, err)
					require.NotNil(t, resp2)
					assert.Equal(t, len(addReq.UserIds)+1, len(resp2.UserIds))

					expectedUsers := append(addReq.UserIds, targetMemberID)
					assert.ElementsMatch(t, expectedUsers, resp2.UserIds)

					cleanupTeam(t, cl, resp.Team.Id)
				})
			}
		})

		t.Run("fails to adds user with NotFound when a project filter that excludes the team", func(t *testing.T) {
			tests := []struct {
				users []string
				desc  string
			}{
				{[]string{"6ed95714-9466-463b-80da-0513ecb42a08"}, "single user"},
				{[]string{
					"299ea25b-62d4-4660-965a-e25870298792",
					"d1f642c8-8907-4e8b-a9a0-b998a44dc4bf",
				}, "multiple users"},
			}
			for _, test := range tests {
				t.Run("when provided valid team and "+test.desc, func(t *testing.T) {
					ctx := context.Background()

					// arrange
					req := &teams.CreateTeamReq{
						Name:     "Gotta Catch Em All",
						Id:       "corgis-inc",
						Projects: []string{"project1", "project2"},
					}
					resp, err := cl.CreateTeam(ctx, req)
					require.NoError(t, err)

					addReq := &teams.AddTeamMembersReq{
						Id:      resp.GetTeam().GetId(),
						UserIds: test.users,
					}

					// act
					resp2, err := cl.AddTeamMembers(insertProjectsIntoNewContext([]string{"wrong_project"}), addReq)

					// assert
					require.Nil(t, resp2)
					grpctest.AssertCode(t, codes.NotFound, err)

					cleanupTeam(t, cl, resp.Team.Id)
				})
			}
		})

		t.Run("when team exists and user has already been added, does not add duplicate user", func(t *testing.T) {
			ctx := context.Background()
			req := &teams.CreateTeamReq{
				Name:     "with, learning, & wisdom",
				Id:       "ravenclaw",
				Projects: []string{"project1", "project2"},
			}
			createTeam, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			users := []string{"some-id"}
			addReq := &teams.AddTeamMembersReq{
				Id:      createTeam.GetTeam().GetId(),
				UserIds: users,
			}

			// add user first time
			resp, err := cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err, "first add")
			assert.Equal(t, len(users), len(resp.UserIds))
			assert.ElementsMatch(t, users, resp.UserIds)

			// attempt to add user second time
			resp2, err := cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err, "second add")
			assert.Equal(t, len(users), len(resp2.UserIds))
			assert.ElementsMatch(t, users, resp2.UserIds)

			cleanupTeam(t, cl, createTeam.Team.Id)
		})
		t.Run("when team does not exist, returns Not Found error", func(t *testing.T) {
			ctx := context.Background()

			addReq := &teams.AddTeamMembersReq{
				Id:      "not-found",
				UserIds: []string{"a-user"},
			}

			resp, err := cl.AddTeamMembers(ctx, addReq)

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when no user ids provided, returns invalid request", func(t *testing.T) {
			ctx := context.Background()
			req := &teams.CreateTeamReq{
				Name:     "with, learning, & wisdom",
				Id:       "ravenclaw",
				Projects: []string{"project1", "project2"},
			}
			_, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			addReq := &teams.AddTeamMembersReq{
				Id:      "ravenclaw",
				UserIds: []string{},
			}

			resp, err := cl.AddTeamMembers(ctx, addReq)

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	})

	t.Run("RemoveTeamMembers", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when team does not exist, returns NotFound error", func(t *testing.T) {
			ctx := context.Background()
			req := &teams.RemoveTeamMembersReq{
				Id:      "not-found-id",
				UserIds: []string{"some-id"},
			}

			updatedTeam, err := cl.RemoveTeamMembers(ctx, req)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when team exists without users the list remains empty", func(t *testing.T) {
			ctx := context.Background()
			createReq := &teams.CreateTeamReq{
				Name:     "Guard the galaxy (with dope music)",
				Id:       "guardians",
				Projects: []string{"project1", "project2"},
			}
			createTeam, err := cl.CreateTeam(ctx, createReq)
			require.NoError(t, err)

			users := []string{"user-1", "user-2"}
			req := &teams.RemoveTeamMembersReq{
				Id:      createTeam.Team.Id,
				UserIds: users,
			}
			resp, err := cl.RemoveTeamMembers(ctx, req)
			require.NoError(t, err)
			assert.Equal(t, 0, len(resp.UserIds))

			cleanupTeam(t, cl, createTeam.Team.Id)
		})

		t.Run("when team exists with a project filter", func(t *testing.T) {
			ctx := context.Background()
			createReq := &teams.CreateTeamReq{
				Name:     "Guard the galaxy (with dope music)",
				Id:       "guardians",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, createReq)
			require.NoError(t, err)
			addReq := &teams.AddTeamMembersReq{
				Id: resp.GetTeam().GetId(),
				UserIds: []string{
					"user-1",
					"user-2",
					"user-3",
				},
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			req := &teams.RemoveTeamMembersReq{
				Id: resp.Team.Id,
				UserIds: []string{
					"user-1",
					"user-2",
				},
			}
			removeResp, err := cl.RemoveTeamMembers(insertProjectsIntoNewContext([]string{"project1", "other"}), req)
			require.NoError(t, err)
			assert.Equal(t, 1, len(removeResp.UserIds))

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when team exists with a project filter of *", func(t *testing.T) {
			ctx := context.Background()
			createReq := &teams.CreateTeamReq{
				Name:     "Guard the galaxy (with dope music)",
				Id:       "guardians",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, createReq)
			require.NoError(t, err)
			addReq := &teams.AddTeamMembersReq{
				Id: resp.GetTeam().GetId(),
				UserIds: []string{
					"user-1",
					"user-2",
					"user-3",
				},
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			req := &teams.RemoveTeamMembersReq{
				Id: resp.Team.Id,
				UserIds: []string{
					"user-1",
					"user-2",
				},
			}
			removeResp, err := cl.RemoveTeamMembers(insertProjectsIntoNewContext([]string{"*"}), req)
			require.NoError(t, err)
			assert.Equal(t, 1, len(removeResp.UserIds))

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when team exists with a project filter of (unassigned)", func(t *testing.T) {
			ctx := context.Background()
			createReq := &teams.CreateTeamReq{
				Name:     "Guard the galaxy (with dope music)",
				Id:       "guardians",
				Projects: []string{},
			}
			resp, err := cl.CreateTeam(ctx, createReq)
			require.NoError(t, err)
			addReq := &teams.AddTeamMembersReq{
				Id: resp.GetTeam().GetId(),
				UserIds: []string{
					"user-1",
					"user-2",
					"user-3",
				},
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			req := &teams.RemoveTeamMembersReq{
				Id: resp.Team.Id,
				UserIds: []string{
					"user-1",
					"user-2",
				},
			}
			removeResp, err := cl.RemoveTeamMembers(insertProjectsIntoNewContext([]string{constants.UnassignedProjectID}), req)
			require.NoError(t, err)
			assert.Equal(t, 1, len(removeResp.UserIds))

			cleanupTeam(t, cl, resp.Team.Id)
		})

		t.Run("when team exists with a project filter that excludes the team", func(t *testing.T) {
			ctx := context.Background()
			createReq := &teams.CreateTeamReq{
				Name:     "Guard the galaxy (with dope music)",
				Id:       "guardians",
				Projects: []string{"project1"},
			}
			resp, err := cl.CreateTeam(ctx, createReq)
			require.NoError(t, err)
			addReq := &teams.AddTeamMembersReq{
				Id: resp.GetTeam().GetId(),
				UserIds: []string{
					"user-1",
					"user-2",
					"user-3",
				},
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			req := &teams.RemoveTeamMembersReq{
				Id: resp.Team.Id,
				UserIds: []string{
					"user-1",
					"user-2",
				},
			}
			removeResp, err := cl.RemoveTeamMembers(insertProjectsIntoNewContext([]string{"wrong"}), req)
			require.Nil(t, removeResp)
			grpctest.AssertCode(t, codes.NotFound, err)

			cleanupTeam(t, cl, resp.Team.Id)
		})

		tests := map[string]struct {
			usersToStart            []string
			usersToRemove           []string
			expectedLengthRemaining int
		}{
			"with the same set of users as to delete, the list becomes empty": {
				usersToStart: []string{
					"user-1",
					"user-2",
				},
				usersToRemove: []string{
					"user-1",
					"user-2",
				},
				expectedLengthRemaining: 0,
			},
			"with intersecting users existing and to remove, the list is updated": {
				usersToStart: []string{
					"user-1",
					"user-2",
					"user-3",
				},
				usersToRemove: []string{
					"user-1",
					"user-2",
				},
				expectedLengthRemaining: 1,
			},
			"with users, but an empty user list is passed": {
				usersToStart: []string{
					"user-1",
					"user-2",
					"user-3",
				},
				usersToRemove:           []string{},
				expectedLengthRemaining: 3,
			},
		}
		for desc, test := range tests {
			ctx := context.Background()
			t.Run("when team exists "+desc, func(t *testing.T) {
				createReq := &teams.CreateTeamReq{
					Name:     "Guard the galaxy (with dope music)",
					Id:       "guardians",
					Projects: []string{"project1", "project2"},
				}
				resp, err := cl.CreateTeam(ctx, createReq)
				require.NoError(t, err)
				addReq := &teams.AddTeamMembersReq{
					Id:      resp.GetTeam().GetId(),
					UserIds: test.usersToStart,
				}
				_, err = cl.AddTeamMembers(ctx, addReq)
				require.NoError(t, err)

				req := &teams.RemoveTeamMembersReq{
					Id:      resp.Team.Id,
					UserIds: test.usersToRemove,
				}
				removeResp, err := cl.RemoveTeamMembers(ctx, req)
				require.NoError(t, err)
				assert.Equal(t, test.expectedLengthRemaining, len(removeResp.UserIds))

				cleanupTeam(t, cl, resp.Team.Id)
			})
		}
	})

	t.Run("GetTeamsForMember", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when valid member id provided with a project filter, "+
			"returns array of teams that are in project", func(t *testing.T) {
			ctx := context.Background()
			// create first team
			req := &teams.CreateTeamReq{
				Name:     "daring, nerve, & chivalry",
				Id:       "gryffindor",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			req2 := &teams.CreateTeamReq{
				Name:     "save the wizarding world",
				Id:       "aurors",
				Projects: []string{"project1", "project3"},
			}
			resp2, err := cl.CreateTeam(ctx, req2)
			require.NoError(t, err)

			req3 := &teams.CreateTeamReq{
				Name:     "destroy the wizarding world",
				Id:       "death-eaters",
				Projects: []string{},
			}
			resp3, err := cl.CreateTeam(ctx, req3)
			require.NoError(t, err)

			users := []string{"user-1"}
			addReq := &teams.AddTeamMembersReq{
				Id:      resp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			addReq2 := &teams.AddTeamMembersReq{
				Id:      resp2.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq2)
			require.NoError(t, err)

			addReq3 := &teams.AddTeamMembersReq{
				Id:      resp3.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq3)
			require.NoError(t, err)

			listReq := &teams.GetTeamsForMemberReq{
				UserId: users[0],
			}
			fetchedData, err := cl.GetTeamsForMember(
				insertProjectsIntoNewContext([]string{"project2", constants.UnassignedProjectID}), listReq)

			require.NoError(t, err)
			require.NotNil(t, fetchedData)
			assert.Equal(t, 2, len(fetchedData.Teams))
			fetchedTeamIDs := []string{fetchedData.Teams[0].Id, fetchedData.Teams[1].Id}
			assert.Contains(t, fetchedTeamIDs, resp.Team.Id)
			assert.Contains(t, fetchedTeamIDs, resp3.Team.Id)

			cleanupTeam(t, cl, resp.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
			cleanupTeam(t, cl, resp3.Team.Id)
		})

		t.Run("when valid member id provided with a project filter of *, returns array of all teams", func(t *testing.T) {
			ctx := context.Background()
			// create first team
			req := &teams.CreateTeamReq{
				Name:     "daring, nerve, & chivalry",
				Id:       "gryffindor",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			// create second team
			req2 := &teams.CreateTeamReq{
				Name:     "save the wizarding world",
				Id:       "aurors",
				Projects: []string{"project1", "project2"},
			}
			resp2, err := cl.CreateTeam(ctx, req2)
			require.NoError(t, err)

			// add user to first team
			users := []string{"user-1"}
			addReq := &teams.AddTeamMembersReq{
				Id:      resp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			// add user to second team
			addReq2 := &teams.AddTeamMembersReq{
				Id:      resp2.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq2)
			require.NoError(t, err)

			// get user's teams
			listReq := &teams.GetTeamsForMemberReq{
				UserId: users[0],
			}
			fetchedData, err := cl.GetTeamsForMember(insertProjectsIntoNewContext([]string{"*"}), listReq)

			require.NoError(t, err)
			require.NotNil(t, fetchedData)
			assert.Equal(t, 2, len(fetchedData.Teams))
			fetchedTeamIDs := []string{fetchedData.Teams[0].Id, fetchedData.Teams[1].Id}
			assert.Contains(t, fetchedTeamIDs, resp.Team.Id)
			assert.Contains(t, fetchedTeamIDs, resp2.Team.Id)

			cleanupTeam(t, cl, resp.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when valid member id provided with a project filter of (unassigned), "+
			"returns array of (unassigned) teams", func(t *testing.T) {

			ctx := context.Background()
			req := &teams.CreateTeamReq{
				Name:     "daring, nerve, & chivalry",
				Id:       "gryffindor",
				Projects: []string{},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			req2 := &teams.CreateTeamReq{
				Name:     "save the wizarding world",
				Id:       "aurors",
				Projects: []string{"project1", "project2"},
			}
			resp2, err := cl.CreateTeam(ctx, req2)
			require.NoError(t, err)

			req3 := &teams.CreateTeamReq{
				Name:     "destroy the wizarding world",
				Id:       "death-eaters",
				Projects: []string{},
			}
			resp3, err := cl.CreateTeam(ctx, req3)
			require.NoError(t, err)

			users := []string{"user-1"}
			addReq := &teams.AddTeamMembersReq{
				Id:      resp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			addReq2 := &teams.AddTeamMembersReq{
				Id:      resp2.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq2)
			require.NoError(t, err)

			addReq3 := &teams.AddTeamMembersReq{
				Id:      resp3.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq3)
			require.NoError(t, err)

			listReq := &teams.GetTeamsForMemberReq{
				UserId: users[0],
			}
			fetchedData, err := cl.GetTeamsForMember(insertProjectsIntoNewContext([]string{constants.UnassignedProjectID}), listReq)

			require.NoError(t, err)
			require.NotNil(t, fetchedData)
			assert.Equal(t, 2, len(fetchedData.Teams))
			fetchedTeamIDs := []string{fetchedData.Teams[0].Id, fetchedData.Teams[1].Id}
			assert.Contains(t, fetchedTeamIDs, resp.Team.Id)
			assert.Contains(t, fetchedTeamIDs, resp3.Team.Id)

			cleanupTeam(t, cl, resp.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
			cleanupTeam(t, cl, resp3.Team.Id)
		})

		t.Run("when valid member id and project filter provided, returns array of teams", func(t *testing.T) {
			ctx := context.Background()
			// create first team
			req := &teams.CreateTeamReq{
				Name:     "daring, nerve, & chivalry",
				Id:       "gryffindor",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			// create second team
			req2 := &teams.CreateTeamReq{
				Name:     "save the wizarding world",
				Id:       "aurors",
				Projects: []string{"project1", "project2"},
			}
			resp2, err := cl.CreateTeam(ctx, req2)
			require.NoError(t, err)

			// add user to first team
			users := []string{"user-1"}
			addReq := &teams.AddTeamMembersReq{
				Id:      resp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			// add user to second team
			addReq2 := &teams.AddTeamMembersReq{
				Id:      resp2.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq2)
			require.NoError(t, err)

			// get user's teams
			listReq := &teams.GetTeamsForMemberReq{
				UserId: users[0],
			}
			fetchedData, err := cl.GetTeamsForMember(ctx, listReq)

			require.NoError(t, err)
			require.NotNil(t, fetchedData)
			assert.Equal(t, 2, len(fetchedData.Teams))
			fetchedTeamIDs := []string{fetchedData.Teams[0].Id, fetchedData.Teams[1].Id}
			assert.Contains(t, fetchedTeamIDs, resp.Team.Id)
			assert.Contains(t, fetchedTeamIDs, resp2.Team.Id)

			cleanupTeam(t, cl, resp.Team.Id)
			cleanupTeam(t, cl, resp2.Team.Id)
		})

		t.Run("when user id does not exist on any teams, returns empty array", func(t *testing.T) {
			ctx := context.Background()
			req := &teams.CreateTeamReq{
				Name:     "cunning & ambitious",
				Id:       "slytherin",
				Projects: []string{"project1", "project2"},
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			listReq := &teams.GetTeamsForMemberReq{
				UserId: "user-1",
			}

			fetchedData, err := cl.GetTeamsForMember(ctx, listReq)

			require.NoError(t, err)
			require.NotNil(t, fetchedData)
			assert.Empty(t, fetchedData.Teams)

			cleanupTeam(t, cl, resp.Team.Id)
		})
	})

	t.Run("GetTeamMembership", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when the team does not exist", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetTeamMembership(ctx, &teams.GetTeamMembershipReq{
				Id: "not-found",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when the team exists but has no members", func(t *testing.T) {
			ctx := context.Background()
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "i can be the very best...",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			resp, err := cl.GetTeamMembership(ctx, &teams.GetTeamMembershipReq{
				Id: initResp.Team.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, 0, len(resp.UserIds))

			cleanupTeam(t, cl, initResp.Team.Id)
		})

		t.Run("when the team exists with members", func(t *testing.T) {
			ctx := context.Background()
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "i can be the very best...",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			users := []string{"user-1", "user-2"}
			addReq := &teams.AddTeamMembersReq{
				Id:      initResp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			resp, err := cl.GetTeamMembership(ctx, &teams.GetTeamMembershipReq{
				Id: initResp.Team.Id,
			})
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(users), len(resp.UserIds))
			assert.ElementsMatch(t, users, resp.UserIds)

			cleanupTeam(t, cl, initResp.Team.Id)
		})

		t.Run("when the team exists with members and is in the project filter", func(t *testing.T) {
			ctx := context.Background()
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "i can be the very best...",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			users := []string{"user-1", "user-2"}
			addReq := &teams.AddTeamMembersReq{
				Id:      initResp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			resp, err := cl.GetTeamMembership(
				insertProjectsIntoNewContext([]string{"project1"}),
				&teams.GetTeamMembershipReq{
					Id: initResp.Team.Id,
				})
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(users), len(resp.UserIds))
			assert.ElementsMatch(t, users, resp.UserIds)

			cleanupTeam(t, cl, initResp.Team.Id)
		})

		t.Run("when the team exists with members and the project filter is *", func(t *testing.T) {
			ctx := context.Background()
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "i can be the very best...",
				Projects: []string{"project1", "project2"},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			users := []string{"user-1", "user-2"}
			addReq := &teams.AddTeamMembersReq{
				Id:      initResp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			resp, err := cl.GetTeamMembership(
				insertProjectsIntoNewContext([]string{"*"}),
				&teams.GetTeamMembershipReq{
					Id: initResp.Team.Id,
				})
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(users), len(resp.UserIds))
			assert.ElementsMatch(t, users, resp.UserIds)

			cleanupTeam(t, cl, initResp.Team.Id)
		})

		t.Run("when the team exists with members and the project filter is (unassigned)", func(t *testing.T) {
			ctx := context.Background()
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "i can be the very best...",
				Projects: []string{},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			users := []string{"user-1", "user-2"}
			addReq := &teams.AddTeamMembersReq{
				Id:      initResp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			resp, err := cl.GetTeamMembership(
				insertProjectsIntoNewContext([]string{constants.UnassignedProjectID}),
				&teams.GetTeamMembershipReq{
					Id: initResp.Team.Id,
				})
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(users), len(resp.UserIds))
			assert.ElementsMatch(t, users, resp.UserIds)

			cleanupTeam(t, cl, initResp.Team.Id)
		})

		t.Run("when the team exists with members and the project filter is excludes the team", func(t *testing.T) {
			ctx := context.Background()
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Id:       "other-team",
				Name:     "i can be the very best...",
				Projects: []string{"project1"},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			users := []string{"user-1", "user-2"}
			addReq := &teams.AddTeamMembersReq{
				Id:      initResp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddTeamMembers(ctx, addReq)
			require.NoError(t, err)

			resp, err := cl.GetTeamMembership(
				insertProjectsIntoNewContext([]string{"wrong"}),
				&teams.GetTeamMembershipReq{
					Id: initResp.Team.Id,
				})
			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)

			cleanupTeam(t, cl, initResp.Team.Id)
		})
	})

	t.Run("PurgeUserMembership", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when user id is not passed, returns InvalidArgument error", func(t *testing.T) {
			ctx := context.Background()
			req := &teams.PurgeUserMembershipReq{
				UserId: "",
			}

			resp, err := cl.PurgeUserMembership(ctx, req)

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		tests := map[string]struct {
			userToPurge             string
			initialTeamsAndMembers  map[string][]string
			expectedTeamsAndMembers map[string][]string
			expectedUpdatedTeamIDs  map[string]bool
		}{
			"when is only the admins team": {
				userToPurge:            "f2f5300c-48dc-4633-8ac8-2bcf814e7b8a",
				initialTeamsAndMembers: map[string][]string{},
				expectedTeamsAndMembers: map[string][]string{
					storage.AdminsTeamID: {},
				},
				expectedUpdatedTeamIDs: map[string]bool{},
			},
			`when there are multiple teams and the purged user
				is a member of one of them, only one team should be updated`: {
				userToPurge: "2041bad7-8ae4-418b-9e66-6af87838ab97",
				initialTeamsAndMembers: map[string][]string{
					"team1": {
						"2041bad7-8ae4-418b-9e66-6af87838ab97",
						"2041bad8-8ae4-418b-9e66-6af87838ab97",
						"2041bad9-8ae4-418b-9e66-6af87838ab97",
					},
					"team2": {
						"c34d1891-907e-4677-bc90-458c9e94f772",
						"c34d1892-907e-4677-bc90-458c9e94f772",
						"c34d1893-907e-4677-bc90-458c9e94f772",
					},
				},
				expectedTeamsAndMembers: map[string][]string{
					storage.AdminsTeamID: {},
					"team1": {
						"2041bad8-8ae4-418b-9e66-6af87838ab97",
						"2041bad9-8ae4-418b-9e66-6af87838ab97",
					},
					"team2": {
						"c34d1891-907e-4677-bc90-458c9e94f772",
						"c34d1892-907e-4677-bc90-458c9e94f772",
						"c34d1893-907e-4677-bc90-458c9e94f772",
					},
				},
				expectedUpdatedTeamIDs: map[string]bool{"team1": true},
			},
			`when there is only one team besides the admins team and the deleted user is a member
				the team should be updated`: {
				userToPurge: "2041bad8-8ae4-418b-9e66-6af87838ab97",
				initialTeamsAndMembers: map[string][]string{
					"team1": {
						"2041bad7-8ae4-418b-9e66-6af87838ab97",
						"2041bad8-8ae4-418b-9e66-6af87838ab97",
						"2041bad9-8ae4-418b-9e66-6af87838ab97",
					},
				},
				expectedTeamsAndMembers: map[string][]string{
					storage.AdminsTeamID: {},
					"team1": {
						"2041bad7-8ae4-418b-9e66-6af87838ab97",
						"2041bad9-8ae4-418b-9e66-6af87838ab97",
					},
				},
				expectedUpdatedTeamIDs: map[string]bool{"team1": true},
			},
			`when there are multiple teams and the purged user
				is a member of both, both teams should be updated`: {
				userToPurge: "2041bad9-8ae4-418b-9e66-6af87838ab97",
				initialTeamsAndMembers: map[string][]string{
					"team1": {
						"2041bad7-8ae4-418b-9e66-6af87838ab97",
						"2041bad8-8ae4-418b-9e66-6af87838ab97",
						"2041bad9-8ae4-418b-9e66-6af87838ab97",
					},
					"team2": {
						"c34d1891-907e-4677-bc90-458c9e94f772",
						"c34d1892-907e-4677-bc90-458c9e94f772",
						"c34d1893-907e-4677-bc90-458c9e94f772",
						"2041bad9-8ae4-418b-9e66-6af87838ab97",
					},
				},
				expectedTeamsAndMembers: map[string][]string{
					storage.AdminsTeamID: {},
					"team1": {
						"2041bad7-8ae4-418b-9e66-6af87838ab97",
						"2041bad8-8ae4-418b-9e66-6af87838ab97",
					},
					"team2": {
						"c34d1891-907e-4677-bc90-458c9e94f772",
						"c34d1892-907e-4677-bc90-458c9e94f772",
						"c34d1893-907e-4677-bc90-458c9e94f772",
					},
				},
				expectedUpdatedTeamIDs: map[string]bool{"team1": true, "team2": true},
			},
			`when there are multiple teams and the purged user
				is a member of none, neither team should be updated`: {
				userToPurge: "d989bca0-4535-444c-8300-24bec6aa446e",
				initialTeamsAndMembers: map[string][]string{
					"team1": {
						"f6d4e661-15a7-4514-b1a4-60a00becde58",
						"f6d4e662-15a7-4514-b1a4-60a00becde58",
						"f6d4e663-15a7-4514-b1a4-60a00becde58",
					},
					"team2": {
						"e7dedee5-7942-49a7-8735-f24421224f40",
						"e7dedee6-7942-49a7-8735-f24421224f40",
						"e7dedee7-7942-49a7-8735-f24421224f40",
					},
				},
				expectedTeamsAndMembers: map[string][]string{
					storage.AdminsTeamID: {},
					"team1": {
						"f6d4e661-15a7-4514-b1a4-60a00becde58",
						"f6d4e662-15a7-4514-b1a4-60a00becde58",
						"f6d4e663-15a7-4514-b1a4-60a00becde58",
					},
					"team2": {
						"e7dedee5-7942-49a7-8735-f24421224f40",
						"e7dedee6-7942-49a7-8735-f24421224f40",
						"e7dedee7-7942-49a7-8735-f24421224f40",
					},
				},
				expectedUpdatedTeamIDs: map[string]bool{},
			},
		}
		for desc, test := range tests {
			t.Run(desc, func(t *testing.T) {
				ctx := context.Background()
				var expectedResponseIds []string
				var allCreatedIds []string
				for team, members := range test.initialTeamsAndMembers {
					createReq := &teams.CreateTeamReq{
						Name: "ignored",
						Id:   team,
					}
					resp, err := cl.CreateTeam(ctx, createReq)
					require.NoError(t, err)

					addReq := &teams.AddTeamMembersReq{
						Id:      createReq.Id,
						UserIds: members,
					}
					_, err = cl.AddTeamMembers(ctx, addReq)
					require.NoError(t, err)

					allCreatedIds = append(allCreatedIds, createReq.Id)
					if _, isExpectedInResponse := test.expectedUpdatedTeamIDs[team]; isExpectedInResponse {
						expectedResponseIds = append(expectedResponseIds, resp.GetTeam().GetId())
					}
				}

				req := &teams.PurgeUserMembershipReq{
					UserId: test.userToPurge,
				}
				resp, err := cl.PurgeUserMembership(ctx, req)

				// Check that IDs of updated teams returned by API
				// match what we expected.
				require.NoError(t, err)
				require.NotNil(t, resp)
				assert.ElementsMatch(t, expectedResponseIds, resp.Ids)

				// Check that user membership was properly updated
				finalTeamsState, err := cl.ListTeams(ctx, &teams.ListTeamsReq{})
				require.NoError(t, err)
				for _, team := range finalTeamsState.GetTeams() {
					expectedTeamMembers, found := test.expectedTeamsAndMembers[team.Id]
					require.Equal(t, found, true)
					assert.NotNil(t, expectedTeamMembers)
					usersReq := &teams.GetTeamMembershipReq{
						Id: team.Id,
					}
					usersResp, err := cl.GetTeamMembership(ctx, usersReq)
					require.NoError(t, err)
					assert.ElementsMatch(t, expectedTeamMembers, usersResp.UserIds)
				}

				// Cleanup
				for _, teamID := range allCreatedIds {
					cleanupTeam(t, cl, teamID)
				}
			})
		}
	})

}

func cleanupTeam(t *testing.T, cl teams.TeamsServiceClient, id string) {
	t.Helper()

	deleteReq := teams.DeleteTeamReq{Id: id}
	_, err := cl.DeleteTeam(context.Background(), &deleteReq)
	require.NoError(t, err)
}

// Pass nil for migrationConfig if you want in-memory server.
func setupTeamsService(ctx context.Context, t *testing.T, l logger.Logger,
	migrationConfig *migration.Config) (*team_serv.TeamServer, *service.Service,
	*grpc.ClientConn, func(), *authz.PoliciesServiceServerMock) {

	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, "teams-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()

	mockPolicies := authz.NewPoliciesServiceServerMock()
	mockPolicies.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
	authz.RegisterPoliciesServiceServer(grpcAuthz, mockPolicies)

	mockAuthz := authz.NewAuthorizationServiceServerMock()
	mockAuthz.ValidateProjectAssignmentFunc = defaultValidateProjectAssignmentFunc
	authz.RegisterAuthorizationServiceServer(grpcAuthz, mockAuthz)

	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)

	authzPoliciesClient := authz.NewPoliciesServiceClient(authzConn)
	authzAuthorizationClient := authz.NewAuthorizationServiceClient(authzConn)

	var serviceRef *service.Service
	if migrationConfig == nil {
		serviceRef, err = service.NewInMemoryService(l, connFactory, authzPoliciesClient)
	} else {
		serviceRef, err = service.NewPostgresService(l, connFactory,
			*migrationConfig, authzPoliciesClient, authzAuthorizationClient)
	}
	if err != nil {
		t.Fatalf("could not create server: %s", err)
	}
	grpcServ := serviceRef.ConnFactory.NewServer(tracing.GlobalServerInterceptor())
	teamServer := team_serv.NewTeamServer(serviceRef)
	teams.RegisterTeamsServiceServer(grpcServ, teamServer)

	resetState(ctx, t, serviceRef)

	g := grpctest.NewServer(grpcServ)

	conn, err := connFactory.Dial("teams-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	return teamServer, serviceRef, conn, func() { g.Close(); authzServer.Close() }, mockPolicies
}

func resetState(ctx context.Context, t *testing.T, serviceRef *service.Service) {
	t.Helper()

	if r, ok := serviceRef.Storage.(storage.Resetter); ok {
		err := r.Reset(ctx)
		require.NoError(t, err)
	}
}

func defaultMockPurgeFunc(context.Context,
	*authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	return &authz.PurgeSubjectFromPoliciesResp{}, nil
}

func defaultValidateProjectAssignmentFunc(context.Context,
	*authz.ValidateProjectAssignmentReq) (*authz.ValidateProjectAssignmentResp, error) {
	return &authz.ValidateProjectAssignmentResp{}, nil
}

func insertProjectsIntoNewContext(projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(auth_context.NewContext(context.Background(),
		[]string{}, projects, "resource", "action"))
}
