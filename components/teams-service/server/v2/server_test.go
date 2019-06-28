package v2

import (
	"context"
	"errors"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	teams "github.com/chef/automate/api/interservice/teams/v2"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/test/helpers"
	"github.com/chef/automate/lib/tracing"

	"github.com/chef/automate/components/teams-service/constants"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/components/teams-service/test"
)

func TestTeamsGRPC(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../../storage/postgres/migration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	dataMigrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../../storage/postgres/datamigration/sql")
	if err != nil {
		t.Fatalf("couldn't initialize pg data config for tests: %s", err.Error())
	}

	if migrationConfig == nil {
		serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx, t, l, nil, nil)
		runAllServerTests(t, serv, serviceRef, authzMock, teams.NewTeamsV2Client(conn), close)
	} else {
		serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx,
			t, l, migrationConfig, (*datamigration.Config)(dataMigrationConfig))
		runAllServerTests(t, serv, serviceRef, authzMock, teams.NewTeamsV2Client(conn), close)

		// If ciMode, run in-memory AND PG
		// else just run PG.
		if os.Getenv("CI") == "true, *authz.SubjectPurgeServerMock" {
			serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx, t, l, nil, nil)
			runAllServerTests(t, serv, serviceRef, authzMock, teams.NewTeamsV2Client(conn), close)
		}
	}
}

func runAllServerTests(
	t *testing.T, serv *Server, serviceRef *service.Service,
	authzMock *authz.SubjectPurgeServerMock, cl teams.TeamsV2Client, close func()) {

	t.Helper()
	defer close()

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
				Id: storage.AdminsTeamName,
			})

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, storage.AdminsTeamName, resp.Team.Id)
			assert.Equal(t, "Members of the admins team, by default, have access to all parts of the API.",
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

			cleanupTeamV2(t, cl, initResp.Team.Id)
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

			cleanupTeamV2(t, cl, resp1.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
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

			cleanupTeamV2(t, cl, resp1.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
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

			cleanupTeamV2(t, cl, resp1.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
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

			cleanupTeamV2(t, cl, resp1.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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
			cleanupTeamV2(t, cl, resp2.Team.Id)
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
			cleanupTeamV2(t, cl, resp2.Team.Id)
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
			cleanupTeamV2(t, cl, resp2.Team.Id)
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
			cleanupTeamV2(t, cl, resp2.Team.Id)
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

			cleanupTeamV2(t, cl, resp1.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
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
			cleanupTeamV2(t, cl, resp2.Team.Id)
		})

		t.Run("when the team to delete is not found", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: "some-wrong-id"})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when attempting to delete a team that is not allowed to be deleted", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: storage.AdminsTeamName})

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
			if teamsList.Teams[0].Id != storage.AdminsTeamName {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeamV2(t, cl, resp.Team.Id)
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
			if teamsList.Teams[0].Id != storage.AdminsTeamName {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeamV2(t, cl, resp.Team.Id)
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
			if teamsList.Teams[0].Id != storage.AdminsTeamName {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeamV2(t, cl, resp.Team.Id)
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
			if teamsList.Teams[0].Id != storage.AdminsTeamName {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}
			assert.Equal(t, newName, updatedTeam.Name)

			cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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
						Id:      resp.GetTeam().GetId(),
						UserIds: test.users,
					}

					// act
					resp2, err := cl.AddTeamMembers(ctx, addReq)

					// assert
					require.NoError(t, err)
					require.NotNil(t, resp2)
					assert.Equal(t, len(addReq.UserIds), len(resp2.UserIds))
					assert.ElementsMatch(t, addReq.UserIds, resp2.UserIds)

					cleanupTeamV2(t, cl, resp.Team.Id)
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

					cleanupTeamV2(t, cl, resp.Team.Id)
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

					cleanupTeamV2(t, cl, resp.Team.Id)
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

					cleanupTeamV2(t, cl, resp.Team.Id)
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

					cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, createTeam.Team.Id)
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

			cleanupTeamV2(t, cl, createTeam.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
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

				cleanupTeamV2(t, cl, resp.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
			cleanupTeamV2(t, cl, resp3.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
			cleanupTeamV2(t, cl, resp3.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
			cleanupTeamV2(t, cl, resp2.Team.Id)
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

			cleanupTeamV2(t, cl, resp.Team.Id)
		})
	})

	t.Run("GetTeamMembership", func(t *testing.T) {
		resetState(context.Background(), t, serviceRef)

		t.Run("when the team does not exist", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetTeamMembership(ctx, &teams.GetTeamMembershipReq{
				Id: "test-team",
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

			cleanupTeamV2(t, cl, initResp.Team.Id)
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

			cleanupTeamV2(t, cl, initResp.Team.Id)
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

			cleanupTeamV2(t, cl, initResp.Team.Id)
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

			cleanupTeamV2(t, cl, initResp.Team.Id)
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

			cleanupTeamV2(t, cl, initResp.Team.Id)
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

			cleanupTeamV2(t, cl, initResp.Team.Id)
		})
	})
}

func cleanupTeam(ctx context.Context, t *testing.T, cl teams.TeamsV2Client, teamID string) {
	t.Helper()
	deleteReq := teams.DeleteTeamReq{Id: teamID}
	_, err := cl.DeleteTeam(ctx, &deleteReq)
	require.NoError(t, err)
}

func cleanupTeamV2(t *testing.T, cl teams.TeamsV2Client, teamName string) {
	t.Helper()

	deleteReq := teams.DeleteTeamReq{Id: teamName}
	_, err := cl.DeleteTeam(context.Background(), &deleteReq)
	require.NoError(t, err)
}

// Pass nil for migrationConfig if you want in-memory server.
func setupTeamsService(ctx context.Context,
	t *testing.T,
	l logger.Logger,
	migrationConfig *migration.Config,
	dataMigrationConfig *datamigration.Config) (*Server, *service.Service, *grpc.ClientConn, func(), *authz.SubjectPurgeServerMock) {

	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, "teams-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()

	mockCommon := authz.NewSubjectPurgeServerMock()
	mockCommon.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
	authz.RegisterSubjectPurgeServer(grpcAuthz, mockCommon)

	mockV2 := authz_v2.NewPoliciesServerMock()
	mockV2.GetPolicyVersionFunc = defaultGetPolicyVersionFunc
	authz_v2.RegisterPoliciesServer(grpcAuthz, mockV2)

	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)

	authzClient := authz.NewSubjectPurgeClient(authzConn)
	authzV2Client := authz_v2.NewPoliciesClient(authzConn)

	var serviceRef *service.Service
	if migrationConfig == nil {
		serviceRef, err = service.NewInMemoryService(l, connFactory, authzClient)
	} else {
		serviceRef, err = service.NewPostgresService(l, connFactory,
			*migrationConfig, *dataMigrationConfig, authzClient, authzV2Client)
	}
	if err != nil {
		t.Fatalf("could not create server: %s", err)
	}
	grpcServ := serviceRef.ConnFactory.NewServer(tracing.GlobalServerInterceptor())
	v2Server := NewServer(serviceRef)
	teams.RegisterTeamsV2Server(grpcServ, v2Server)

	resetState(ctx, t, serviceRef)

	g := grpctest.NewServer(grpcServ)

	conn, err := connFactory.Dial("teams-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	return v2Server, serviceRef, conn, func() { g.Close(); authzServer.Close() }, mockCommon
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

func defaultGetPolicyVersionFunc(context.Context,
	*authz_v2.GetPolicyVersionReq) (*authz_v2.GetPolicyVersionResp, error) {
	return &authz_v2.GetPolicyVersionResp{
		Version: &authz_v2.Version{
			Major: authz_v2.Version_V2,
			Minor: authz_v2.Version_V0,
		},
	}, nil
}

func insertProjectsIntoNewContext(projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(auth_context.NewContext(context.Background(),
		[]string{}, projects, "resource", "action", "pol"))
}
