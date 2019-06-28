package v1

import (
	"context"
	"errors"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	"github.com/chef/automate/api/external/common/version"
	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	teams "github.com/chef/automate/api/interservice/teams/v1"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/test/helpers"
	"github.com/chef/automate/lib/tracing"

	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/components/teams-service/test"
)

func TestHealthGRPC(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	_, _, conn, close, _ := setupTeamsService(ctx, t, l, nil, nil)
	defer close()

	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		require.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}

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
		runAllServerTests(ctx, t, serv, serviceRef, authzMock, teams.NewTeamsV1Client(conn), close)
	} else {
		serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx,
			t, l, migrationConfig, (*datamigration.Config)(dataMigrationConfig))
		runAllServerTests(ctx, t, serv, serviceRef, authzMock, teams.NewTeamsV1Client(conn), close)

		// If ciMode, run in-memory AND PG
		// else just run PG.
		if os.Getenv("CI") == "true, *authz.SubjectPurgeServerMock" {
			serv, serviceRef, conn, close, authzMock := setupTeamsService(ctx, t, l, nil, nil)
			runAllServerTests(ctx, t, serv, serviceRef, authzMock, teams.NewTeamsV1Client(conn), close)
		}
	}
}

func runAllServerTests(ctx context.Context,
	t *testing.T, serv *Server, serviceRef *service.Service,
	authzMock *authz.SubjectPurgeServerMock, cl teams.TeamsV1Client, close func()) {

	t.Helper()
	defer close()

	t.Run("GetVersion", func(t *testing.T) {
		resp, err := cl.GetVersion(ctx, &version.VersionInfoRequest{})
		require.NoError(t, err)
		// Version is injected via linker flags that we don't set during tests
		require.Equal(t, "unknown", resp.Version)
	})

	t.Run("CreateTeam", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when a valid team is submitted", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "Gotta Catch Em All",
				Name:        "Corgis Inc.",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			team := resp.Team
			assert.Equal(t, req.Description, team.Description)
			assert.Equal(t, req.Name, team.Name)
			assert.Equal(t, 36, len(team.Id))

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})

		t.Run("when name is missing", func(t *testing.T) {
			resp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{})
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when description is missing", func(t *testing.T) {
			resp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "Some Name"})
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the team exists", func(t *testing.T) {
			resp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "Some Name", Description: "montag"})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "Some Name", Description: "does not matter"})
			assert.Nil(t, resp2)
			grpctest.AssertCode(t, codes.AlreadyExists, err)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})
	})

	t.Run("GetTeams", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when the list is successfully returned", func(t *testing.T) {
			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "First Name", Description: "montag"})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "Other Name", Description: "does not matter"})
			require.NoError(t, err)

			list, err := cl.GetTeams(ctx, &teams.GetTeamsReq{})
			require.NoError(t, err)
			require.NotNil(t, list)
			assert.Contains(t, list.Teams, resp1.Team)
			assert.Contains(t, list.Teams, resp2.Team)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(list.Teams))

			// Cleanup
			cleanupTeam(ctx, t, cl, resp1.Team.Id)
			cleanupTeam(ctx, t, cl, resp2.Team.Id)
		})

		t.Run("when there is only the non-deletable teams", func(t *testing.T) {
			resp, err := cl.GetTeams(ctx, &teams.GetTeamsReq{})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, len(storage.NonDeletableTeams), len(resp.Teams))
		})
	})

	t.Run("GetTeamByName", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when quering for the admins team (which exists by default)", func(t *testing.T) {
			resp, err := cl.GetTeamByName(ctx, &teams.GetTeamByNameReq{
				Name: storage.AdminsTeamName,
			})
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, storage.AdminsTeamName, resp.Team.Name)
		})

		t.Run("when querying for a non-default team", func(t *testing.T) {
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Name:        "other_team",
				Description: "i can be the very best...",
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			resp, err := cl.GetTeamByName(ctx, &teams.GetTeamByNameReq{
				Name: initResp.Team.Name,
			})

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, initResp.Team.Name, resp.Team.Name)
			assert.Equal(t, initResp.Team.Description, resp.Team.Description)
			assert.Equal(t, initResp.Team.Id, resp.Team.Id)
		})

		t.Run("when the team requested does not exist", func(t *testing.T) {
			resp, err := cl.GetTeamByName(ctx, &teams.GetTeamByNameReq{
				Name: "_super_not_found_",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when there is no name in the request", func(t *testing.T) {
			resp, err := cl.GetTeamByName(ctx, &teams.GetTeamByNameReq{})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	})

	t.Run("GetTeam", func(t *testing.T) {
		resetState(ctx, t, serviceRef)
		t.Run("when there is no id in the request", func(t *testing.T) {
			resp, err := cl.GetTeam(ctx, &teams.GetTeamReq{
				Id: "",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the team does not exist", func(t *testing.T) {
			resp, err := cl.GetTeam(ctx, &teams.GetTeamReq{
				Id: "d111c32d-f441-4b22-a133-b4d7e80b7e23",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when the team exists", func(t *testing.T) {
			initResp, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{
				Name:        "other_team",
				Description: "i can be the very best...",
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			resp, err := cl.GetTeam(ctx, &teams.GetTeamReq{
				Id: initResp.Team.Id,
			})

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, initResp.Team.Name, resp.Team.Name)
			assert.Equal(t, initResp.Team.Description, resp.Team.Description)
			assert.Equal(t, initResp.Team.Id, resp.Team.Id)

			cleanupTeam(ctx, t, cl, initResp.Team.Id)
		})
	})

	t.Run("GetTeamsForUser", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when valid user id provided, returns array of teams", func(t *testing.T) {
			// create first team
			req := &teams.CreateTeamReq{
				Description: "daring, nerve, & chivalry",
				Name:        "Gryffindor",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			// create second team
			req2 := &teams.CreateTeamReq{
				Description: "save the wizarding world",
				Name:        "Aurors",
			}
			resp2, err := cl.CreateTeam(ctx, req2)
			require.NoError(t, err)

			// add user to first team
			users := []string{"ab725430-b6d1-4385-a255-7899d0c326a3"}
			addReq := &teams.AddUsersReq{
				Id:      resp.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddUsers(ctx, addReq)
			require.NoError(t, err)

			// add user to second team
			addReq2 := &teams.AddUsersReq{
				Id:      resp2.GetTeam().GetId(),
				UserIds: users,
			}
			_, err = cl.AddUsers(ctx, addReq2)
			require.NoError(t, err)

			// get user's teams
			listReq := &teams.GetTeamsForUserReq{
				UserId: users[0],
			}

			fetchedData, err := cl.GetTeamsForUser(ctx, listReq)

			require.NoError(t, err)
			require.NotNil(t, fetchedData)
			assert.Equal(t, 2, len(fetchedData.Teams))
			fetchedTeamIDs := []string{fetchedData.Teams[0].Id, fetchedData.Teams[1].Id}
			assert.Contains(t, fetchedTeamIDs, resp.Team.Id)
			assert.Contains(t, fetchedTeamIDs, resp2.Team.Id)

			// Cleanup
			firstTeamID := teams.DeleteTeamReq{Id: resp.Team.Id}
			_, err = cl.DeleteTeam(ctx, &firstTeamID)
			require.NoError(t, err)
			secondTeamID := teams.DeleteTeamReq{Id: resp2.Team.Id}
			_, err = cl.DeleteTeam(ctx, &secondTeamID)
			require.NoError(t, err)
		})

		t.Run("when user id does not exist on any teams, returns empty array", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "cunning & ambitious",
				Name:        "Slytherin",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			listReq := &teams.GetTeamsForUserReq{
				UserId: "e7a449b0-eea7-44c5-be65-227edb267588",
			}

			fetchedData, err := cl.GetTeamsForUser(ctx, listReq)

			require.NoError(t, err)
			require.NotNil(t, fetchedData)
			assert.Empty(t, fetchedData.Teams)

			teamID := teams.DeleteTeamReq{Id: resp.Team.Id}
			_, err = cl.DeleteTeam(ctx, &teamID)
			assert.Nil(t, err)
		})
	})

	t.Run("DeleteTeam", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when an existing team is deleted", func(t *testing.T) {
			teamToDeleteName := "First Name"

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "team:local:"+teamToDeleteName {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected team name passed to PurgeSubjectFromPolicies")
			}

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: teamToDeleteName, Description: "montag"})
			require.NoError(t, err)
			require.NotNil(t, resp1)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "Other Name", Description: "does not matter"})
			require.NoError(t, err)
			require.NotNil(t, resp2)

			teamListBefore, err := cl.GetTeams(ctx, &teams.GetTeamsReq{})
			require.NoError(t, err)
			assert.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp1.Team.Id})
			require.NoError(t, err2)
			require.NotNil(t, resp)
			assert.Equal(t, resp1.Team.Description, resp.Team.Description)
			assert.Equal(t, resp1.Team.Name, resp.Team.Name)

			teamListAfter, err3 := cl.GetTeams(ctx, &teams.GetTeamsReq{})
			require.NoError(t, err3)
			assert.Equal(t, len(storage.NonDeletableTeams), len(teamListAfter.Teams)-1)
			assert.Contains(t, teamListAfter.Teams, resp2.Team)

			// Cleanup
			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
			cleanupTeam(ctx, t, cl, resp2.Team.Id)
		})

		t.Run("when an existing team is deleted but the deletion of their policy membership fails", func(t *testing.T) {
			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				return nil, errors.New("test failure of PurgeSubjectFromPolicies")
			}

			resp1, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "First Name", Description: "montag"})
			require.NoError(t, err)
			resp2, err := cl.CreateTeam(ctx, &teams.CreateTeamReq{Name: "Other Name", Description: "does not matter"})
			require.NoError(t, err)

			teamListBefore, err := cl.GetTeams(ctx, &teams.GetTeamsReq{})
			require.NoError(t, err)
			require.Equal(t, 2+len(storage.NonDeletableTeams), len(teamListBefore.Teams))

			resp, err2 := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: resp1.Team.Id})
			require.Nil(t, resp)
			require.NotNil(t, err2)
			grpctest.AssertCode(t, codes.Internal, err2)

			teamListAfter, err3 := cl.GetTeams(ctx, &teams.GetTeamsReq{})
			require.NoError(t, err3)
			assert.Equal(t, 1+len(storage.NonDeletableTeams), len(teamListAfter.Teams))
			assert.Contains(t, teamListAfter.Teams, resp2.Team)

			// Cleanup
			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
			cleanupTeam(ctx, t, cl, resp2.Team.Id)
		})

		t.Run("when the team to delete is not found", func(t *testing.T) {
			resp, err := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: "38a63837-96d9-42a1-bd4b-5a8f866c39b3"})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when the team id for the team to delete is not valid", func(t *testing.T) {
			resp, err := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: ""})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when attempting to delete a team that is not allowed to be deleted", func(t *testing.T) {
			getTeamResp, err := cl.GetTeamByName(ctx, &teams.GetTeamByNameReq{Name: storage.AdminsTeamName})
			require.NoError(t, err)
			require.NotNil(t, getTeamResp)
			resp, err := cl.DeleteTeam(ctx, &teams.DeleteTeamReq{Id: getTeamResp.Team.Id})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	})

	t.Run("UpdateTeam", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when a valid team update request is submitted", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "Gotta Catch Em All",
				Name:        "Corgis Inc.",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			newDescr, newName := "Gotta Catch Only The Most Special", "Corgis Inc."
			updateReq := &teams.UpdateTeamReq{
				Id:          resp.Team.Id,
				Description: newDescr,
				Name:        newName,
			}

			updatedTeamResp, err := cl.UpdateTeam(ctx, updateReq)
			require.NoError(t, err, "update team")
			require.NotNil(t, resp)
			assert.Equal(t, updateReq.Description, updatedTeamResp.Team.Description)
			assert.Equal(t, updateReq.Name, updatedTeamResp.Team.Name)
			assert.Equal(t, resp.Team.Id, updatedTeamResp.Team.Id)

			teamsList, err := cl.GetTeams(ctx, &teams.GetTeamsReq{})
			require.NoError(t, err, "reading back teams")

			require.Equal(t, 2, len(teamsList.Teams))
			var updatedTeam *teams.Team
			if teamsList.Teams[0].Name != storage.AdminsTeamName {
				updatedTeam = teamsList.Teams[0]
			} else {
				updatedTeam = teamsList.Teams[1]
			}

			assert.Equal(t, newDescr, updatedTeam.Description)
			assert.Equal(t, newName, updatedTeam.Name)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})

		t.Run("when name is missing", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "Gotta Catch Em All",
				Name:        "Corgis Inc.",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			updateReq := &teams.UpdateTeamReq{
				Id:          resp.Team.Id,
				Description: "Gotta Catch Only The Most Special",
				Name:        "",
			}
			updatedTeam, err := cl.UpdateTeam(ctx, updateReq)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})

		t.Run("when description is missing", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "Gotta Catch Em All",
				Name:        "Corgis Inc.",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			updateReq := &teams.UpdateTeamReq{
				Id:          resp.Team.Id,
				Description: "",
				Name:        "Corgis Inc.",
			}
			updatedTeam, err := cl.UpdateTeam(ctx, updateReq)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})

		t.Run("when id and description are missing", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "Gotta Catch Em All",
				Name:        "Corgis Inc.",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			updateReq := &teams.UpdateTeamReq{
				Id:          "",
				Description: "",
				Name:        "Corgis Lol.",
			}
			updatedTeam, err := cl.UpdateTeam(ctx, updateReq)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.InvalidArgument, err)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})

		t.Run("when updated team name already exists", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "Gotta Catch Em All",
				Name:        "Corgis Inc.",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			req = &teams.CreateTeamReq{
				Description: "Gotta Catch Em All",
				Name:        "Dogs Inc.",
			}
			resp2, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			updateReq := &teams.UpdateTeamReq{
				Id:          resp.GetTeam().GetId(),
				Description: "Gotta Catch Em All",
				Name:        "Dogs Inc.",
			}
			updatedTeam, err := cl.UpdateTeam(ctx, updateReq)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.AlreadyExists, err)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
			cleanupTeam(ctx, t, cl, resp2.Team.Id)
		})

		t.Run("when team to update does not exist", func(t *testing.T) {
			updateReq := &teams.UpdateTeamReq{
				Id:          "e089a53f-3dc8-487d-a2af-f90c76f42949",
				Description: "Gotta Catch Em All",
				Name:        "Corgis Inc.",
			}
			updatedTeam, err := cl.UpdateTeam(ctx, updateReq)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.NotFound, err)
		})
	})

	t.Run("AddUsers", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

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

					// arrange
					req := &teams.CreateTeamReq{
						Description: "Gotta Catch Em All",
						Name:        "Corgis Inc.",
					}
					resp, err := cl.CreateTeam(ctx, req)
					require.NoError(t, err)

					addReq := &teams.AddUsersReq{
						Id:      resp.GetTeam().GetId(),
						UserIds: test.users,
					}

					// act
					resp2, err := cl.AddUsers(ctx, addReq)

					// assert
					require.NoError(t, err)
					require.NotNil(t, resp2)
					assert.Equal(t, len(addReq.UserIds), len(test.users))
					assert.ElementsMatch(t, addReq.UserIds, test.users)

					// Cleanup
					cleanupTeam(ctx, t, cl, resp.Team.Id)
				})
			}
		})

		t.Run("when team exists and user has already been added, does not add duplicate user", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "with, learning, & wisdom",
				Name:        "Ravenclaw",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			users := []string{"5733dd17-f593-4c31-9dea-41e3521878d9"}
			addReq := &teams.AddUsersReq{
				Id:      resp.GetTeam().GetId(),
				UserIds: users,
			}

			// add user first time
			_, err = cl.AddUsers(ctx, addReq)
			require.NoError(t, err, "first add")

			// attempt to add user second time
			updatedTeam, err := cl.AddUsers(ctx, addReq)
			require.NoError(t, err, "second add")
			require.NotNil(t, updatedTeam)

			usersReq := &teams.GetUsersReq{
				Id: resp.GetTeam().GetId(),
			}
			usersResp, err := cl.GetUsers(ctx, usersReq)
			require.NoError(t, err)
			assert.ElementsMatch(t, users, usersResp.UserIds)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})

		t.Run("when team does not exist, returns Not Found error", func(t *testing.T) {
			users := []string{"ea1e54ea-6dfe-44a8-a5c6-30d1398254db"}
			addReq := &teams.AddUsersReq{
				Id:      "b6325ef9-22c9-4d18-90ae-b2782735a2ef",
				UserIds: users,
			}

			updatedTeam, err := cl.AddUsers(ctx, addReq)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when no user ids provided, fails with invalid arg", func(t *testing.T) {
			req := &teams.CreateTeamReq{
				Description: "Guard the galaxy",
				Name:        "Guardians of the Galaxy",
			}
			resp, err := cl.CreateTeam(ctx, req)
			require.NoError(t, err)

			addReq := &teams.AddUsersReq{
				Id:      resp.GetTeam().GetId(),
				UserIds: []string{},
			}

			updatedTeam, err := cl.AddUsers(ctx, addReq)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, updatedTeam)

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})
	})

	t.Run("RemoveUsers", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when team id is not passed, returns InvalidArgument error", func(t *testing.T) {
			req := &teams.RemoveUsersReq{
				Id:      "",
				UserIds: []string{"3lk7b810-9dad-11d1-80b4-00c04fd430c8"},
			}

			updatedTeam, err := cl.RemoveUsers(ctx, req)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when team does not exist, returns NotFound error", func(t *testing.T) {
			req := &teams.RemoveUsersReq{
				Id:      "5481fe86-6eb0-415b-9903-12411a8e0fab",
				UserIds: []string{"842e11cc-92a0-4562-a8de-104664675504"},
			}

			updatedTeam, err := cl.RemoveUsers(ctx, req)

			require.Nil(t, updatedTeam)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when team exists without users the list remains empty", func(t *testing.T) {
			createReq := &teams.CreateTeamReq{
				Description: "Guard the galaxy",
				Name:        "Guardians of the Galaxy",
			}
			resp, err := cl.CreateTeam(ctx, createReq)
			require.NoError(t, err)

			req := &teams.RemoveUsersReq{
				Id: resp.Team.Id,
				UserIds: []string{
					"4fbc13ec-3205-4f04-a663-1a104848eadf",
					"4fbc13ed-3205-4f04-a663-1a104848eadf",
				},
			}
			_, err = cl.RemoveUsers(ctx, req)
			require.NoError(t, err)
			usersReq := &teams.GetUsersReq{
				Id: resp.GetTeam().GetId(),
			}
			usersResp, err := cl.GetUsers(ctx, usersReq)
			require.NoError(t, err)
			assert.Equal(t, 0, len(usersResp.UserIds))

			// Cleanup
			cleanupTeam(ctx, t, cl, resp.Team.Id)
		})

		tests := map[string]struct {
			usersToStart            []string
			usersToRemove           []string
			expectedLengthRemaining int
		}{
			"with the same set of users as to delete, the list becomes empty": {
				usersToStart: []string{
					"e76ecf48-c3de-4f5f-b9d6-b21c0ca0d696",
					"e76ecf49-c3de-4f5f-b9d6-b21c0ca0d696",
				},
				usersToRemove: []string{
					"e76ecf48-c3de-4f5f-b9d6-b21c0ca0d696",
					"e76ecf49-c3de-4f5f-b9d6-b21c0ca0d696",
				},
				expectedLengthRemaining: 0,
			},
			"with intersecting users existing and to remove, the list is updated": {
				usersToStart: []string{
					"e76ecf47-c3de-4f5f-b9d6-b21c0ca0d696",
					"e76ecf48-c3de-4f5f-b9d6-b21c0ca0d696",
					"e76ecf49-c3de-4f5f-b9d6-b21c0ca0d696",
				},
				usersToRemove: []string{
					"e76ecf47-c3de-4f5f-b9d6-b21c0ca0d696",
					"e76ecf48-c3de-4f5f-b9d6-b21c0ca0d696",
				},
				expectedLengthRemaining: 1,
			},
			"with users, but an empty user list is passed": {
				usersToStart: []string{
					"e76ecf47-c3de-4f5f-b9d6-b21c0ca0d696",
					"e76ecf48-c3de-4f5f-b9d6-b21c0ca0d696",
					"e76ecf49-c3de-4f5f-b9d6-b21c0ca0d696",
				},
				usersToRemove:           []string{},
				expectedLengthRemaining: 3,
			},
		}
		for desc, test := range tests {
			t.Run("when team exists "+desc, func(t *testing.T) {
				createReq := &teams.CreateTeamReq{
					Description: "Guard the galaxy",
					Name:        "Guardians of the Galaxy",
				}
				resp, err := cl.CreateTeam(ctx, createReq)
				require.NoError(t, err)
				addReq := &teams.AddUsersReq{
					Id:      resp.GetTeam().GetId(),
					UserIds: test.usersToStart,
				}
				_, err = cl.AddUsers(ctx, addReq)
				require.NoError(t, err)

				req := &teams.RemoveUsersReq{
					Id:      resp.Team.Id,
					UserIds: test.usersToRemove,
				}
				_, err = cl.RemoveUsers(ctx, req)
				require.NoError(t, err)

				usersReq := &teams.GetUsersReq{
					Id: resp.GetTeam().GetId(),
				}
				usersResp, err := cl.GetUsers(ctx, usersReq)
				require.NoError(t, err)
				assert.Equal(t, test.expectedLengthRemaining, len(usersResp.UserIds))

				// Cleanup
				cleanupTeam(ctx, t, cl, resp.Team.Id)
			})
		}
	})

	t.Run("PurgeUserMembership", func(t *testing.T) {
		resetState(ctx, t, serviceRef)

		t.Run("when user id is not passed, returns InvalidArgument error", func(t *testing.T) {
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
					storage.AdminsTeamName: {},
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
					storage.AdminsTeamName: {},
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
					storage.AdminsTeamName: {},
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
					storage.AdminsTeamName: {},
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
					storage.AdminsTeamName: {},
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
				var expectedResponseIds []string
				var allCreatedIds []string
				for team, members := range test.initialTeamsAndMembers {
					createReq := &teams.CreateTeamReq{
						Description: "ignored",
						Name:        team,
					}
					resp, err := cl.CreateTeam(ctx, createReq)
					require.NoError(t, err)

					addReq := &teams.AddUsersReq{
						Id:      resp.GetTeam().GetId(),
						UserIds: members,
					}
					_, err = cl.AddUsers(ctx, addReq)
					require.NoError(t, err)

					allCreatedIds = append(allCreatedIds, resp.GetTeam().GetId())
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
				finalTeamsState, err := cl.GetTeams(ctx, &teams.GetTeamsReq{})
				require.NoError(t, err)
				for _, team := range finalTeamsState.GetTeams() {
					expectedTeamMembers, found := test.expectedTeamsAndMembers[team.Name]
					require.Equal(t, found, true)
					assert.NotNil(t, expectedTeamMembers)
					usersReq := &teams.GetUsersReq{
						Id: team.Id,
					}
					usersResp, err := cl.GetUsers(ctx, usersReq)
					require.NoError(t, err)
					assert.ElementsMatch(t, expectedTeamMembers, usersResp.UserIds)
				}

				// Cleanup
				for _, teamID := range allCreatedIds {
					cleanupTeam(ctx, t, cl, teamID)
				}
			})
		}
	})
}

func cleanupTeam(ctx context.Context, t *testing.T, cl teams.TeamsV1Client, teamID string) {
	t.Helper()
	deleteReq := teams.DeleteTeamReq{Id: teamID}
	_, err := cl.DeleteTeam(ctx, &deleteReq)
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
	v1Server := NewServer(serviceRef)
	teams.RegisterTeamsV1Server(grpcServ, v1Server)
	health.RegisterHealthServer(grpcServ, health.NewService())

	resetState(ctx, t, serviceRef)

	g := grpctest.NewServer(grpcServ)

	conn, err := connFactory.Dial("teams-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	return v1Server, serviceRef, conn, func() { g.Close(); authzServer.Close() }, mockCommon
}

func resetState(ctx context.Context, t *testing.T, serv *service.Service) {
	t.Helper()

	if r, ok := serv.Storage.(storage.Resetter); ok {
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
			Major: authz_v2.Version_V1,
			Minor: authz_v2.Version_V0,
		},
	}, nil
}
