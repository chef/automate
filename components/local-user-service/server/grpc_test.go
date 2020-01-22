package server

import (
	"context"
	"testing"

	"go.uber.org/zap"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	authz "github.com/chef/automate/api/interservice/authz/common"
	api "github.com/chef/automate/api/interservice/local_user"
	teams_api "github.com/chef/automate/api/interservice/teams/v1"
	"github.com/chef/automate/components/local-user-service/users"
	usersMock "github.com/chef/automate/components/local-user-service/users/mock"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

var logger *zap.Logger

func init() {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.DebugLevel)
	logger, _ = cfg.Build()
}

func TestHealthGRPC(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	serviceCerts := helpers.LoadDevCerts(t, "local-user-service")
	connFactory := secureconn.NewFactory(*serviceCerts)
	config := Config{
		Logger:       logger,
		Users:        &usersMock.Config{},
		ServiceCerts: serviceCerts,
	}

	serv, err := NewServer(ctx, config)
	if err != nil {
		t.Fatalf("opening connector: %s", err)
	}

	g := grpctest.NewServer(serv.NewGRPCServer())
	defer g.Close()

	conn, err := connFactory.Dial("local-user-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}

func TestUsersGRPC(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	mockUsers := &usersMock.Config{Users: map[string]users.ShowUser{
		"alice@email.com": {
			ID:    "alice",
			Name:  "Alice",
			Email: "alice@email.com",
		},
		"bob@bobmail.com": {
			ID:    "bob",
			Name:  "Bob",
			Email: "bob@bobmail.com",
		},
		"ada@email.com": {
			ID:    "ada",
			Name:  "Ada",
			Email: "ada@email.com",
		},
	},
		Validate: func(email string) bool {
			return email == "alice@email.com" // only alice's previous password will be valid
		},
	}

	teamsServer, mockTeams := newTeamsMock(t)
	defer teamsServer.Close()

	authzServer, mockAuthz := newAuthzMock(t)
	defer authzServer.Close()

	serviceCerts := helpers.LoadDevCerts(t, "local-user-service")
	connFactory := secureconn.NewFactory(*serviceCerts)
	config := Config{
		Logger:       logger,
		Users:        mockUsers,
		ServiceCerts: serviceCerts,
		TeamsAddress: teamsServer.URL,
		AuthzAddress: authzServer.URL,
	}

	serv, err := NewServer(ctx, config)
	if err != nil {
		t.Fatalf("opening connector: %s", err)
	}

	g := grpctest.NewServer(serv.NewGRPCServer())
	defer g.Close()

	conn, err := connFactory.Dial("local-user-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	cl := api.NewUsersMgmtClient(conn)

	t.Run("GetUsers", func(t *testing.T) {
		resp, err := cl.GetUsers(ctx, &api.GetUsersReq{})
		if assert.Nil(t, err) {
			assert.Equal(t, 3, len(resp.Users))
			if assert.NotNil(t, resp.Users["bob@bobmail.com"]) {
				assert.Equal(t, "Bob", resp.Users["bob@bobmail.com"].Name)
			}
		}
	})

	t.Run("GetUser", func(t *testing.T) {
		t.Run("when user is found", func(t *testing.T) {
			userEmail := api.Email{Email: "alice@email.com"}
			user, err := cl.GetUser(ctx, &userEmail)
			if assert.Nil(t, err) {
				assert.Equal(t, "alice", user.Id)
			}
		})

		t.Run("when user is not found", func(t *testing.T) {
			userEmail := api.Email{Email: "wrong@email.com"}
			resp, err := cl.GetUser(ctx, &userEmail)
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		})
	})

	t.Run("CreateUser", func(t *testing.T) {
		t.Run("when new user created successfully", func(t *testing.T) {
			req := api.CreateUserReq{
				Id:       "",
				Name:     "testUser",
				Email:    "test@test.test",
				Password: "testing123",
			}

			us, err := cl.CreateUser(ctx, &req)
			if assert.Nil(t, err) {
				assert.Equal(t, us.Email, req.Email)
				assert.Equal(t, us.Name, req.Name)
			}
		})

		t.Run("when user to be created already exists", func(t *testing.T) {
			req := api.CreateUserReq{
				Id:       "alice",
				Name:     "Alice",
				Email:    "alice@email.com",
				Password: "topsecret",
			}

			resp, err := cl.CreateUser(ctx, &req)
			grpctest.AssertCode(t, codes.AlreadyExists, err)
			assert.Nil(t, resp)
		})

		t.Run("when user to be created has a too-simple password", func(t *testing.T) {
			req := api.CreateUserReq{
				Id:       "bob",
				Name:     "Bob",
				Email:    "bob@email.com",
				Password: "top",
			}

			resp, err := cl.CreateUser(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})
	})

	t.Run("UpdateUser", func(t *testing.T) {
		t.Run("when user is updated successfully without the password", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:    "alice",
				Name:  "NotAlice",
				Email: "alice@email.com",
			}

			getUserResp, err := cl.GetUser(ctx, &api.Email{Email: req.Email})
			require.NoError(t, err)
			assert.NotEqual(t, getUserResp.Name, req.Name)

			us, err := cl.UpdateUser(ctx, &req)
			require.NoError(t, err)
			assert.Equal(t, us.Name, req.Name)
		})

		t.Run("when user is updated successfully", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:       "alice",
				Name:     "ChangeAgain",
				Email:    "alice@email.com",
				Password: "topsecretnew",
			}

			getUserResp, err := cl.GetUser(ctx, &api.Email{Email: req.Email})
			require.NoError(t, err)
			assert.NotEqual(t, getUserResp.Name, req.Name)

			us, err := cl.UpdateUser(ctx, &req)
			if assert.Nil(t, err) {
				assert.Equal(t, us.Name, req.Name)
			}
		})

		t.Run("when user is not found", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:    "nope",
				Name:  "nada",
				Email: "notfound@whereami.org",
			}

			resp, err := cl.UpdateUser(ctx, &req)
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		})

		t.Run("when the password is too short", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:       "nope",
				Name:     "nada",
				Email:    "notfound@whereami.org",
				Password: "nada",
			}

			resp, err := cl.UpdateUser(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when name is empty", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:       "nope",
				Email:    "notfound@whereami.org",
				Password: "topsecretnew",
			}

			resp, err := cl.UpdateUser(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when name is whitespace", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:       "nope",
				Name:     "    ",
				Email:    "notfound@whereami.org",
				Password: "nada",
			}

			resp, err := cl.UpdateUser(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})
	})

	t.Run("UpdateSelf", func(t *testing.T) {
		t.Run("when user is updated successfully without the password", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:    "alice",
				Name:  "NotAlice",
				Email: "alice@email.com",
			}

			getUserResp, err := cl.GetUser(ctx, &api.Email{Email: req.Email})
			require.NoError(t, err)
			assert.NotEqual(t, getUserResp.Name, req.Name)

			us, err := cl.UpdateSelf(ctx, &req)
			require.NoError(t, err)
			assert.Equal(t, us.Name, req.Name)
		})

		t.Run("when user is updated successfully", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:               "alice",
				Name:             "ChangeAgain",
				Email:            "alice@email.com",
				Password:         "topsecretnew",
				PreviousPassword: "topsecretold", // alice's previous pass is always OK
			}

			getUserResp, err := cl.GetUser(ctx, &api.Email{Email: req.Email})
			require.NoError(t, err)
			assert.NotEqual(t, getUserResp.Name, req.Name)

			us, err := cl.UpdateSelf(ctx, &req)
			require.NoError(t, err)
			assert.Equal(t, us.Name, req.Name)
		})

		t.Run("when user wants to update their password but do not pass the correct previous password", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:               "bob",
				Name:             "ChangeAgain",
				Email:            "bob@email.com",
				Password:         "topsecretnew",
				PreviousPassword: "topsecretold",
			}

			resp, err := cl.UpdateSelf(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when user wants to update their password but does not pass a previous password", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:       "alice",
				Name:     "ChangeAgain",
				Email:    "alice@email.com",
				Password: "topsecretnew",
			}

			resp, err := cl.UpdateSelf(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when user wants to update their password but the new password is too short", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:               "alice",
				Name:             "ChangeAgain",
				Email:            "alice@email.com",
				PreviousPassword: "topsecretnew",
				Password:         "nada",
			}

			resp, err := cl.UpdateSelf(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when user wants to update their password but the new password all whitespace", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:               "alice",
				Name:             "ChangeAgain",
				Email:            "alice@email.com",
				PreviousPassword: "topsecretnew",
				Password:         "                  ",
			}

			resp, err := cl.UpdateSelf(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when user is not found", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:    "nope",
				Name:  "nada",
				Email: "notfound@whereami.org",
			}

			resp, err := cl.UpdateSelf(ctx, &req)
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		})

		t.Run("when the password is too short", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:       "nope",
				Name:     "nada",
				Email:    "notfound@whereami.org",
				Password: "nada",
			}

			resp, err := cl.UpdateSelf(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when the update password is too short", func(t *testing.T) {
			req := api.UpdateSelfReq{
				Id:               "nope",
				Name:             "nada",
				Email:            "notfound@whereami.org",
				PreviousPassword: "asdf",
				Password:         "topsecretnew",
			}

			resp, err := cl.UpdateSelf(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when name is empty", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:       "nope",
				Email:    "notfound@whereami.org",
				Password: "topsecretnew",
			}

			resp, err := cl.UpdateUser(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})

		t.Run("when name is whitespace", func(t *testing.T) {
			req := api.UpdateUserReq{
				Id:       "nope",
				Name:     "    ",
				Email:    "notfound@whereami.org",
				Password: "nada",
			}

			resp, err := cl.UpdateUser(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		})
	})

	t.Run("DeleteUser", func(t *testing.T) {
		t.Run("when user is deleted successfully", func(t *testing.T) {

			mockTeams.PurgeUserMembershipFunc = func(
				context.Context, *teams_api.PurgeUserMembershipReq) (*teams_api.PurgeUserMembershipResp, error) {
				return &teams_api.PurgeUserMembershipResp{}, nil
			}

			mockAuthz.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				// TODO: ChangeAgain bc required tests like UpdateUser have side effects on the database
				if req.Subject == "user:local:ChangeAgain" {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected user passed to PurgeSubjectFromPolicies")
			}

			mockTeams.PurgeUserMembershipFunc = func(
				_ context.Context, req *teams_api.PurgeUserMembershipReq) (*teams_api.PurgeUserMembershipResp, error) {
				if req.UserId == "alice" {
					return &teams_api.PurgeUserMembershipResp{Ids: []string{"teams-uuid-is-not-read"}}, nil
				}
				return nil, errors.New("unexpected args")
			}

			userEmail := api.Email{Email: "alice@email.com"}
			resp, err := cl.DeleteUser(ctx, &userEmail)
			assert.NoError(t, err)
			assert.NotNil(t, resp)

			// Cleanup
			mockAuthz.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
		})

		t.Run("when user is not found", func(t *testing.T) {
			userEmail := api.Email{Email: "nope@nope.nope"}
			resp, err := cl.DeleteUser(ctx, &userEmail)
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		})

		t.Run("when user is found but the policy purge fails", func(t *testing.T) {
			mockAuthz.PurgeSubjectFromPoliciesFunc = func(
				context.Context, *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				return nil, errors.New("simulate PurgeSubjectFromPolicies failure")
			}

			userEmail := api.Email{Email: "ada@email.com"}
			resp, err := cl.DeleteUser(ctx, &userEmail)
			assert.Error(t, err)
			assert.Nil(t, resp)

			// Cleanup
			mockAuthz.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
		})

	})
}

func TestUsersGRPCInternalErrors(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	serviceCerts := helpers.LoadDevCerts(t, "local-user-service")
	connFactory := secureconn.NewFactory(*serviceCerts)
	config := Config{
		Logger: logger,
		Users: &usersMock.ErrorOnlyConfig{
			Msg: "something went wrong",
		},
		ServiceCerts: serviceCerts,
	}

	serv, err := NewServer(ctx, config)
	if err != nil {
		t.Fatalf("opening connector: %s", err)
	}

	g := grpctest.NewServer(serv.NewGRPCServer())
	defer g.Close()

	conn, err := connFactory.Dial("local-user-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	cl := api.NewUsersMgmtClient(conn)

	t.Run("GetUsers", func(t *testing.T) {
		_, err := cl.GetUsers(ctx, &api.GetUsersReq{})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("GetUser", func(t *testing.T) {
		_, err := cl.GetUser(ctx, &api.Email{Email: "alice@email.com"})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("CreateUser", func(t *testing.T) {
		_, err := cl.CreateUser(ctx, &api.CreateUserReq{
			Email: "alice@email.com", Password: "longenoughpassword", Name: "Alice"})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("CreateUser (password too short)", func(t *testing.T) {
		_, err := cl.CreateUser(ctx, &api.CreateUserReq{
			Email: "alice@email.com", Password: "foo", Name: "Alice"})
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})

	t.Run("UpdateUser", func(t *testing.T) {
		_, err := cl.UpdateUser(ctx, &api.UpdateUserReq{Id: "unused",
			Email: "alice@email.com", Password: "foobar123", Name: "Alice"})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("DeleteUser", func(t *testing.T) {
		_, err := cl.DeleteUser(ctx, &api.Email{Email: "admin"})
		grpctest.AssertCode(t, codes.Internal, err)
	})
}

func newTeamsMock(t *testing.T) (*grpctest.Server, *teams_api.TeamsV1ServerMock) {
	t.Helper()
	certs := helpers.LoadDevCerts(t, "teams-service")
	mockTeams := teams_api.NewTeamsV1ServerMock()
	connFactory := secureconn.NewFactory(*certs)
	g := connFactory.NewServer()
	teams_api.RegisterTeamsV1Server(g, mockTeams)
	teams := grpctest.NewServer(g)
	return teams, mockTeams
}

func newAuthzMock(t *testing.T) (*grpctest.Server, *authz.SubjectPurgeServerMock) {
	t.Helper()
	certs := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*certs)
	g := connFactory.NewServer()
	mockCommon := authz.NewSubjectPurgeServerMock()
	mockCommon.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
	authz.RegisterSubjectPurgeServer(g, mockCommon)
	authzServer := grpctest.NewServer(g)
	return authzServer, mockCommon
}

func defaultMockPurgeFunc(context.Context,
	*authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	return &authz.PurgeSubjectFromPoliciesResp{}, nil
}
