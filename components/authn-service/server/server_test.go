package server

import (
	"context"
	"net/url"
	"testing"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"

	// XXX teams_server.NewInMemoryServer will end in a nil pointer exception if
	// this package's init() hasn't run? It seems like it.
	_ "google.golang.org/grpc/grpclog"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tls/test/helpers"

	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	teams_api "github.com/chef/automate/api/interservice/teams/v2"
	teams_server "github.com/chef/automate/components/teams-service/server"
	teams_service "github.com/chef/automate/components/teams-service/service"
	teams_logger "github.com/chef/automate/lib/logger"

	auth "github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/components/authn-service/authenticator/mock"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Note: this test is only initializing mock-oidc authenticators, as those
// do not depend on other services.
// TODO: get some integration test going
func TestNewServerInitializesAuthenticators(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	authenticatorConfigs := map[string]AuthenticatorConfig{
		"foo": &mock.OidcConfig{Issuer: "https://issuer", Audience: "audience"},
		"bar": &mock.OidcConfig{Issuer: "bar", Audience: "guest"},
	}
	authenticatorCount := len(authenticatorConfigs)

	actual, _, err := newTestServer(ctx, t, nil, authenticatorConfigs, "")
	require.Nil(t, err)

	assert.Equal(t, authenticatorCount, len(actual.authenticators))
}

// This test uses an in-memory teams-server instance, so it's leaning a bit on
// the "integration-y" side. See below for proper, and more extensive unit
// tests.
func TestFetchLocalTeamsInAuthenticate(t *testing.T) {
	ctx := context.Background()

	// setup in-memory team-service
	l, err := teams_logger.NewLogger("text", "debug")
	require.Nil(t, err, "init logger for teams service: %s", err)
	teamsServiceCerts := helpers.LoadDevCerts(t, "teams-service")

	teamsSrv, err := teams_service.NewInMemoryService(l, secureconn.NewFactory(*teamsServiceCerts), nil)
	require.Nil(t, err, "init teams in memory server: %s", err)
	teams := grpctest.NewServer(teams_server.NewGRPCServer(teamsSrv))
	t.Log(teams.URL)
	defer teams.Close()

	mockUserID := "83a2fe1f-0793-4cbe-9b4b-737a9316a515"
	mockEmail := "admin"
	authenticatorConfigs := map[string]AuthenticatorConfig{
		"local": &mock.OidcConfig{Issuer: "https://a2-local-fresh-install-dev.cd.chef.co/dex",
			Audience: "automate-session",
			ConnID:   "local",
			UserID:   mockUserID,
			Email:    mockEmail,
		},
	}

	srv, serviceCerts, err := newTestServer(ctx, t, nil, authenticatorConfigs, teams.URL)
	require.Nil(t, err)

	authn := grpctest.NewServer(srv.NewGRPCServer(nil, nil))
	defer authn.Close()
	factory := secureconn.NewFactory(*serviceCerts)
	conn, err := factory.Dial("authn-service", authn.URL)
	require.Nil(t, err)
	client := auth.NewAuthenticationClient(conn)

	// Note: this id_token was snatched from a2-unstable, but is authenticated by
	// the mock-oidc adapter set up above. Furthermore, the mock adapter will
	// return a ConnID triggering the teams-service lookup in
	// (*Server).Authenticate. So, these things fit together.
	// nolint: lll
	idToken := `eyJhbGciOiJSUzI1NiIsImtpZCI6ImY1MjMxZTI2MDk2ZWVkNjZmM2FjMzhkMGJhMzhhNjNkMzg5NmU3NmIifQ.eyJpc3MiOiJodHRwczovL2EyLWxvY2FsLWZyZXNoLWluc3RhbGwtZGV2LmNkLmNoZWYuY28vZGV4Iiwic3ViIjoiQ2lRM01EVTJOV1l6Wmkxa1lURTJMVFJqT1RjdE9UUTJPUzAyTW1RMVpUSTJOREUyWWpRU0JXeHZZMkZzIiwiYXVkIjoiYXV0b21hdGUtc2Vzc2lvbiIsImV4cCI6MTUyMDk3NjQwNiwiaWF0IjoxNTIwODkwMDA2LCJhdF9oYXNoIjoiY2U4N1hOQzF3aVR2elBmZnNmRWxFZyIsImVtYWlsIjoiYWRtaW5AZXhhbXBsZS5jb20iLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwibmFtZSI6ImFkbWluIn0.ujMaaTVamcqYzi15iNFmICyE4mrf4H4YJ_kKXJLC4Ld4Xv8lS38-ZPaxdADWngjU6_HzZurZlJ_kobE94ueNEBtclAsVNASw1Rtf9y_ENTVypRKBsMpGT5fV_C0nL_HRgnRHZYWNVTqhjcc05YO3v6UFERs_Kj-GrGCe-WIXu11bFc4W8Zpxh5pRVewjDA5vfgd_CXftz3Ob9_9sCbyXPTuIPwobiBLy2yd8_wrm4UHi5DDruINQPrHJmgYEvDi_CA-0mHwbl8TYsQkIHECh87zFV31ufMTjZVbP81xUGAGurlk4SUG3kw4PqG5lfmBTBRA_LGOWBu3Xe93PXXRCQA`
	subjectID := "user:local:" + mockEmail

	// inject proper metadata header for (mock-)oidc
	ctx = metadata.NewOutgoingContext(ctx, metadata.MD(map[string][]string{
		"authorization": []string{"bearer " + idToken}}))

	t.Run("Authenticate", func(t *testing.T) {

		t.Run("with no existing teams", func(t *testing.T) {
			actual, err := client.Authenticate(ctx, &auth.AuthenticateRequest{})
			require.Nil(t, err)
			assert.Equal(t, subjectID, actual.Subject)
			assert.Equal(t, []string(nil), actual.Teams)
		})

		t.Run("with an existing team that has this member", func(t *testing.T) {
			// arrange
			teamAdmins, err := srv.teamsClient.GetTeam(ctx,
				&teams_api.GetTeamReq{
					Id: "admins",
				})
			require.Nil(t, err, "arrange: create team in teams-service")

			_, err = srv.teamsClient.AddTeamMembers(ctx,
				&teams_api.AddTeamMembersReq{
					Id:      teamAdmins.GetTeam().GetId(),
					UserIds: []string{mockUserID},
				})
			require.Nil(t, err, "arrange: add user to team in teams-service")

			// act
			actual, err := client.Authenticate(ctx, &auth.AuthenticateRequest{})
			require.Nil(t, err)

			// assert
			assert.Equal(t, subjectID, actual.Subject)
			assert.Equal(t, []string{"team:local:admins"}, actual.Teams)
		})
	})
}

func TestTeamsLookupForLocalUsersInAuthenticate(t *testing.T) {
	ctx := context.Background()

	mockTeams := teams_api.NewTeamsV2ServerMock()
	teams := newTeamService(t, mockTeams)
	defer teams.Close()

	mockUserID := uuid.Must(uuid.NewV4()).String()
	mockEmail := "admin"
	authenticatorConfigs := map[string]AuthenticatorConfig{
		"local": &mock.StaticConfig{ // "local" is the authenticator ID in authn-service
			ExternalID: "opaque-subject-id", // doesn't really matter for these tests
			ConnID:     "local",             // connector ID in dex
			UserID:     mockUserID,          // would usually be unwrapped from ExternalID
			Email:      mockEmail,
		},
	}

	srv, serviceCerts, err := newTestServer(ctx, t, nil, authenticatorConfigs, teams.URL)
	require.Nil(t, err)
	authn := grpctest.NewServer(srv.NewGRPCServer(nil, nil))
	defer authn.Close()

	factory := secureconn.NewFactory(*serviceCerts)
	conn, err := factory.Dial("authn-service", authn.URL)
	require.Nil(t, err)
	client := auth.NewAuthenticationClient(conn)

	tests := map[string]struct {
		teamsResp *teams_api.GetTeamsForMemberResp
		checks    []checkFunc
	}{
		"when the user has no teams": {
			&teams_api.GetTeamsForMemberResp{},
			check(
				hasTeams(0),
			),
		},
		"when the user has exactly one team": {
			&teams_api.GetTeamsForMemberResp{Teams: []*teams_api.Team{
				{
					Id:   "admins",
					Name: "admins",
				},
			}},
			check(
				hasTeams(1),
				containsTeam("team:local:admins"),
			),
		},
		"when the user has a team that has a space in it": {
			&teams_api.GetTeamsForMemberResp{Teams: []*teams_api.Team{
				{
					Id:   "ad mins",
					Name: "admins",
				},
			}},
			check(
				hasTeams(1),
				containsTeam("team:local:ad mins"),
			),
		},
		"when the user has two teams": {
			&teams_api.GetTeamsForMemberResp{Teams: []*teams_api.Team{
				{
					Id:   "admins",
					Name: "admins",
				},
				{
					Id:   "überadmins",
					Name: "they can do so much more",
				},
			}},
			check(
				hasTeams(2),
				containsTeam("team:local:admins"),
				containsTeam("team:local:überadmins"),
			),
		},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			// arrange
			mockTeams.Reset()
			mockTeams.GetTeamsForMemberFunc = func(
				context.Context, *teams_api.GetTeamsForMemberReq) (*teams_api.GetTeamsForMemberResp, error) {
				return tc.teamsResp, nil
			}

			// act
			resp, err := client.Authenticate(ctx, &auth.AuthenticateRequest{})
			require.Nil(t, err, "authenticate call")

			// assert
			for _, f := range tc.checks {
				f(t, resp)
			}
		})
	}
}

func TestNoTeamsLookupForNonLocalUsersInAuthenticate(t *testing.T) {
	ctx := context.Background()

	mockTeams := teams_api.NewTeamsV2ServerMock()
	teams := newTeamService(t, mockTeams)
	defer teams.Close()

	mockUserID := uuid.Must(uuid.NewV4()).String()
	authenticatorConfigs := map[string]AuthenticatorConfig{
		"local": &mock.StaticConfig{ // "local" is the authenticator ID in authn-service
			ExternalID: "opaque-subject-id", // doesn't really matter for these tests
			ConnID:     "ldap",              // connector ID in dex
			UserID:     mockUserID,          // would usually be unwrapped from ExternalID
			// hardcoded: returned for each authentication attempt using this (mock, static) authenticator
			Teams: []string{"admins", "viewers"},
		},
	}

	srv, serviceCerts, err := newTestServer(ctx, t, nil, authenticatorConfigs, teams.URL)
	require.Nil(t, err)
	authn := grpctest.NewServer(srv.NewGRPCServer(nil, nil))
	defer authn.Close()

	factory := secureconn.NewFactory(*serviceCerts)
	conn, err := factory.Dial("authn-service", authn.URL)
	require.Nil(t, err)
	client := auth.NewAuthenticationClient(conn)

	checks := []checkFunc{
		hasTeams(2),
		containsTeam("team:ldap:admins"),
		containsTeam("team:ldap:viewers"),
	}

	t.Run("ensure teams-service not called", func(t *testing.T) {
		// arrange
		mockTeams.Reset()
		// setup our teams-service mock so that everything goes to hell if it's called
		mockTeams.GetTeamsForMemberFunc = func(
			context.Context, *teams_api.GetTeamsForMemberReq) (*teams_api.GetTeamsForMemberResp, error) {
			assert.True(t, false, "don't call this")
			return nil, status.Error(codes.Internal, "shouldn't have called this service")
		}

		// act
		resp, err := client.Authenticate(ctx, &auth.AuthenticateRequest{})
		require.Nil(t, err, "authenticate call")

		// assert
		for _, f := range checks {
			f(t, resp)
		}
	})
}

// Table driven test helpers, adapted from:
// https://github.com/golang/go/blob/bf9f1c15035ab9bb695a9a3504e465a1896b4b8c/src/net/http/httptest/recorder_test.go#L14
type checkFunc func(*testing.T, *auth.AuthenticateResponse)

// Convenience wrapper allows you to say
//     check(x, y, z) instead of []checkFunc { x, y, z }
var check = func(fns ...checkFunc) []checkFunc { return fns }

func hasTeams(n int) checkFunc {
	return func(t *testing.T, r *auth.AuthenticateResponse) {
		assert.Equal(t, n, len(r.Teams))
	}
}

func containsTeam(team string) checkFunc {
	return func(t *testing.T, r *auth.AuthenticateResponse) {
		assert.Contains(t, r.Teams, team)
	}
}

// mini-factories

func newTeamService(t *testing.T, m *teams_api.TeamsV2ServerMock) *grpctest.Server {
	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, "teams-service")
	connFactory := secureconn.NewFactory(*serviceCerts)
	g := connFactory.NewServer()
	teams_api.RegisterTeamsV2Server(g, m)
	return grpctest.NewServer(g)
}

func newTestServer(ctx context.Context,
	t *testing.T,
	upstream *url.URL,
	authenticators map[string]AuthenticatorConfig,
	teamURL string) (*Server, *certs.ServiceCerts, error) {
	if upstream == nil {
		upstream, _ = url.Parse("http://upstream:port/path")
	}
	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	config := Config{
		Upstream:       upstream,
		Logger:         logger,
		Authenticators: authenticators,
		TeamsAddress:   teamURL,
		ServiceCerts:   serviceCerts,
	}

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()
	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)
	authzV2Client := authz_v2.NewAuthorizationClient(authzConn)

	srv, err := newServer(ctx, config, authzV2Client)
	return srv, serviceCerts, err
}
