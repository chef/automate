package integration

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"testing"

	"github.com/gorilla/mux"
	"github.com/stretchr/testify/require"
	"go.uber.org/zap"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authn"
	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	tokenauthn "github.com/chef/automate/components/authn-service/authenticator/tokens"
	"github.com/chef/automate/components/authn-service/constants"
	"github.com/chef/automate/components/authn-service/server"
	"github.com/chef/automate/components/authn-service/tokens/pg"
	"github.com/chef/automate/components/authn-service/tokens/pg/testconstants"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

var logger *zap.Logger

func init() {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.ErrorLevel)
	logger, _ = cfg.Build()
}

// In TestChefClientAuthn, we stand up a server that
// - serves REST endpoints for tokens
// - uses the tokens passed in to authenticate requests
// and run through some common operations.
//
// In these operations, the gateway-provided REST endpoints are used for the
// actions that are commonly done via the UI, and the GRPC endpoint for
// checking authentication (in the same way automate-gateway does it).
func TestChefClientAuthn(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	r := mux.NewRouter()
	r.Path("/pinata").
		HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte("Piñata!\n"))
		})
	upstream := httptest.NewServer(r)
	defer upstream.Close()

	upstreamURL, err := url.Parse(upstream.URL)
	if err != nil {
		t.Fatalf("parse test upstream URL %+v: %v", upstream, err)
	}

	pgURLGiven := false
	pgCfg := pg.Config{
		PGURL:          constants.TestPgURL,
		MigrationsPath: "../pg/sql/",
	}
	if v, found := os.LookupEnv("PG_URL"); found {
		pgCfg.PGURL = v
		pgURLGiven = true
	}

	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	// we wire in the pg-backed users adapter with both the authenticator, and the
	// tokens REST endpoint
	config := server.Config{
		Logger:   logger,
		Upstream: upstreamURL,
		Authenticators: map[string]server.AuthenticatorConfig{
			"chef-client-token-header-auth": &tokenauthn.HeaderTokenConfig{
				Headers: []string{"x-data-collector-token", "api-token"},
				Storage: tokenauthn.StorageConfig{
					Type:   "postgres",
					Config: &pgCfg,
				},
			},
		},
		Token:        &pgCfg,
		ServiceCerts: serviceCerts,
	}

	subjectPurgeClient, authorizationClient, close := newAuthzMock(t)
	fmt.Printf("pointer %v\n", authorizationClient)
	defer close()

	serv, err := server.NewServer(ctx, config, authorizationClient)
	if err != nil {
		// SKIP these tests if there's no PG_URL given -- and never skip during CI!
		if pgURLGiven || os.Getenv("CI") == "true" {
			t.Fatalf("opening connector: %s", err)
		} else {
			t.Logf("opening database: %s", err)
			t.Logf(testconstants.SkipPGMessageFmt, pgCfg.PGURL)
			t.SkipNow()
		}
		t.Fatalf("opening connector: %s", err)
	}

	// start services: local mgmt REST interface, and proxy service, and
	// authenticate endpoint
	g := grpctest.NewServer(serv.NewGRPCServer(subjectPurgeClient, authorizationClient))
	defer g.Close()

	connFactory := secureconn.NewFactory(*serviceCerts)

	conn, err := connFactory.Dial("authn-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	authClient := authn.NewAuthenticationClient(conn)
	tokenClient := authn.NewTokensMgmtClient(conn)

	tests := []struct {
		authHeader string
		desc       string
	}{
		{"api-token", "when using the api-token header"},
		{"x-data-collector-token", "when using the x-data-collector-token header"},
	}

	for _, test := range tests {
		t.Run(test.desc, func(t *testing.T) {

			var myTokenID string
			// set up a token to authenticate with
			fmt.Println("TEST ABOUT TO RUN")
			resp, err := tokenClient.CreateToken(ctx, &authn.CreateTokenReq{Active: true, Description: "mytoken", Projects: []string{}})
			if err != nil {
				t.Fatalf("create token request: %s", err)
			}

			// Note: technically, we already have the token's value in this response. We are going to go grab
			// it in the next step any, to ensure that flow works.
			myTokenID = resp.GetId()

			// read back token
			var myTokenValue string
			{
				resp, err := tokenClient.GetToken(ctx, &authn.GetTokenReq{Id: myTokenID})
				if err != nil {
					t.Fatalf("read back token: %s", err)
				}

				myTokenValue = resp.GetValue()
			}

			// read back the list of existing tokens
			{
				resp, err := tokenClient.GetTokens(ctx, &authn.GetTokensReq{})
				if err != nil {
					t.Fatalf("read tokens: %s", err)
				}
				if n := len(resp.Tokens); n != 1 {
					t.Errorf("expected 1 token, got %d", n)
				}
			}

			// check token works by sending a request (with that token) to the authenticate
			// grpc service
			{
				// use token in md to authenticate GRPC
				md := metadata.Pairs(test.authHeader, myTokenValue)
				ctx := metadata.NewOutgoingContext(ctx, md)

				_, err := authClient.Authenticate(ctx, &authn.AuthenticateRequest{})
				if err != nil {
					t.Errorf("error authenticating request: %s", err)
				}
			}

			// update active: disable token
			{
				_, err := tokenClient.UpdateToken(ctx, &authn.UpdateTokenReq{Id: myTokenID, Active: false, Projects: []string{}})
				if err != nil {
					t.Fatalf("update token: %s", err)
				}
			}

			// use just-disabled token again, expect failure
			{
				md := metadata.Pairs(test.authHeader, myTokenValue)
				ctx := metadata.NewOutgoingContext(ctx, md)

				_, err := authClient.Authenticate(ctx, &authn.AuthenticateRequest{})
				if err == nil {
					t.Error("expected error, got nil")
				}
				if err != nil {
					if s, ok := status.FromError(err); ok {
						if s.Code() != codes.Unauthenticated {
							t.Errorf("expected status %d, got %d: %s", codes.Unauthenticated, s.Code(), s.Err())
						}
					}
				}
			}
			// delete the token
			{
				_, err := tokenClient.DeleteToken(ctx, &authn.DeleteTokenReq{Id: myTokenID})
				if err != nil {
					t.Fatalf("delete token: %s", err)
				}
			}
		})
	}
}

func newAuthzMock(t *testing.T) (authz.SubjectPurgeClient, authz_v2.AuthorizationClient, func()) {
	t.Helper()
	certs := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*certs)
	g := connFactory.NewServer()
	mockV2Authz := authz_v2.NewAuthorizationServerMock()
	mockV2Authz.ValidateProjectAssignmentFunc = defaultValidateProjectAssignmentFunc
	mockCommon := authz.NewSubjectPurgeServerMock()
	mockCommon.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
	authz.RegisterSubjectPurgeServer(g, mockCommon)
	authz_v2.RegisterAuthorizationServer(g, mockV2Authz)
	authzServer := grpctest.NewServer(g)
	conn, err := connFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)

	return authz.NewSubjectPurgeClient(conn), authz_v2.NewAuthorizationClient(conn), authzServer.Close
}

func defaultMockPurgeFunc(context.Context,
	*authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	return &authz.PurgeSubjectFromPoliciesResp{}, nil
}

func defaultValidateProjectAssignmentFunc(context.Context,
	*authz_v2.ValidateProjectAssignmentReq) (*authz_v2.ValidateProjectAssignmentResp, error) {
	fmt.Println("WE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HEREWE IN HERE")
	return &authz_v2.ValidateProjectAssignmentResp{}, nil
}
