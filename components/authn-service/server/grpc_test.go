package server

import (
	"context"
	"fmt"
	"net/url"
	"reflect"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/zap"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	api "github.com/chef/automate/api/interservice/authn"
	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	tokenMock "github.com/chef/automate/components/authn-service/tokens/mock"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
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

func TestHealthGRPC(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	upstreamURL, err := url.Parse("http://internal-lb")
	if err != nil {
		t.Fatal(err)
	}
	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	config := Config{
		Logger:         logger,
		Upstream:       upstreamURL,
		Token:          &tokenMock.Config{Tokens: []*tokens.Token{}},
		Authenticators: map[string]AuthenticatorConfig{},
		ServiceCerts:   serviceCerts,
	}

	conn, close, _ := newTestGRPCServer(ctx, t, config)
	defer close()

	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}

func defaultMockPurgeFunc(context.Context,
	*authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	return &authz.PurgeSubjectFromPoliciesResp{}, nil
}

func newTestGRPCServer(ctx context.Context,
	t *testing.T, config Config) (*grpc.ClientConn, func(), *authz.SubjectPurgeServerMock) {
	t.Helper()

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()
	mockCommon := authz.NewSubjectPurgeServerMock()
	mockCommon.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
	authz.RegisterSubjectPurgeServer(grpcAuthz, mockCommon)
	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)
	authzSubjectClient := authz.NewSubjectPurgeClient(authzConn)
	authzV2Client := authz_v2.NewAuthorizationClient(authzConn)

	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	serv, err := NewServer(ctx, config, authzV2Client)
	if err != nil {
		t.Fatalf("instantiate server: %s", err)
	}

	authnServer := grpctest.NewServer(serv.NewGRPCServer(authzSubjectClient, authzV2Client))

	connFactory := secureconn.NewFactory(*serviceCerts)
	conn, err := connFactory.Dial("authn-service", authnServer.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	return conn, func() { authnServer.Close(); authzServer.Close() }, mockCommon
}

func TestTokenGRPC(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	upstreamURL, _ := url.Parse("http://internal-lb")
	tokenID := "e7a41d83-98e7-44a6-b835-d0938752e836"
	projects := []string{"project-1", "project-2"}
	mockToken := tokens.Token{
		ID:          tokenID,
		Value:       "mysecret",
		Description: "myfavtoken",
		Active:      true,
		Projects:    projects,
	}

	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	config := Config{
		Logger:   logger,
		Upstream: upstreamURL,
		Token: &tokenMock.Config{
			Tokens: []*tokens.Token{&mockToken},
		},
		Authenticators: map[string]AuthenticatorConfig{},
		ServiceCerts:   serviceCerts,
	}

	conn, close, authzMock := newTestGRPCServer(ctx, t, config)
	defer close()

	cl := api.NewTokensMgmtClient(conn)

	// note: we're a bit reckless with state in the token mock adapter so better
	// run this first

	// TODO 2017/10/06 sr: implement Reset() on mock adapter, use it (see
	//                     tokens/tokens_test.go)
	t.Run("GetTokens", func(t *testing.T) {
		cs, err := cl.GetTokens(ctx, &api.GetTokensReq{})
		if err != nil {
			t.Fatal(err)
		}
		if len(cs.Tokens) != 1 {
			t.Errorf("expected 1 result, got %d", len(cs.Tokens))
		}
	})

	t.Run("GetToken", func(t *testing.T) {
		t.Run("when ID is not a UUID4", func(t *testing.T) {
			ret, err := cl.GetToken(ctx, &api.GetTokenReq{Id: "something"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, ret)
		})

		t.Run("when token is not found", func(t *testing.T) {
			ret, err := cl.GetToken(ctx, &api.GetTokenReq{Id: "00000000-0000-0000-0000-000000000000"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, ret)
		})

		t.Run("when token is found", func(t *testing.T) {
			tok, err := cl.GetToken(ctx, &api.GetTokenReq{Id: tokenID})
			if err != nil {
				t.Fatalf("expected err == nil, got %v", err)
			}
			if tok.Id != tokenID {
				t.Errorf("expected ID %q, got %q", tokenID, tok.Id)
			}
			if tok.Value != mockToken.Value {
				t.Errorf("expected value %q, got %q", mockToken.Value, tok.Value)
			}
			if tok.Description != mockToken.Description {
				t.Errorf("expected description %q, got %q", mockToken.Description, tok.Description)
			}
			if tok.Active != mockToken.Active {
				t.Errorf("expected active %v, got %v", mockToken.Active, tok.Active)
			}
			if !reflect.DeepEqual(tok.Projects, mockToken.Projects) {
				t.Errorf("expected projects %q, got %q", mockToken.Projects, tok.Projects)
			}
		})
	})

	t.Run("CreateToken", func(t *testing.T) {
		reqs := []api.CreateTokenReq{
			{
				Active:      false,
				Description: "my new favorite",
				Projects:    []string{"project-1"},
			},
			{
				Active:      true,
				Description: "", // empty descr
				Projects:    []string{"project-1", "project-2"},
			},
			{
				Active:      true,
				Description: "my cool token",
				Projects:    []string{}, // empty projects
			},
		}
		for n, req := range reqs {
			t.Run(fmt.Sprintf("token %d", n), func(t *testing.T) {

				tok, err := cl.CreateToken(ctx, &req)
				if err != nil {
					t.Fatalf("expected err == nil, got %v", err)
				}
				if tok.Id == "" {
					t.Error("expected token to have an ID, got none")
				}
				if tok.Value == "" {
					t.Error("expected non-empty token")
				}
				if tok.Created == "" {
					t.Error("expected non-empty 'created'")
				} else if _, err := time.Parse(time.RFC3339, tok.Created); err != nil {
					t.Errorf("failed to parse 'created': %q", err)
				}
				if tok.Updated == "" {
					t.Error("expected non-empty 'updated'")
				} else if _, err := time.Parse(time.RFC3339, tok.Updated); err != nil {
					t.Errorf("failed to parse 'updated': %q", err)
				}
				if tok.Description != req.Description {
					t.Errorf("expected description %q, got %q", req.Description, tok.Description)
				}
				if tok.Active != req.Active {
					t.Errorf("expected active %v, got %v", req.Active, tok.Active)
				}
				assert.ElementsMatch(t, req.Projects, tok.Projects)
			})
		}

		t.Run("CreateToken with no projects succeeds", func(t *testing.T) {
			tok, err := cl.CreateToken(ctx, &api.CreateTokenReq{
				Active:      true,
				Description: "my new real favorite",
				// no projects
			})
			require.NoError(t, err)
			require.NotNil(t, tok)

			assert.Equal(t, "my new real favorite", tok.Description)
			assert.Equal(t, true, tok.Active)
			// the database returns an empty array which we test in pg_test.go
			// but here go coerces that empty array into a nil array
			// https://programming.guide/go/nil-slice-vs-empty-slice.html
			assert.Equal(t, []string(nil), tok.Projects)
		})
	})

	t.Run("DeleteToken", func(t *testing.T) {
		t.Run("when token ID is not a UUID4", func(t *testing.T) {
			ret, err := cl.DeleteToken(ctx, &api.DeleteTokenReq{Id: "what?"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, ret)
		})

		t.Run("when token is found", func(t *testing.T) {
			tok, err := cl.CreateToken(ctx, &api.CreateTokenReq{
				Active:      false,
				Description: "my new favorite",
				Projects:    []string{"project1"},
			})
			require.NoError(t, err)
			require.NotNil(t, tok)

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				if req.Subject == "token:"+tok.Id {
					return &authz.PurgeSubjectFromPoliciesResp{}, nil
				}
				return nil, errors.New("unexpected token ID passed to PurgeSubjectFromPolicies")
			}

			ret, err := cl.DeleteToken(ctx, &api.DeleteTokenReq{Id: tok.Id})
			require.NoError(t, err)
			require.NotNil(t, ret)

			// Cleanup
			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
		})

		t.Run("when token is not found", func(t *testing.T) {
			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				return nil, errors.New("unexpected call to PurgeSubjectFromPolicies")
			}

			ret, err := cl.DeleteToken(ctx, &api.DeleteTokenReq{Id: "00000000-0000-0000-0000-000000000000"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, ret)

			// Cleanup
			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
		})

		t.Run("when token is found but the policy purge fails", func(t *testing.T) {
			tok, err := cl.CreateToken(ctx, &api.CreateTokenReq{
				Active:      false,
				Description: "my new favorite",
				Projects:    []string{"project1"},
			})
			require.NoError(t, err)
			require.NotNil(t, tok)

			authzMock.PurgeSubjectFromPoliciesFunc = func(
				_ context.Context, req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
				return nil, errors.New("simulate PurgeSubjectFromPolicies failure")
			}

			ret, err := cl.DeleteToken(ctx, &api.DeleteTokenReq{Id: tok.Id})
			require.Nil(t, ret)
			require.NotNil(t, err)
			grpctest.AssertCode(t, codes.Internal, err)

			// Cleanup
			authzMock.PurgeSubjectFromPoliciesFunc = defaultMockPurgeFunc
		})

	})

	t.Run("UpdateToken", func(t *testing.T) {
		t.Run("when token ID is not a UUID4", func(t *testing.T) {
			// projects are always required but description is not
			ret, err := cl.UpdateToken(ctx, &api.UpdateTokenReq{Id: "what?", Projects: projects})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, ret)
		})

		t.Run("when token is not found", func(t *testing.T) {
			ret, err := cl.UpdateToken(ctx, &api.UpdateTokenReq{Id: "00000000-0000-0000-0000-000000000000", Projects: projects})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, ret)
		})

		t.Run("when token is updated successfully for active field only", func(t *testing.T) {
			tok, err := cl.UpdateToken(ctx, &api.UpdateTokenReq{
				Active:   false,
				Id:       tokenID,
				Projects: projects,
			})
			require.NoError(t, err)
			require.NotNil(t, tok)

			assert.Equal(t, mockToken.Description, tok.Description)
			assert.Equal(t, mockToken.Projects, tok.Projects)

			updated, err := time.Parse(time.RFC3339, tok.Updated)
			if err != nil {
				t.Errorf("failed to parse 'updated': %v", err)
			}
			created, err := time.Parse(time.RFC3339, tok.Created)
			if err != nil {
				t.Errorf("failed to parse 'created': %v", err)
			}
			if !updated.After(created) {
				t.Error("expected updated > created")
			}
		})

		t.Run("when no projects are passed request succeeds", func(t *testing.T) {
			desc := "A Description"
			tok, err := cl.UpdateToken(ctx, &api.UpdateTokenReq{
				Active:      false,
				Id:          tokenID,
				Description: desc,
			})

			require.NoError(t, err)
			require.NotNil(t, tok)

			assert.Equal(t, desc, tok.Description)
			assert.Equal(t, false, tok.Active)
			assert.Equal(t, tokenID, tok.Id)
			assert.Equal(t, 0, len(tok.Projects))
		})

		t.Run("when token is updated successfully for all fields", func(t *testing.T) {
			newDesc := "New Description"
			projects := append(projects, "new-project")
			tok, err := cl.UpdateToken(ctx, &api.UpdateTokenReq{
				Active:      false,
				Id:          tokenID,
				Description: newDesc,
				Projects:    projects,
			})
			require.NoError(t, err)
			require.NotNil(t, tok)

			assert.Equal(t, newDesc, tok.Description)
			assert.ElementsMatch(t, projects, tok.Projects)

			updated, err := time.Parse(time.RFC3339, tok.Updated)
			if err != nil {
				t.Errorf("failed to parse 'updated': %v", err)
			}
			created, err := time.Parse(time.RFC3339, tok.Created)
			if err != nil {
				t.Errorf("failed to parse 'created': %v", err)
			}
			if !updated.After(created) {
				t.Error("expected updated > created")
			}
		})
	})
}

func TestTokenGRPCInternalErrors(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	upstreamURL, err := url.Parse("http://internal-lb")
	if err != nil {
		t.Fatal(err)
	}
	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	config := Config{
		Logger:   logger,
		Upstream: upstreamURL,
		Token: &tokenMock.ErrorOnlyConfig{
			Msg: "something went wrong",
		},
		Authenticators: map[string]AuthenticatorConfig{},
		ServiceCerts:   serviceCerts,
	}

	conn, close, _ := newTestGRPCServer(ctx, t, config)
	defer close()
	cl := api.NewTokensMgmtClient(conn)

	t.Run("GetTokens", func(t *testing.T) {
		_, err := cl.GetTokens(ctx, &api.GetTokensReq{})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("GetToken", func(t *testing.T) {
		_, err := cl.GetToken(ctx, &api.GetTokenReq{Id: "00000000-0000-0000-0000-000000000000"})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("CreateToken", func(t *testing.T) {
		_, err := cl.CreateToken(ctx, &api.CreateTokenReq{
			Projects: []string{"project1"},
		})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("UpdateToken", func(t *testing.T) {
		_, err := cl.UpdateToken(ctx, &api.UpdateTokenReq{Id: "00000000-0000-0000-0000-000000000000", Projects: []string{"mock"}})
		grpctest.AssertCode(t, codes.Internal, err)
	})

	t.Run("DeleteToken", func(t *testing.T) {
		_, err := cl.DeleteToken(ctx, &api.DeleteTokenReq{Id: "00000000-0000-0000-0000-000000000000"})
		grpctest.AssertCode(t, codes.Internal, err)
	})
}
