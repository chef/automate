package token

import (
	"context"
	"math/rand"
	"os"
	"reflect"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/zap"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/authn-service/constants"
	"github.com/chef/automate/components/authn-service/tokens/mock"
	"github.com/chef/automate/components/authn-service/tokens/pg"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	tutil "github.com/chef/automate/components/authn-service/tokens/util"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
	uuid "github.com/chef/automate/lib/uuid4"
)

var logger *zap.Logger

func init() {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.ErrorLevel)
	logger, _ = cfg.Build()
	rand.Seed(time.Now().Unix())
}

func initializePG() (*pg.Config, error) {
	ciMode := os.Getenv("CI") == "true"

	// If in CI mode, use the default
	if ciMode {
		return &pg.Config{
			PGURL:          constants.TestPgURL,
			MigrationsPath: "sql/",
		}, nil
	}

	customPGURL, pgURLPassed := os.LookupEnv("PG_URL")

	// If PG_URL wasn't passed (and we aren't in CI)
	// we shouldn't run the postgres tests, return nil.
	if !pgURLPassed {
		return nil, nil
	}

	return &pg.Config{
		PGURL:          customPGURL,
		MigrationsPath: "sql/",
	}, nil
}

type adapterTestFunc func(context.Context, *testing.T, tokens.Storage)

// TestToken tests the mock and pg adapters via their implemented adapter
// interface
func TestToken(t *testing.T) {
	pgCfg, err := initializePG()
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	adapters := map[string]tokens.TokenConfig{
		"mock": &mock.Config{},
		"pg":   pgCfg,
	}

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()
	mockAuthz := authz.NewAuthorizationServiceServerMock()
	mockAuthz.ValidateProjectAssignmentFunc = defaultValidateProjectAssignmentFunc
	authz.RegisterAuthorizationServiceServer(grpcAuthz, mockAuthz)
	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)
	authzClient := authz.NewAuthorizationServiceClient(authzConn)

	// Note: because the pg adapter doesn't let us set the stage so easily,
	//       these overlap a bit: most _create_ 1+ tokens first
	//       (any failures in these "setup creates" are triggering a test failure,
	//       i.e., they're t.Fatal'ing out)-
	tests := []adapterTestFunc{
		testGetTokens,
		testGetToken,
		testGetTokenIDWithValue,
		testGetTokenIDWithValueNotFound,
		testCreateToken,
		testCreateTokenWithInvalidValueFails,
		testCreateTokenWithValue,
		testCreateLegacyTokenWithInvalidValueFails,
		testCreateLegacyTokenWithValue,
		testDeleteToken,
		testDeleteTokenNotFound,
		testUpdateTokenActiveOnly,
		testUpdateTokenNotFound,
	} // Note: if a "not found" case is last, we'll leave a tidy test database

	for adpName, adpCfg := range adapters {
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()

		t.Run(adpName, func(t *testing.T) {
			for _, test := range tests {

				// TODO 2017/09/02 sr: this is pretty inefficient, we'll run the pg
				//  migrations for each and every test case. Since the overall
				//  performance still isn't that bad, I'll leave it at that for now.
				adp, err := adpCfg.Open(nil, logger, authzClient)
				if err != nil {
					// The logic to determine if we want to ignore this PG connection
					// failure is as follows:
					//   - if the developer has passed PG_URL, we assume they want to run
					//     the pg tests (for testing migrations, etc)
					//   - if this is running on CI, never skip
					// Why bother skipping? -- We don't want our test suite to require
					// a running postgres instance, as that we would be annoying.
					if pgCfg == nil {
						t.Fatalf("opening connector: %s", err)
					} else {
						t.Logf("opening database: %s", err)
						t.Logf("failed to open test database with PG_URL: %q", pgCfg.PGURL)
						t.SkipNow()
					}
				}
				require.Nil(t, err, "opening connector: %s", err)

				if r, ok := adp.(tokens.Resetter); ok {
					err := r.Reset(ctx)
					require.Nil(t, err, "reset adapter: %s", err)
				}

				// use the function name to identify the test case
				name := strings.Split(runtime.FuncForPC(reflect.ValueOf(test).Pointer()).Name(), ".")[2]
				t.Run(name, func(t *testing.T) {
					test(ctx, t, adp)
				})
			}
		})
	}
}

// TODO (bs): we should insert these w/ sql
func testGetTokens(ctx context.Context, t *testing.T, ta tokens.Storage) {
	tok, err := ta.CreateToken(ctx, "id0", "node1", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	tok2, err := ta.CreateToken(ctx, "id1", "node2", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	actualToks, err := ta.GetTokens(ctx)
	assert.NoError(t, err)
	assert.ElementsMatch(t, []*tokens.Token{tok, tok2}, actualToks)
}

func testGetToken(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{"project-1"})
	require.NoError(t, err)

	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testGetTokenIDWithValueNotFound(ctx context.Context, t *testing.T, ta tokens.Storage) {
	_, err := ta.GetTokenIDWithValue(ctx, "not-found")
	assert.Error(t, err)
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); !ok {
			t.Errorf("expected token.NotFoundError, got %s", err)
		}
	}
}

func testGetTokenIDWithValue(ctx context.Context, t *testing.T, ta tokens.Storage) {
	expectedID := "id0"
	expectedTok, err := ta.CreateToken(ctx, expectedID, "name", true, []string{"project-1"})
	require.NoError(t, err)

	tokID, err := ta.GetTokenIDWithValue(ctx, expectedTok.Value)
	assert.NoError(t, err)
	assert.Equal(t, expectedID, tokID)
}

func testCreateToken(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	// TODO use SQL or this is the same as GetToken
	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testCreateTokenWithValue(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	value := generateRandomTokenString(tutil.MinimumTokenLength())
	tok, err := ta.CreateTokenWithValue(ctx,
		id, value, "node3", true, []string{"project-1"})
	assert.NoError(t, err)
	assert.Equal(t, value, tok.Value)
	tok2, err := ta.GetToken(ctx, tok.ID)
	require.NoError(t, err)

	assert.Equal(t, tok, tok2)
}

func testCreateTokenWithInvalidValueFails(ctx context.Context, t *testing.T, ta tokens.Storage) {
	badValue := generateRandomTokenString(tutil.MinimumTokenLength() - 1)
	tok, err := ta.CreateTokenWithValue(ctx,
		"id0", badValue, "node3", true, []string{"project-1"})
	assert.Error(t, err)
	assert.Nil(t, tok)
}

func testCreateLegacyTokenWithValue(ctx context.Context, t *testing.T, ta tokens.Storage) {
	value := generateRandomTokenString(tutil.MinimumTokenLength())
	tok, err := ta.CreateLegacyTokenWithValue(ctx, value)
	assert.NoError(t, err)
	assert.NotNil(t, tok)
	assert.Equal(t, value, tok.Value)

	tok2, err := ta.GetToken(ctx, tok.ID)
	require.NoError(t, err)

	assert.Equal(t, tok, tok2)
}

func testCreateLegacyTokenWithInvalidValueFails(ctx context.Context, t *testing.T, ta tokens.Storage) {
	badValue := generateRandomTokenString(tutil.MinimumLegacyTokenLength - 1)
	tok, err := ta.CreateLegacyTokenWithValue(ctx, badValue)
	assert.Error(t, err)
	assert.Nil(t, tok)
}

func generateRandomTokenString(length int) string {
	var letters = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ=")
	b := make([]rune, length)
	for i := range b {
		b[i] = letters[rand.Intn(len(letters))]
	}
	return string(b)
}

func testDeleteToken(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	_, err := ta.CreateToken(ctx, id, "node1", true, []string{"project-1"})
	require.NoError(t, err)

	err = ta.DeleteToken(ctx, id)
	assert.NoError(t, err)

	_, err = ta.GetToken(ctx, id)
	assert.Error(t, err)
	if _, ok := errors.Cause(err).(*tokens.NotFoundError); !ok {
		t.Errorf("expected not found token error, got err=%v", err)
	}
}

func testDeleteTokenNotFound(ctx context.Context, t *testing.T, ta tokens.Storage) {
	err := ta.DeleteToken(ctx, uuid.Must(uuid.NewV4()).String())
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); !ok {
			t.Errorf("expected not found token 'node1', got err=%v", err)
		}
	}
}

func testUpdateTokenActiveOnly(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	desc := "node1"
	projs := []string{"project-1"}
	tok0, err := ta.CreateToken(ctx, id, desc, true, projs)
	require.NoError(t, err)

	tok, err := ta.UpdateToken(ctx, id, desc, false, projs)
	assert.NoError(t, err)
	assert.NotNil(t, tok)
	assert.Equal(t, tok0.Name, tok.Name)
	assert.Equal(t, false, tok.Active)
	assert.Equal(t, tok0.Created, tok.Created)
	assert.True(t, tok.Updated.After(tok.Created))
	assert.ElementsMatch(t, tok0.Projects, tok.Projects)
}

func testUpdateTokenUpdatesAll(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	tok, err := ta.CreateToken(ctx, id, "node1", true, []string{"project-1"})
	require.NoError(t, err)

	newDesc := "newDesc"
	newProj := []string{"project-2"}
	_, err = ta.UpdateToken(ctx, id, newDesc, false, newProj)
	require.NoError(t, err)

	updatedTok, err := ta.GetToken(ctx, id)
	assert.Equal(t, newDesc, updatedTok.Name)
	assert.Equal(t, false, updatedTok.Active)
	assert.Equal(t, tok.Created, updatedTok.Created)
	assert.True(t, tok.Updated.After(tok.Created))
	assert.ElementsMatch(t, newProj, updatedTok.Projects)
}

func testUpdateTokenNotFound(ctx context.Context, t *testing.T, ta tokens.Storage) {
	_, err := ta.UpdateToken(ctx, uuid.Must(uuid.NewV4()).String(), "desc", true, []string{"project-1"})
	assert.Error(t, err)
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); !ok {
			t.Errorf("expected not found token 'node1', got err=%v", err)
		}
	}
}

func insertProjectsIntoNewContext(projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(auth_context.NewContext(context.Background(),
		[]string{}, projects, "resource", "action"))
}

func defaultValidateProjectAssignmentFunc(context.Context,
	*authz.ValidateProjectAssignmentReq) (*authz.ValidateProjectAssignmentResp, error) {
	return &authz.ValidateProjectAssignmentResp{}, nil
}
