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

	"github.com/chef/automate/components/authn-service/tokens/mock"
	"github.com/chef/automate/components/authn-service/tokens/pg"
	"github.com/chef/automate/components/authn-service/tokens/pg/testconstants"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	tutil "github.com/chef/automate/components/authn-service/tokens/util"
	uuid "github.com/chef/automate/lib/uuid4"
)

var logger *zap.Logger

func init() {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.ErrorLevel)
	logger, _ = cfg.Build()
	rand.Seed(time.Now().Unix())
}

type adapterTestFunc func(context.Context, *testing.T, tokens.Storage)

// TestToken tests the mock and pg adapters via their implemented adapter
// interface
func TestToken(t *testing.T) {
	pgURLGiven := false

	// Note: this matches CI
	pgCfg := pg.Config{
		PGURL: "postgresql://postgres@127.0.0.1:5432/authn_test?sslmode=disable",
	}
	if v, found := os.LookupEnv("PG_URL"); found {
		pgCfg.PGURL = v
		pgURLGiven = true
	}

	adapters := map[string]tokens.TokenConfig{
		"mock": &mock.Config{},
		"pg":   &pgCfg,
	}

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
		testCreateTokenWithValue,
		testCreateLegacyTokenWithValue,
		testDeleteToken,
		testDeleteTokenNotFound,
		testUpdateTokenActiveOnly,
		testUpdateTokenUpdatesUpdatedField,
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
				adp, err := adpCfg.Open(nil, logger)
				if err != nil {
					// The logic to determine if we want to ignore this PG connection
					// failure is as follows:
					//   - if the developer has passed PG_URL, we assume they want to run
					//     the pg tests (for testing migrations, etc)
					//   - if this is running on CI, never skip
					// Why bother skipping? -- We don't want our test suite to require
					// a running postgres instance, as that we would be annoying.
					if pgURLGiven || os.Getenv("CI") == "true" {
						t.Fatalf("opening connector: %s", err)
					} else {
						t.Logf("opening database: %s", err)
						t.Logf(testconstants.SkipPGMessageFmt, pgCfg.PGURL)
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

func testGetTokens(ctx context.Context, t *testing.T, ta tokens.Storage) {
	_, err := ta.CreateToken(ctx, "id0", "node1", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	_, err = ta.CreateToken(ctx, "id1", "node2", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	toks, err := ta.GetTokens(ctx)
	if err != nil {
		t.Fatal(err)
	}

	if len(toks) != 2 {
		t.Errorf("expected two tokens, got %d", len(toks))
	}
}

func testGetToken(ctx context.Context, t *testing.T, ta tokens.Storage) {
	tok0, err := ta.CreateToken(ctx, "id0", "node1", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	tok, err := ta.GetToken(ctx, tok0.ID)
	require.Nil(t, err, "expected no error, got err=%v", err)
	require.NotNil(t, tok, "expected token 'node1', got token=%v", tok)

	if tok.Value != tok0.Value {
		t.Errorf("expected token 'node1' to have a token %v, got %v", tok0.Value, tok.Value)
	}
}

func testGetTokenIDWithValueNotFound(ctx context.Context, t *testing.T, ta tokens.Storage) {
	_, err := ta.GetTokenIDWithValue(ctx, "token1")
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); !ok {
			t.Errorf("expected token.NotFoundError, got %s", err)
		}
	}
}

func testGetTokenIDWithValue(ctx context.Context, t *testing.T, ta tokens.Storage) {
	tok, err := ta.CreateToken(ctx, "id0", "node3", true, []string{"project-1"})
	require.Nilf(t, err, "expected no error, got err=%v", err)
	require.NotNilf(t, tok, "expected token 'node3', got token=%v", tok)

	if tok.Value == "" {
		t.Error("expected returned token to have a value, got ''")
	}

	tokID, err := ta.GetTokenIDWithValue(ctx, tok.Value)
	require.Nilf(t, err, "expected no error, got err=%v", err)
	assert.Equalf(t, tokID, tok.ID, "expected token ID to match %q", tok.ID)
}

func testCreateToken(ctx context.Context, t *testing.T, ta tokens.Storage) {
	before := time.Now().Add(-(time.Second * 20)).UTC()
	tok, err := ta.CreateToken(ctx, "id0", "node3", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)
	require.NotNil(t, tok, "expected token 'node3', got token=%v", tok)

	if tok.Value == "" {
		t.Error("expected returned token to have value, got ''")
	}

	tok2, err := ta.GetToken(ctx, tok.ID)
	require.Nil(t, err, "expected no error, got err=%v", err)
	require.NotNil(t, tok2, "expected token 'node3', got token=%v", tok2)

	if tok2.Value != tok.Value {
		t.Errorf("expected token 'node3' to have a value %v, got %v", tok.Value, tok2.Value)
	}
	if tok2.Description != tok.Description {
		t.Errorf("expected token 'node3' to have a description %v, got %v", tok.Description, tok2.Description)
	}
	if !tok2.Created.After(before) {
		t.Errorf("expected token 'node3' creation time to be after %s, got %s", before, tok2.Created)
	}
	assert.ElementsMatch(t, tok.Projects, tok2.Projects)
}

func testCreateTokenWithValue(ctx context.Context, t *testing.T, ta tokens.Storage) {
	before := time.Now().Add(-time.Second * 20).UTC()

	tok, err := ta.CreateTokenWithValue(ctx,
		"id0", generateRandomTokenString(tutil.MinimumTokenLength()), "node3", true, []string{"project-1"})
	require.NoError(t, err)
	require.NotNilf(t, tok, "expected token 'node3', got token=%v", tok)
	require.NotZero(t, tok.Value, "expected returned token to have a value")

	tok2, err := ta.GetToken(ctx, tok.ID)
	require.Nil(t, err, "expected no error, got err=%v", err)
	require.NotNil(t, tok2, "expected token 'node3', got token=%v", tok2)

	assert.Equal(t, tok.Value, tok2.Value)
	assert.Equal(t, tok.Description, tok2.Description)
	if !tok2.Created.After(before) {
		t.Errorf("expected token 'node3' creation time to be after %s, got %s", before, tok2.Created)
	}
}

func testCreateLegacyTokenWithValue(ctx context.Context, t *testing.T, ta tokens.Storage) {
	before := time.Now().Add(-time.Second * 20).UTC()

	tok, err := ta.CreateLegacyTokenWithValue(ctx, generateRandomTokenString(tutil.MinimumLegacyTokenLength-1))
	if err == nil {
		t.Errorf("expected token validation error")
	}

	tok, err = ta.CreateLegacyTokenWithValue(ctx, generateRandomTokenString(tutil.MinimumLegacyTokenLength))
	require.NoError(t, err)
	require.NotNil(t, tok, "expected token got token=%v", tok)
	require.NotZero(t, tok.Value, "expected returned token to have a value")

	tok2, err := ta.GetToken(ctx, tok.ID)
	require.Nil(t, err, "expected no error, got err=%v", err)
	require.NotNil(t, tok2, "expected token got token=%v", tok2)
	assert.Equal(t, tok.Value, tok2.Value)
	assert.Equal(t, tokens.LegacyTokenDescription, tok2.Description)
	assert.ElementsMatch(t, tok.Projects, tok2.Projects)
	if !tok2.Created.After(before) {
		t.Errorf("expected token creation time to be after %s, got %s", before, tok2.Created)
	}
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
	tok0, err := ta.CreateToken(ctx, "id0", "node1", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	err = ta.DeleteToken(ctx, tok0.ID)
	require.Nil(t, err, "expected deleted token 'node1', got err=%v", err)

	tok1, err := ta.GetToken(ctx, tok0.ID)
	require.NotNil(t, err, "expected no error, got err=%v", err)
	require.Nil(t, tok1, "expected token not to be found, got token=%v", tok1)

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
	tok0, err := ta.CreateToken(ctx, "id0", "node1", true, []string{"project-1"})
	if err != nil {
		t.Fatalf("expected no error, got err=%v", err)
	}
	tok, err := ta.UpdateToken(ctx, tok0.ID, "", false, []string{"project-1"})
	require.NoError(t, err)
	require.NotNil(t, tok)
	assert.Equal(t, tok0.Description, tok.Description)
	assert.Equal(t, false, tok.Active)
	assert.ElementsMatch(t, tok0.Projects, tok.Projects)
}

func testUpdateTokenUpdatesAll(ctx context.Context, t *testing.T, ta tokens.Storage) {
	tok0, err := ta.CreateToken(ctx, "id0", "node1", true, []string{"project-1"})
	if err != nil {
		t.Fatalf("expected no error, got err=%v", err)
	}
	newDesc := "newDesc"
	newProj := []string{"project-2"}
	tok, err := ta.UpdateToken(ctx, tok0.ID, newDesc, false, newProj)
	require.NoError(t, err)
	require.NotNil(t, tok)
	assert.Equal(t, newDesc, tok.Description)
	assert.Equal(t, false, tok.Active)
	assert.ElementsMatch(t, newProj, tok.Projects)
}

func testUpdateTokenNotFound(ctx context.Context, t *testing.T, ta tokens.Storage) {
	_, err := ta.UpdateToken(ctx, uuid.Must(uuid.NewV4()).String(), "desc", true, []string{"project-1"})
	if err != nil {
		if _, ok := errors.Cause(err).(*tokens.NotFoundError); !ok {
			t.Errorf("expected not found token 'node1', got err=%v", err)
		}
	}
}

func testUpdateTokenUpdatesUpdatedField(ctx context.Context, t *testing.T, ta tokens.Storage) {
	tok0, err := ta.CreateToken(ctx, "id0", "node1", true, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)

	tok, err := ta.UpdateToken(ctx, tok0.ID, "", false, []string{"project-1"})
	require.Nil(t, err, "expected no error, got err=%v", err)
	require.NotNil(t, tok, "expected token 'node1', got token=%v", tok)

	if tok.Created != tok0.Created {
		t.Errorf("expected token 'node1' to have created=%v, got %v", tok0.Created, tok.Created)
	}
}
