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

	"github.com/chef/automate/components/authn-service/constants"
	"github.com/chef/automate/components/authn-service/tokens/mock"
	"github.com/chef/automate/components/authn-service/tokens/pg"
	"github.com/chef/automate/components/authn-service/tokens/pg/testconstants"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	tutil "github.com/chef/automate/components/authn-service/tokens/util"
	"github.com/chef/automate/lib/grpc/auth_context"
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
		testGetTokensWithSingleProjectsSingleFilter,
		testGetTokensWithMultipleProjectsSingleFilter,
		testGetTokensWithMultipleProjectsMultipleFilters,
		testGetTokensWithUnassignedAndOtherFilter,
		testGetTokensWithoutProjectsWithUnassignedFilter,
		testGetToken,
		testGetTokenWithSingleProjectSingleFilter,
		testGetTokenWithMultipleProjectsSingleFilter,
		testGetTokenWithMultipleProjectsMultipleFilters,
		testGetTokenNoProjectsUnassignedFilter,
		testGetTokenNoProjectsUnassignedAndOtherFilter,
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

func testGetTokensWhenSingleProjectsSingleFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	tok, err := ta.CreateToken(ctx, "id0", "node0", true, []string{"overlapping"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id1", "node1", true, []string{"no-overlap"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id2", "node2", true, []string{})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{"overlapping"})

	actualToks, err := ta.GetTokens(ctx)
	assert.NoError(t, err)
	assert.ElementsMatch(t, []*tokens.Token{tok}, actualToks)
}

func testGetTokensWithMultipleProjectsSingleFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	match1, err := ta.CreateToken(ctx, "id0", "node0", true, []string{"overlapping"})
	require.NoError(t, err)
	match2, err := ta.CreateToken(ctx, "id1", "node1", true, []string{"overlapping", "project-4"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id2", "node2", true, []string{"no-overlap"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id3", "node3", true, []string{})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{"overlapping"})

	expectedToks := []*tokens.Token{match1, match2}
	actualToks, err := ta.GetTokens(ctx)
	assert.NoError(t, err)
	assert.ElementsMatch(t, expectedToks, actualToks)
}

func testGetTokensWithMultipleProjectsMultipleFilters(ctx context.Context, t *testing.T, ta tokens.Storage) {
	match1, err := ta.CreateToken(ctx, "id", "node", true, []string{"overlapping"})
	require.NoError(t, err)
	match2, err := ta.CreateToken(ctx, "id-0", "node-0", true, []string{"more-overlap"})
	require.NoError(t, err)
	match3, err := ta.CreateToken(ctx, "id0", "node0", true, []string{"overlapping", "more-overlap"})
	require.NoError(t, err)
	match4, err := ta.CreateToken(ctx, "id1", "node1", true, []string{"more-overlap", "project-4"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id2", "node2", true, []string{"no-overlap"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id3", "node3", true, []string{})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{"overlapping", "more-overlap"})

	expectedToks := []*tokens.Token{match1, match2, match3, match4}
	actualToks, err := ta.GetTokens(ctx)
	assert.NoError(t, err)
	assert.ElementsMatch(t, expectedToks, actualToks)
}

func testGetTokensWithUnassignedAndOtherFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	match1, err := ta.CreateToken(ctx, "id", "node", true, []string{"overlapping"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id-0", "node-0", true, []string{"more-overlap"})
	require.NoError(t, err)
	match2, err := ta.CreateToken(ctx, "id0", "node0", true, []string{"overlapping", "more-overlap"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id1", "node1", true, []string{"more-overlap", "project-4"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id2", "node2", true, []string{"no-overlap"})
	require.NoError(t, err)
	unassignedProj, err := ta.CreateToken(ctx, "id3", "node3", true, []string{})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectsFilter, "overlapping"})

	expectedToks := []*tokens.Token{match1, match2, unassignedProj}
	actualToks, err := ta.GetTokens(ctx)
	assert.NoError(t, err)
	assert.ElementsMatch(t, expectedToks, actualToks)
}

func testGetTokensWithoutProjectsWithUnassignedFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	_, err := ta.CreateToken(ctx, "id", "node", true, []string{"overlapping"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id-0", "node-0", true, []string{"more-overlap"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id0", "node0", true, []string{"overlapping", "more-overlap"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id1", "node1", true, []string{"more-overlap", "project-4"})
	require.NoError(t, err)
	_, err = ta.CreateToken(ctx, "id2", "node2", true, []string{"no-overlap"})
	require.NoError(t, err)
	unassignedProj, err := ta.CreateToken(ctx, "id3", "node3", true, []string{})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectsFilter})

	expectedToks := []*tokens.Token{unassignedProj}
	actualToks, err := ta.GetTokens(ctx)
	assert.NoError(t, err)
	assert.ElementsMatch(t, expectedToks, actualToks)
}

func testGetToken(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{"project-1"})
	require.NoError(t, err)

	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testGetTokenWithSingleProjectSingleFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{"overlapping"})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{"overlapping"})

	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testGetTokenWithMultipleProjectsSingleFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{"overlapping", "another-project"})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{"overlapping"})

	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testGetTokenWithSingleProjectMultipleFilters(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{"overlapping"})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{"overlapping", "another-project"})

	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testGetTokenWithMultipleProjectsMultipleFilters(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{"overlapping", "no-overlap", "more-overlap"})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{"overlapping", "more-overlap"})

	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testGetTokenNoProjectsUnassignedFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectsFilter})

	actualTok, err := ta.GetToken(ctx, id)
	assert.NoError(t, err)
	assert.Equal(t, expectedTok, actualTok)
}

func testGetTokenNoProjectsUnassignedAndOtherFilter(ctx context.Context, t *testing.T, ta tokens.Storage) {
	id := "id0"
	expectedTok, err := ta.CreateToken(ctx, id, "node1", true, []string{})
	require.NoError(t, err)

	ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectsFilter, "another-filter"})

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
	expectedTok, err := ta.CreateToken(ctx, expectedID, "description", true, []string{"project-1"})
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
	assert.Equal(t, tok0.Description, tok.Description)
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
	assert.Equal(t, newDesc, updatedTok.Description)
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
		[]string{}, projects, "resource", "action", "pol"))
}
