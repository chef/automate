package pg_test

import (
	"context"
	"database/sql"
	"os"
	"testing"
	"time"

	"github.com/lib/pq"
	"go.uber.org/zap"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authn-service/constants"
	"github.com/chef/automate/components/authn-service/tokens/pg"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	"github.com/chef/automate/lib/grpc/auth_context"
)

func setup(t *testing.T) (tokens.Storage, *sql.DB) {
	t.Helper()

	// Note: to set up PG locally for running these tests,
	// run the following from your command line from the components/authn-service folder:
	//
	// To start postgres in a docker container:
	//
	// make setup_docker_pg
	//
	// To run the tests, you can run this multiple times:
	//
	// make test_with_db
	//
	// When you are done testing, spin down the postgres container:
	//
	// make kill_docker_pg

	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.ErrorLevel)
	l, err := cfg.Build()
	require.NoError(t, err)

	pgCfg, err := initializePG()
	if err != nil {
		t.Fatalf("couldn't initialize pg config for tests: %s", err.Error())
	}

	if pgCfg == nil {
		t.Skipf("start pg container and set PG_URL to run")
	}

	backend, err := pgCfg.Open(nil, l)
	require.NoError(t, err)

	db := openDB(t)
	reset(t, db)

	return backend, db
}

func initializePG() (*pg.Config, error) {
	ciMode := os.Getenv("CI") == "true"

	// If in CI mode, use the default
	if ciMode {
		return &pg.Config{
			PGURL: constants.PgURL,
		}, nil
	}

	customPGURL, pgURLPassed := os.LookupEnv("PG_URL")

	// If PG_URL wasn't passed (and we aren't in CI)
	// we shouldn't run the postgres tests, return nil.
	if !pgURLPassed {
		return nil, nil
	}

	return &pg.Config{
		PGURL: customPGURL,
	}, nil
}

func openDB(t *testing.T) *sql.DB {
	t.Helper()
	db, err := sql.Open("postgres", constants.PgURL)
	require.NoError(t, err, "error opening db")
	err = db.Ping()
	require.NoError(t, err, "error pinging db")

	return db
}

func reset(t *testing.T, db *sql.DB) {
	_, err := db.Exec(`DELETE FROM chef_authn_tokens`)
	require.NoError(t, err)
}

func TestGetToken(t *testing.T) {
	store, db := setup(t)

	//DB time and Golang time are rounded differently
	tme := time.Now().UTC().Round(time.Second)
	id := "coolest-token-on-the-block"
	tok := tokens.Token{
		ID:          id,
		Description: "Cool Token",
		Active:      true,
		Value:       "asdf",
		Created:     tme,
		Updated:     tme,
		Projects:    []string{},
	}

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()

			_, err := store.GetToken(ctx, "not-real-token")
			assert.Error(t, err)
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token not found with existing token in store": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)

			_, err := store.GetToken(ctx, "not-real-token")
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token found in store": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)

			resp, err := store.GetToken(ctx, tok.ID)
			require.NoError(t, err)
			assert.Equal(t, &tok, resp)
		},
		"token has single project, filter matches exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{"overlapping"})

			resp, err := store.GetToken(ctx, tok.ID)
			require.NoError(t, err)
			assert.Equal(t, &tok, resp)
		},
		"token has single project, one project in filter matches": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{"overlapping", "not-overlapping"})

			resp, err := store.GetToken(ctx, tok.ID)
			require.NoError(t, err)
			assert.Equal(t, &tok, resp)
		},
		"token has multiple projects, filter matches both exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping", "foo"}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{"overlapping", "foo"})

			resp, err := store.GetToken(ctx, tok.ID)
			require.NoError(t, err)
			assert.Equal(t, &tok, resp)
		},
		"token has no projects, filter has unassigned and other project": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{constants.UnassignedProjectID, "foo"})

			_, err := store.GetToken(ctx, "not-real-token")
			assert.Error(t, err)
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token has no projects, filter has (unassigned)": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{constants.UnassignedProjectID})

			resp, err := store.GetToken(ctx, tok.ID)
			require.NoError(t, err)
			assert.Equal(t, &tok, resp)
		},
		"no tokens found with projects matching filter": func(t *testing.T) {
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{"no-match"})

			_, err := store.GetToken(ctx, tok.ID)
			assert.Error(t, err)
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
	}

	for name, test := range cases {
		reset(t, db)
		t.Run(name, test)
	}
}

func TestGetTokens(t *testing.T) {
	store, db := setup(t)

	//DB time and Golang time are rounded differently
	tme := time.Now().UTC().Round(time.Second)
	testTokens := []*tokens.Token{
		{
			ID:          "Uncool-token",
			Description: "Uncool Token",
			Active:      true,
			Value:       "abcd",
			Created:     tme,
			Updated:     tme,
			Projects:    []string{"project9"},
		},
		{
			ID:          "cool-token",
			Description: "Cool Token",
			Active:      true,
			Value:       "asdf",
			Created:     tme,
			Updated:     tme,
			Projects:    []string{"project1", "project2"},
		},
		{
			ID:          "also-cool-token",
			Description: "Also Cool Token",
			Active:      true,
			Value:       "other",
			Created:     tme,
			Updated:     tme,
			Projects:    []string{"project1"},
		},
		{
			ID:          "token-with-no-project",
			Description: "TokenX",
			Active:      true,
			Value:       "other",
			Created:     tme,
			Updated:     tme,
			Projects:    []string{},
		},
	}

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		// "successfully returns empty list when the database is empty": func(t *testing.T) {
		// 	ctx := context.Background()

		// 	resp, err := store.GetTokens(ctx)
		// 	require.NoError(t, err)
		// 	assert.Equal(t, resp, []*types.Token{})
		// },
		"no project filter returns all tokens": func(t *testing.T) {
			ctx := context.Background()

			resp, err := store.GetTokens(ctx)
			require.NoError(t, err)
			assert.ElementsMatch(t, testTokens, resp)
		},
		"all projects filter matches all tokens": func(t *testing.T) {
			ctx := insertProjectsIntoNewContext([]string{constants.AllProjectsExternalID})

			resp, err := store.GetTokens(ctx)
			require.NoError(t, err)
			assert.ElementsMatch(t, testTokens, resp)
		},
		"single-project filter matches single token": func(t *testing.T) {
			ctx := insertProjectsIntoNewContext([]string{"project2"})

			resp, err := store.GetTokens(ctx)
			require.NoError(t, err)
			assert.Equal(t, 1, len(resp))
			assert.Contains(t, resp, testTokens[1])
		},
		"single-project filter matches multiple tokens": func(t *testing.T) {
			ctx := insertProjectsIntoNewContext([]string{"project1"})

			resp, err := store.GetTokens(ctx)
			require.NoError(t, err)
			assert.Equal(t, 2, len(resp))
			assert.Contains(t, resp, testTokens[1])
			assert.Contains(t, resp, testTokens[2])
		},
		"single-project filter matches unassigned project": func(t *testing.T) {
			ctx := insertProjectsIntoNewContext([]string{constants.UnassignedProjectID})

			resp, err := store.GetTokens(ctx)
			require.NoError(t, err)
			assert.Equal(t, 1, len(resp))
			assert.Contains(t, resp, testTokens[3])
		},
		"multiple-project filter matches multiple tokens": func(t *testing.T) {
			ctx := insertProjectsIntoNewContext([]string{"project2", constants.UnassignedProjectID})

			resp, err := store.GetTokens(ctx)
			require.NoError(t, err)
			assert.Equal(t, 2, len(resp))
			assert.Contains(t, resp, testTokens[1])
			assert.Contains(t, resp, testTokens[3])
		},
		"returns empty list if projects filter excludes all tokens": func(t *testing.T) {
			ctx := insertProjectsIntoNewContext([]string{"project-other"})

			resp, err := store.GetTokens(ctx)
			require.NoError(t, err)
			assert.Equal(t, 0, len(resp))
		},
	}

	for name, test := range cases {
		reset(t, db)
		for _, tok := range testTokens {
			insertToken(t, db, *tok)
		}
		t.Run(name, test)
	}
}

func TestGetTokenIDWithValue(t *testing.T) {
	ctx := context.Background()
	store, db := setup(t)

	assert := assert.New(t)
	require := require.New(t)

	ti := time.Now().UTC()

	tokens := []*tokens.Token{
		{
			ID:          "Uncool-token",
			Description: "Uncool Token",
			Active:      true,
			Value:       "abcd",
			Created:     ti,
			Updated:     ti,
		},
		{
			ID:          "cool-token",
			Description: "Cool Token",
			Active:      true,
			Value:       "asdf",
			Created:     ti,
			Updated:     ti,
		},
	}
	insertToken(t, db, *tokens[0])
	insertToken(t, db, *tokens[1])

	resp, err := store.GetTokenIDWithValue(ctx, "asdf")
	require.NoError(err)

	assert.Equal("cool-token", resp)
}

func TestCreateToken(t *testing.T) {
	ctx := context.Background()
	store, db := setup(t)

	assert := assert.New(t)
	require := require.New(t)

	id := "coolest-token-on-the-block"
	desc := "THE coolest token on the block"
	active := true

	cases := map[string]func(*testing.T){
		"when valid project id passed": func(t *testing.T) {
			project_ids := []string{"project-1"}

			tok, err := store.CreateToken(ctx, id, desc, active, project_ids)
			require.NoError(err)
			require.NotNil(tok)

			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
			WHERE id=$1`, id))

			var value string
			err = db.QueryRow(`SELECT value FROM chef_authn_tokens WHERE id=$1`, id).Scan(&value)
			require.NoError(err)
			assert.NotNil(value)

			assert.Equal(id, tok.ID)
			assert.Equal(desc, tok.Description)
			assert.Equal(active, tok.Active)
			assert.WithinDuration(time.Now(), tok.Created, time.Second)
			assert.WithinDuration(time.Now(), tok.Updated, time.Second)
			assert.Equal(project_ids, tok.Projects)
		},
		"when no project_ids passed": func(t *testing.T) {
			project_ids := []string{}

			tok, err := store.CreateToken(ctx, id, desc, active, project_ids)
			require.NoError(err)
			require.NotNil(tok)

			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
			WHERE id=$1`, id))

			var value string
			err = db.QueryRow(`SELECT value FROM chef_authn_tokens WHERE id=$1`, id).Scan(&value)
			require.NoError(err)
			assert.NotNil(value)

			assert.Equal(id, tok.ID)
			assert.Equal(desc, tok.Description)
			assert.Equal(active, tok.Active)
			assert.WithinDuration(time.Now(), tok.Created, time.Second)
			assert.WithinDuration(time.Now(), tok.Updated, time.Second)
			assert.Equal(project_ids, tok.Projects)
		},
	}

	for name, test := range cases {
		reset(t, db)
		t.Run(name, test)
	}
}

func TestCreateTokenWithValue(t *testing.T) {
	ctx := context.Background()
	store, db := setup(t)
	assert := assert.New(t)

	id := "coolest-token-on-the-block"
	desc := "THE coolest token on the block"
	active := true
	value := "2flYtvKNAISyGAX9SlvuJOWQ1fU="
	project_ids := []string{}
	tok, err := store.CreateTokenWithValue(ctx, id, value, desc, active, project_ids)
	assert.NoError(err)
	assert.NotNil(tok)

	assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
		WHERE id=$1`, id))
	assert.Equal(id, tok.ID)
	assert.Equal(desc, tok.Description)
	assert.Equal(active, tok.Active)
	assert.WithinDuration(time.Now(), tok.Created, time.Second)
	assert.WithinDuration(time.Now(), tok.Updated, time.Second)
	assert.Equal([]string{}, tok.Projects)
}

func TestUpdateToken(t *testing.T) {
	store, db := setup(t)

	assert := assert.New(t)
	//DB time and Golang time are rounded differently
	tme := time.Now().UTC().Round(time.Second)
	id := "coolest-token-on-the-block"
	desc := "Cool Token"
	active := true
	value := "asdf"
	projects := []string{"project-1"}
	tok := tokens.Token{
		ID:          id,
		Description: desc,
		Active:      active,
		Value:       value,
		Created:     tme,
		Updated:     tme,
		Projects:    projects,
	}

	cases := map[string]func(*testing.T){
		"when original values unchanged": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)
			resp, err := store.UpdateToken(ctx, tok.ID, tok.Description, tok.Active, tok.Projects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(tok.Description, resp.Description)
			assert.Equal(tok.Active, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(tok.Projects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when only description changed": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)
			updatedDesc := "THE coolest token on the block"
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, tok.Active, tok.Projects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(tok.Active, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(tok.Projects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when only active changed": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)
			updatedActive := false
			resp, err := store.UpdateToken(ctx, tok.ID, tok.Description, updatedActive, tok.Projects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(tok.Description, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(tok.Projects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when only projects changed": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)
			updatedProjects := []string{"project-ABC"}
			resp, err := store.UpdateToken(ctx, tok.ID, tok.Description, tok.Active, updatedProjects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(tok.Description, resp.Description)
			assert.Equal(tok.Active, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when all values changed": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, updatedActive, updatedProjects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"token has single project, filter matches exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"overlapping"})
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, updatedActive, updatedProjects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"token has single project, one project in filter matches": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"overlapping", "not-overlapping"})
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, updatedActive, updatedProjects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"token has multiple projects, filter matches both exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping", "foo"}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"overlapping", "foo"})
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, updatedActive, updatedProjects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"token has no projects, filter has unassigned and other project": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"overlapping", constants.UnassignedProjectID})
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, updatedActive, updatedProjects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"token has no projects, filter has (unassigned)": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{constants.UnassignedProjectID})
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, updatedActive, updatedProjects)
			assert.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"no tokens have projects matching filter": func(t *testing.T) {
			tok.Projects = []string{"i-wish-i-matched"}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{"no-match"})
			updatedDesc := "THE coolest token on the block!"

			_, err := store.UpdateToken(ctx, tok.ID, updatedDesc, tok.Active, tok.Projects)

			assert.Equal(&tokens.NotFoundError{}, err)
		},
	}

	for name, test := range cases {
		reset(t, db)
		t.Run(name, test)
	}
}

func TestDeleteToken(t *testing.T) {
	store, db := setup(t)

	id := "coolest-token-on-the-block"
	active := true
	value := "asdf"
	tok := tokens.Token{
		ID:          id,
		Description: "Cool Token",
		Active:      active,
		Value:       value,
		Projects:    []string{},
	}

	// description => test func (map used for randomization)
	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			err := store.DeleteToken(ctx, "not-real-token")
			assert.Error(t, err)
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token not found with existing token in store": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)

			err := store.DeleteToken(ctx, "not-real-token")
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token found in store": func(t *testing.T) {
			ctx := context.Background()
			insertToken(t, db, tok)

			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has single project, filter matches exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"overlapping"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has single project, one project in filter matches": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"overlapping", "not-overlapping"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has multiple projects, filter matches both exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping", "foo"}
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"overlapping", "foo"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has no projects, filter has unassigned and other project": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{constants.UnassignedProjectID, "foo"})

			err := store.DeleteToken(ctx, "not-real-token")

			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token has no projects, filter has (unassigned)": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)
			ctx := insertProjectsIntoNewContext([]string{constants.UnassignedProjectID})

			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"no tokens found with projects matching filter": func(t *testing.T) {
			insertToken(t, db, tok)

			ctx := insertProjectsIntoNewContext([]string{"no-match"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
	}

	for name, test := range cases {
		reset(t, db)
		t.Run(name, test)
	}
}

func insertToken(t *testing.T, db *sql.DB, tok tokens.Token) {
	if len(tok.Projects) == 0 {
		tok.Projects = []string{}
	}
	_, err := db.Exec(`INSERT INTO chef_authn_tokens (id, description, active, value, project_ids, created, updated)
		values ($1, $2, $3, $4, $5, $6, $7)`, tok.ID, tok.Description, tok.Active, tok.Value, pq.Array(tok.Projects),
		tok.Created, tok.Updated)
	require.NoError(t, err)
	assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens WHERE id=$1`, tok.ID))
}

func assertCount(t *testing.T, expected int, row *sql.Row) {
	t.Helper()
	require.NotNil(t, row)
	var count int
	require.NoError(t, row.Scan(&count))
	assert.Equal(t, expected, count)
}

func insertProjectsIntoNewContext(projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(auth_context.NewContext(context.Background(),
		[]string{}, projects, "resource", "action", "pol"))
}
