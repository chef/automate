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

func setup(t *testing.T) (tokens.Storage, *sql.DB, context.Context) {
	t.Helper()
	ctx := context.Background()

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

	return backend, db, ctx
}

func initializePG() (*pg.Config, error) {
	ciMode := os.Getenv("CI") == "true"

	// If in CI mode, use the default
	if ciMode {
		return &pg.Config{
			PGURL: "postgresql://postgres@127.0.0.1:5432/authn_test?sslmode=disable",
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
	db, err := sql.Open("postgres", "postgresql://postgres@127.0.0.1:5432/authn_test?sslmode=disable")
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
	store, db, ctx := setup(t)

	assert := assert.New(t)
	require := require.New(t)

	//DB time and Golang time are rounded differently
	tme := time.Now().UTC().Round(time.Second)
	exp := tokens.Token{
		ID:          "cool-token",
		Description: "Cool Token",
		Active:      true,
		Value:       "asdf",
		Created:     tme,
		Updated:     tme,
		Projects:    []string{"project1", "project2"},
	}

	insertToken(t, db, exp)

	resp, err := store.GetToken(ctx, exp.ID)
	require.NoError(err)
	assert.Equal(&exp, resp)
}

func TestGetTokens(t *testing.T) {
	store, db, ctx := setup(t)

	assert := assert.New(t)
	require := require.New(t)

	//DB time and Golang time are rounded differently
	tme := time.Now().UTC().Round(time.Second)

	tokens := []*tokens.Token{
		{
			ID:          "Uncool-token",
			Description: "Uncool Token",
			Active:      true,
			Value:       "abcd",
			Created:     tme,
			Updated:     tme,
			Projects:    []string{"proj9"},
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
	}

	resp, err := store.GetTokens(ctx)
	require.Empty(resp)

	insertToken(t, db, *tokens[0])
	insertToken(t, db, *tokens[1])

	resp, err = store.GetTokens(ctx)
	require.NoError(err)

	assert.ElementsMatch(tokens, resp)
}

func TestGetTokenIDWithValue(t *testing.T) {
	store, db, ctx := setup(t)

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
	store, db, ctx := setup(t)

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
	store, db, ctx := setup(t)
	assert := assert.New(t)
	require := require.New(t)

	id := "coolest-token-on-the-block"
	desc := "THE coolest token on the block"
	active := true
	value := "2flYtvKNAISyGAX9SlvuJOWQ1fU="
	project_ids := []string{}
	tok, err := store.CreateTokenWithValue(ctx, id, value, desc, active, project_ids)
	require.NoError(err)
	require.NotNil(tok)

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
	store, db, ctx := setup(t)

	assert := assert.New(t)
	require := require.New(t)

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
			resp, err := store.UpdateToken(ctx, tok.ID, tok.Description, tok.Active, tok.Projects)
			require.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(tok.Description, resp.Description)
			assert.Equal(tok.Active, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(tok.Projects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when only description changed": func(t *testing.T) {
			updatedDesc := "THE coolest token on the block"
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, tok.Active, tok.Projects)
			require.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(tok.Active, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(tok.Projects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when only active changed": func(t *testing.T) {
			updatedActive := false
			resp, err := store.UpdateToken(ctx, tok.ID, tok.Description, updatedActive, tok.Projects)
			require.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(tok.Description, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(tok.Projects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when only projects changed": func(t *testing.T) {
			updatedProjects := []string{"project-ABC"}
			resp, err := store.UpdateToken(ctx, tok.ID, tok.Description, tok.Active, updatedProjects)
			require.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(tok.Description, resp.Description)
			assert.Equal(tok.Active, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
		"when all values changed": func(t *testing.T) {
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}
			resp, err := store.UpdateToken(ctx, tok.ID, updatedDesc, updatedActive, updatedProjects)
			require.NoError(err)

			assert.Equal(tok.ID, resp.ID)
			assert.Equal(updatedDesc, resp.Description)
			assert.Equal(updatedActive, resp.Active)
			assert.Equal(tok.Value, resp.Value)
			assert.Equal(updatedProjects, resp.Projects)
			assert.Equal(tok.Created, resp.Created)
			assert.NotEqual(tok.Updated, resp.Updated)
		},
	}

	for name, test := range cases {
		reset(t, db)
		insertToken(t, db, tok)
		t.Run(name, test)
	}
}

func TestDeleteToken(t *testing.T) {
	store, db, ctx := setup(t)
	require := require.New(t)

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
			err := store.DeleteToken(ctx, "not-real-token")
			assert.Error(t, err)
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token not found with existing token in store": func(t *testing.T) {
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))

			err := store.DeleteToken(ctx, "not-real-token")
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token found in store": func(t *testing.T) {
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))

			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has single project, filter matches exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))

			ctx = insertProjectsIntoNewContext([]string{"overlapping"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has single project, one project in filter matches": func(t *testing.T) {
			tok.Projects = []string{"overlapping"}
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))

			ctx = insertProjectsIntoNewContext([]string{"overlapping", "not-overlapping"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has multiple projects, filter matches both exactly": func(t *testing.T) {
			tok.Projects = []string{"overlapping", "foo"}
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))

			ctx = insertProjectsIntoNewContext([]string{"overlapping", "foo"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.NoError(t, err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"token has no projects, filter has unassigned and other project": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
			ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectsFilter, "foo"})

			err := store.DeleteToken(ctx, "not-real-token")

			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
		"token has no projects, filter has (unassigned)": func(t *testing.T) {
			tok.Projects = []string{}
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
			ctx = insertProjectsIntoNewContext([]string{constants.UnassignedProjectsFilter})

			err := store.DeleteToken(ctx, tok.ID)
			require.NoError(err)

			assertCount(t, 0, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))
		},
		"no tokens found with projects matching filter": func(t *testing.T) {
			insertToken(t, db, tok)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM chef_authn_tokens
				WHERE id=$1`, id))

			ctx = insertProjectsIntoNewContext([]string{"no-match"})
			err := store.DeleteToken(ctx, tok.ID)
			assert.Equal(t, &tokens.NotFoundError{}, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		reset(t, db)
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
