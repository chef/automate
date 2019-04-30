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
			PGURL: constants.TestPgURL,
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
	db, err := sql.Open("postgres", constants.TestPgURL)
	require.NoError(t, err, "error opening db")
	err = db.Ping()
	require.NoError(t, err, "error pinging db")

	return db
}

func reset(t *testing.T, db *sql.DB) {
	_, err := db.Exec(`DELETE FROM chef_authn_tokens`)
	require.NoError(t, err)
}

type tokenTestData struct {
	tokProjects   []string
	projectFilter []string
}

var positiveCases = map[string]tokenTestData{

	"token with (unassigned) projects matches filter with nothing specified": {
		[]string{},
		[]string{},
	},
	"token with (unassigned) projects matches filter specifying all projects": {
		[]string{},
		[]string{constants.AllProjectsExternalID},
	},
	"token with some projects matches filter specifying all projects": {
		[]string{"p1", "p2", "p3"},
		[]string{constants.AllProjectsExternalID},
	},
	"token has single project, filter matches exactly": {
		[]string{"overlapping"},
		[]string{"overlapping"},
	},
	"token has single project, one project in filter matches": {
		[]string{"overlapping"},
		[]string{"overlapping", "not-overlapping"},
	},
	"token has multiple projects, filter matches both exactly": {
		[]string{"overlapping", "foo"},
		[]string{"overlapping", "foo"},
	},
	"token has no projects, filter has unassigned and other project": {
		[]string{},
		[]string{constants.UnassignedProjectID, "foo"},
	},
	"token has no projects, filter has (unassigned)": {
		[]string{},
		[]string{constants.UnassignedProjectID},
	},
}

var negativeCases = map[string]tokenTestData{
	"token has multiple project but filter has one non-intersecting project returns not found": {
		[]string{"p1", "p2"},
		[]string{"no-match"},
	},
	"token has projects but filter has non-intersecting projects returns not found": {
		[]string{"p1", "p2"},
		[]string{"p3", "p4"},
	},
	"token has no projects but filter contains some returns not found": {
		[]string{},
		[]string{"no-match"},
	},
	"token has project but filter contains only (unassigned) returns not found": {
		[]string{"p1"},
		[]string{constants.UnassignedProjectID},
	},
}

var standardCases = map[bool]map[string]tokenTestData{
	true:  positiveCases,
	false: negativeCases,
}

var outlierCases = map[string]struct {
	emptyDB       bool
	tokenID       string
	projectFilter []string
}{
	"with empty database no project filter returns not found": {
		true,
		"any-token",
		[]string{},
	},
	"with empty database some project filter returns not found": {
		true,
		"any-token",
		[]string{"project2", "project3"},
	},
	"unknown token with no project filter returns not found": {
		false,
		"unknown-token",
		[]string{},
	},
	"unknown token with some project filter returns not found": {
		false,
		"unknown-token",
		[]string{"project2", "project3"},
	},
}

func TestGetToken(t *testing.T) {
	store, db := setup(t)

	//DB time and Golang time are rounded differently
	tme := time.Now().UTC().Round(time.Second)
	tok := tokens.Token{
		ID:          "coolest-token-on-the-block",
		Description: "Cool Token",
		Active:      true,
		Value:       "secret-encoded-token-value",
		Created:     tme,
		Updated:     tme,
		Projects:    []string{},
	}

	t.Run("standard cases", func(t *testing.T) {
		for expectedSuccess, cases := range standardCases {
			for name, test := range cases {
				t.Run(name, func(t *testing.T) {
					reset(t, db)
					tok.Projects = test.tokProjects
					insertToken(t, db, tok)
					ctx := insertProjectsIntoNewContext(test.projectFilter)

					resp, err := store.GetToken(ctx, tok.ID)

					if expectedSuccess {
						assert.NoError(t, err)
						assert.Equal(t, &tok, resp)
					} else {
						assert.Error(t, err)
						assert.Equal(t, &tokens.NotFoundError{}, err)
					}
				})
			}
		}
	})

	t.Run("outlier cases", func(t *testing.T) {
		for name, test := range outlierCases {
			t.Run(name, func(t *testing.T) {
				reset(t, db)
				if !test.emptyDB {
					tok.Projects = []string{}
					insertToken(t, db, tok)
				}
				ctx := insertProjectsIntoNewContext(test.projectFilter)

				resp, err := store.GetToken(ctx, test.tokenID)

				assert.Nil(t, resp)
				assert.Error(t, err)
				assert.Equal(t, &tokens.NotFoundError{}, err)
			})
		}
	})
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
			Value:       "secret-encoded-token-value",
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

	t.Run("standard cases", func(t *testing.T) {
		cases := map[string]struct {
			projectFilter []string
			results       []*tokens.Token
		}{
			"no project filter returns all tokens": {
				[]string{},
				testTokens,
			},
			"all projects filter matches all tokens": {
				[]string{constants.AllProjectsExternalID},
				testTokens,
			},
			"single-project filter matches single token": {
				[]string{"project2"},
				[]*tokens.Token{testTokens[1]},
			},
			"single-project filter matches multiple tokens": {
				[]string{"project1"},
				[]*tokens.Token{testTokens[1], testTokens[2]},
			},
			"single-project filter matches unassigned project": {
				[]string{constants.UnassignedProjectID},
				[]*tokens.Token{testTokens[3]},
			},
			"multiple-project filter matches multiple tokens": {
				[]string{"project2", constants.UnassignedProjectID},
				[]*tokens.Token{testTokens[1], testTokens[3]},
			},
			"mix of included/excluded projects returns just included ones": {
				[]string{"project2", constants.UnassignedProjectID, "project-unknown"},
				[]*tokens.Token{testTokens[1], testTokens[3]},
			},
			"returns empty list if projects filter excludes all tokens": {
				[]string{"project-other", "project-unknown"},
				[]*tokens.Token{},
			},
		}

		for name, test := range cases {
			t.Run(name, func(t *testing.T) {
				reset(t, db)
				for _, tok := range testTokens {
					insertToken(t, db, *tok)
				}
				ctx := insertProjectsIntoNewContext(test.projectFilter)

				resp, err := store.GetTokens(ctx)

				require.NoError(t, err)
				assert.Equal(t, len(test.results), len(resp))
				for _, tok := range test.results {
					assert.Contains(t, resp, tok)
				}
			})
		}
	})

	t.Run("outlier cases", func(t *testing.T) {
		cases := map[string]struct {
			projectFilter []string
		}{
			"with empty database no project filter returns empty list": {
				[]string{},
			},
			"with empty database and all projects filter returns empty list": {
				[]string{constants.AllProjectsExternalID},
			},
			"with empty database and some project filter returns empty list": {
				[]string{"project2", "project3"},
			},
		}

		for name, test := range cases {
			t.Run(name, func(t *testing.T) {
				reset(t, db)
				ctx := insertProjectsIntoNewContext(test.projectFilter)

				resp, err := store.GetTokens(ctx)

				require.NoError(t, err)
				assert.Equal(t, 0, len(resp))
			})
		}
	})
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
			Value:       "secret-encoded-token-value",
			Created:     ti,
			Updated:     ti,
		},
	}
	insertToken(t, db, *tokens[0])
	insertToken(t, db, *tokens[1])

	resp, err := store.GetTokenIDWithValue(ctx, "secret-encoded-token-value")
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

			assertCount(t, db, 1, id)

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

			assertCount(t, db, 1, id)

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

	assertCount(t, db, 1, id)
	assert.Equal(id, tok.ID)
	assert.Equal(desc, tok.Description)
	assert.Equal(active, tok.Active)
	assert.WithinDuration(time.Now(), tok.Created, time.Second)
	assert.WithinDuration(time.Now(), tok.Updated, time.Second)
	assert.Equal([]string{}, tok.Projects)
}

func TestUpdateToken(t *testing.T) {
	store, db := setup(t)

	t.Run("standard cases", func(t *testing.T) {
		for expectedSuccess, cases := range standardCases {
			for name, test := range cases {
				//DB time and Golang time are rounded differently
				tme := time.Now().UTC().Round(time.Second)
				tok := tokens.Token{
					ID:          "coolest-token-on-the-block",
					Description: "Cool Token",
					Active:      true,
					Value:       "secret-encoded-token-value",
					Created:     tme,
					Updated:     tme,
					Projects:    []string{},
				}
				updatedTok := tokens.Token{
					ID:          tok.ID,
					Description: "THE coolest token on the block!",
					Active:      false,
					Value:       tok.Value,
					Created:     tok.Created,
					Updated:     tok.Updated,
					Projects:    []string{"project-ABC", "project-XYZ"},
				}

				t.Run(name+" (fields updated)", func(t *testing.T) {
					assert := assert.New(t)
					reset(t, db)
					tok.Projects = test.tokProjects
					insertToken(t, db, tok)
					ctx := insertProjectsIntoNewContext(test.projectFilter)

					resp, err := store.UpdateToken(
						ctx, tok.ID, updatedTok.Description, updatedTok.Active, updatedTok.Projects)

					if expectedSuccess {
						assert.NoError(err)
						assertCount(t, db, 1, tok.ID)
						resp.Created = updatedTok.Created // ignore the timestamps in comparison
						resp.Updated = updatedTok.Updated
						assert.Equal(updatedTok, *resp)
					} else {
						assert.Error(err)
						assert.Equal(&tokens.NotFoundError{}, err)
						assertCount(t, db, 1, tok.ID)
					}
				})

				t.Run(name+" (fields unchanged)", func(t *testing.T) {
					assert := assert.New(t)
					reset(t, db)
					tok.Projects = test.tokProjects
					insertToken(t, db, tok)
					ctx := insertProjectsIntoNewContext(test.projectFilter)

					resp, err := store.UpdateToken(ctx, tok.ID, tok.Description, tok.Active, tok.Projects)

					if expectedSuccess {
						assert.NoError(err)
						assertCount(t, db, 1, tok.ID)
						resp.Created = tme // ignore the timestamps in comparison
						resp.Updated = tme
						assert.Equal(tok, *resp)
					} else {
						assert.Error(err)
						assert.Equal(&tokens.NotFoundError{}, err)
						assertCount(t, db, 1, tok.ID)
					}
				})

				t.Run(name+" (description skipped)", func(t *testing.T) {
					assert := assert.New(t)
					reset(t, db)
					tok.Projects = test.tokProjects
					insertToken(t, db, tok)
					ctx := insertProjectsIntoNewContext(test.projectFilter)

					resp, err := store.UpdateToken(
						ctx, tok.ID, "", updatedTok.Active, updatedTok.Projects)

					if expectedSuccess {
						assert.NoError(err)
						assertCount(t, db, 1, tok.ID)
						resp.Created = updatedTok.Created // ignore the timestamps in comparison
						resp.Updated = updatedTok.Updated
						updatedTok.Description = tok.Description // should have the original value
						assert.Equal(updatedTok, *resp)
					} else {
						assert.Error(err)
						assert.Equal(&tokens.NotFoundError{}, err)
						assertCount(t, db, 1, tok.ID)
					}
				})
			}
		}
	})

	t.Run("outlier cases", func(t *testing.T) {
		for name, test := range outlierCases {
			//DB time and Golang time are rounded differently
			tme := time.Now().UTC().Round(time.Second)
			tok := tokens.Token{
				ID:          "coolest-token-on-the-block",
				Description: "Cool Token",
				Active:      true,
				Value:       "secret-encoded-token-value",
				Created:     tme,
				Updated:     tme,
				Projects:    []string{},
			}
			updatedDesc := "THE coolest token on the block!"
			updatedActive := false
			updatedProjects := []string{"project-ABC", "project-XYZ"}

			t.Run(name, func(t *testing.T) {
				assert := assert.New(t)
				reset(t, db)
				if !test.emptyDB {
					tok.Projects = []string{}
					insertToken(t, db, tok)
				}
				ctx := insertProjectsIntoNewContext(test.projectFilter)

				_, err := store.UpdateToken(ctx, test.tokenID, updatedDesc, updatedActive, updatedProjects)

				assert.Error(err)
				assert.Equal(&tokens.NotFoundError{}, err)
			})
		}
	})
}

func TestDeleteToken(t *testing.T) {
	store, db := setup(t)

	tok := tokens.Token{
		ID:          "coolest-token-on-the-block",
		Description: "Cool Token",
		Active:      true,
		Value:       "secret-encoded-token-value",
		Projects:    []string{},
	}

	t.Run("standard cases", func(t *testing.T) {
		for expectedSuccess, cases := range standardCases {
			for name, test := range cases {
				t.Run(name, func(t *testing.T) {
					reset(t, db)
					tok.Projects = test.tokProjects
					insertToken(t, db, tok)
					ctx := insertProjectsIntoNewContext(test.projectFilter)

					err := store.DeleteToken(ctx, tok.ID)

					if expectedSuccess {
						assert.NoError(t, err)
						assertCount(t, db, 0, tok.ID)
					} else {
						assert.Error(t, err)
						assert.Equal(t, &tokens.NotFoundError{}, err)
						assertCount(t, db, 1, tok.ID)
					}
				})
			}
		}
	})

	t.Run("outlier cases", func(t *testing.T) {
		for name, test := range outlierCases {
			t.Run(name, func(t *testing.T) {
				reset(t, db)
				if !test.emptyDB {
					tok.Projects = []string{}
					insertToken(t, db, tok)
				}
				ctx := insertProjectsIntoNewContext(test.projectFilter)

				err := store.DeleteToken(ctx, test.tokenID)

				assert.Error(t, err)
				assert.Equal(t, &tokens.NotFoundError{}, err)
			})
		}
	})
}

func insertToken(t *testing.T, db *sql.DB, tok tokens.Token) {
	if len(tok.Projects) == 0 {
		tok.Projects = []string{}
	}
	_, err := db.Exec(`INSERT INTO chef_authn_tokens
    (id, description, active, value, project_ids, created, updated)
		values ($1, $2, $3, $4, $5, $6, $7)`,
		tok.ID, tok.Description, tok.Active, tok.Value, pq.Array(tok.Projects),
		tok.Created, tok.Updated)
	require.NoError(t, err)
	assertCount(t, db, 1, tok.ID)
}

func assertCount(t *testing.T, db *sql.DB, expected int, id string) {
	t.Helper()
	row := db.QueryRow(`SELECT count(*) FROM chef_authn_tokens WHERE id=$1`, id)
	require.NotNil(t, row)
	var count int
	require.NoError(t, row.Scan(&count))
	assert.Equal(t, expected, count)
}

func insertProjectsIntoNewContext(projects []string) context.Context {
	return auth_context.NewOutgoingProjectsContext(
		auth_context.NewContext(
			context.Background(), []string{}, projects, "resource", "action", "pol"))
}
