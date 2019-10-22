package pg

import (
	"context"
	"database/sql"

	"github.com/lib/pq"
	"github.com/pkg/errors"
	"go.uber.org/zap"

	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	tutil "github.com/chef/automate/components/authn-service/tokens/util"
	"github.com/chef/automate/lib/grpc/auth_context"
	uuid "github.com/chef/automate/lib/uuid4"
)

func (a *adapter) CreateToken(ctx context.Context,
	id, description string, active bool, projects []string) (*tokens.Token, error) {
	value, err := tutil.GenerateNewToken()
	if err != nil {
		return nil, err
	}
	return a.CreateTokenWithValue(ctx, id, value, description, active, projects)
}

func (a *adapter) CreateTokenWithValue(ctx context.Context,
	id, value, description string, active bool, projects []string) (*tokens.Token, error) {
	if err := tutil.IsValidToken(value); err != nil {
		return nil, err
	}
	if id == "" {
		uid, err := uuid.NewV4()
		if err != nil {
			return nil, err
		}
		id = uid.String()
	}

	return a.insertToken(ctx, id, description, value, active, projects)
}

func (a *adapter) CreateLegacyTokenWithValue(ctx context.Context, value string) (*tokens.Token, error) {
	if err := tutil.IsValidLegacyToken(value); err != nil {
		return nil, err
	}

	id, err := uuid.NewV4() // TODO: hard-code legacy token ID?
	if err != nil {
		return nil, err
	}

	return a.insertToken(ctx, id.String(), tokens.LegacyTokenDescription, value, true, []string{})
}

// PurgeProject removes a project from every token it exists in
func (a *adapter) PurgeProject(ctx context.Context, projectID string) error {
	_, err := a.db.ExecContext(ctx, "UPDATE chef_authn_tokens SET project_ids=array_remove(project_ids, $1)", projectID)
	if err != nil {
		return err
	}
	return nil
}

func (a *adapter) insertToken(ctx context.Context,
	id string, description string, value string, active bool, projects []string) (*tokens.Token, error) {

	t := tokens.Token{}
	// ensure we do not pass null projects to db and break the not null constraint
	if projects == nil {
		projects = []string{}
	}
	_, err := a.validator.ValidateProjectAssignment(ctx, &authz_v2.ValidateProjectAssignmentReq{
		Subjects:        auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
		OldProjects:     []string{},
		NewProjects:     projects,
		IsUpdateRequest: false,
	})
	if err != nil {
		return nil, err
	}

	err = a.db.QueryRowContext(ctx,
		`INSERT INTO chef_authn_tokens(id, description, value, active, project_ids, created, updated)
		VALUES ($1, $2, $3, $4, $5, NOW(), NOW())
		RETURNING id, description, value, active, project_ids, created, updated`,
		id, description, value, active, pq.Array(projects)).
		Scan(&t.ID, &t.Description, &t.Value, &t.Active, pq.Array(&t.Projects), &t.Created, &t.Updated)

	if err != nil {
		return nil, processSQLError(err, "insert token")
	}
	return &t, nil
}

func (a *adapter) UpdateToken(ctx context.Context,
	id, description string, active bool, updatedProjects []string) (*tokens.Token, error) {

	tx, err := a.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, processSQLError(err, "get projects transaction")
	}

	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return nil, processSQLError(err, "get projects filter for tokens")
	}
	t := tokens.Token{}

	// ensure we do not pass null projects to db
	if updatedProjects == nil {
		updatedProjects = []string{}
	}

	var originalProjects []string
	var row *sql.Row
	err = tx.QueryRowContext(ctx,
		`SELECT project_ids FROM chef_authn_tokens
		WHERE id=$1 AND projects_match(project_ids, $2::TEXT[])
		FOR UPDATE;`,
		id, pq.Array(projectsFilter)).Scan(pq.Array(&originalProjects))
	if err != nil {
		return nil, processSQLError(err, "fetch projects for update")
	}

	_, err = a.validator.ValidateProjectAssignment(ctx, &authz_v2.ValidateProjectAssignmentReq{
		Subjects:        auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
		OldProjects:     originalProjects,
		NewProjects:     updatedProjects,
		IsUpdateRequest: true,
	})
	if err != nil {
		return nil, err
	}

	if description != "" {
		row = tx.QueryRowContext(ctx,
			`UPDATE chef_authn_tokens cat
			SET active=$2, description=$3, project_ids=$4, updated=NOW() 
			WHERE id=$1 AND projects_match(cat.project_ids, $5::TEXT[])
			RETURNING id, description, value, active, project_ids, created, updated`,
			id, active, description, pq.Array(updatedProjects), pq.Array(projectsFilter))
	} else {
		row = tx.QueryRowContext(ctx,
			`UPDATE chef_authn_tokens cat
			SET active=$2, project_ids=$3, updated=NOW()
			WHERE id=$1 AND projects_match(cat.project_ids, $4::TEXT[])
			RETURNING id, description, value, active, project_ids, created, updated`,
			id, active, pq.Array(updatedProjects), pq.Array(projectsFilter))
	}
	err = row.Scan(
		&t.ID, &t.Description, &t.Value, &t.Active, pq.Array(&t.Projects), &t.Created, &t.Updated)
	if err != nil {
		return nil, processSQLError(err, "update token")
	}

	if err := tx.Commit(); err != nil {
		return nil, processSQLError(err, "update token")
	}
	return &t, nil
}

func (a *adapter) DeleteToken(ctx context.Context, id string) error {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return processSQLError(err, "get projects filter for tokens")
	}

	res, err := a.db.ExecContext(ctx,
		`DELETE FROM chef_authn_tokens cat
		WHERE cat.id=$1
		AND projects_match(cat.project_ids, $2::TEXT[])`,
		id, pq.Array(projectsFilter))
	if err != nil {
		return processSQLError(err, "delete token by id")
	}

	count, err := res.RowsAffected()
	if err != nil {
		return processSQLError(err, "delete token by id")
	} else if count != 1 {
		return &tokens.NotFoundError{}
	}

	return nil
}

func (a *adapter) GetToken(ctx context.Context, id string) (*tokens.Token, error) {
	t := tokens.Token{}
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return &t, processSQLError(err, "get projects filter for tokens")
	}

	if err := a.db.QueryRowContext(ctx,
		`SELECT id, description, value, active, project_ids, created, updated
		FROM chef_authn_tokens cat
		WHERE cat.id=$1
		AND projects_match(cat.project_ids, $2::TEXT[])`,
		id, pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Description, &t.Value, &t.Active, pq.Array(&t.Projects), &t.Created, &t.Updated); err != nil {
		return nil, processSQLError(err, "select token by id")
	}
	return &t, nil
}

func (a *adapter) GetTokenIDWithValue(ctx context.Context, value string) (string, error) {
	var id string
	if err := a.db.QueryRowContext(ctx,
		`SELECT id FROM chef_authn_tokens WHERE value=$1 AND active`,
		value).
		Scan(&id); err != nil {
		return "", processSQLError(err, "select token ID by value")
	}

	return id, nil
}

func (a *adapter) GetTokens(ctx context.Context) ([]*tokens.Token, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return []*tokens.Token{}, processSQLError(err, "get projects filter for tokens")
	}

	ts := []*tokens.Token{}
	rows, err := a.db.QueryContext(ctx,
		`SELECT id, description, value, active, project_ids, created, updated
		FROM chef_authn_tokens cat
		WHERE projects_match(cat.project_ids, $1::TEXT[])`,
		pq.Array(projectsFilter))
	if err != nil {
		return nil, err
	}
	defer func() {
		if err := rows.Close(); err != nil {
			a.logger.Warn("failed to close DB rows", zap.Error(err))
		}
	}()

	for rows.Next() {
		t := tokens.Token{}
		if err := rows.Scan(&t.ID, &t.Description, &t.Value, &t.Active, pq.Array(&t.Projects),
			&t.Created, &t.Updated); err != nil {
			return nil, err
		}
		ts = append(ts, &t)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return ts, nil
}

// ProjectsListFromContext returns the project list from the context.
// In the case that the project list was ["*"], we return an empty list,
// since we do not wish to filter on projects.
func ProjectsListFromContext(ctx context.Context) ([]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		projectsFilter = []string{}
	}
	return projectsFilter, nil
}

// Reset deletes all tokens from the database /!\
func (a *adapter) Reset(ctx context.Context) error {
	_, err := a.db.ExecContext(ctx, `TRUNCATE chef_authn_tokens`)
	return err
}
