package postgres

import (
	"context"
	"fmt"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/projectassignment"
)

func (p *pg) CreateProject(ctx context.Context, project *storage.Project, skipPolicies bool) (*storage.Project, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, p.processError(err)
	}

	if project.Type == storage.Custom {
		row := tx.QueryRowContext(ctx, "SELECT count(*) FROM iam_projects WHERE type='custom'")
		var numProjects int
		if err := row.Scan(&numProjects); err != nil {
			return nil, p.processError(err)
		}

		if numProjects >= p.projectLimit {
			return nil, storage.NewMaxProjectsExceededError(p.projectLimit)
		}
	}

	row := tx.QueryRowContext(ctx, "SELECT EXISTS(SELECT 1 FROM iam_projects_graveyard WHERE id=$1)", project.ID)
	var existsInGraveyard bool
	if err := row.Scan(&existsInGraveyard); err != nil {
		err = p.processError(err)
		// failed with an unexpected error
		if err != storage.ErrNotFound {
			return nil, err
		}
	}
	if existsInGraveyard {
		return nil, storage.ErrProjectInGraveyard
	}

	if err := p.insertProjectWithQuerier(ctx, project, tx); err != nil {
		return nil, p.processError(err)
	}

	if !skipPolicies {
		if err := p.addSupportPolicies(ctx, project, tx); err != nil {
			return nil, p.processError(err)
		}
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	// Currently, we don't change anything from what is passed in.
	return project, nil
}

func (p *pg) addSupportPolicies(ctx context.Context, project *storage.Project, q Querier) error {
	policyParams := []struct {
		id   string
		name string
		role string
	}{
		{
			id:   fmt.Sprintf("%s-%s", project.ID, "project-owners"),
			name: fmt.Sprintf("%s %s", project.Name, "Project Owners"),
			role: constants.ProjectOwnerRoleID,
		},
		{
			id:   fmt.Sprintf("%s-%s", project.ID, "project-editors"),
			name: fmt.Sprintf("%s %s", project.Name, "Project Editors"),
			role: constants.EditorRoleID,
		},
		{
			id:   fmt.Sprintf("%s-%s", project.ID, "project-viewers"),
			name: fmt.Sprintf("%s %s", project.Name, "Project Viewers"),
			role: constants.ViewerRoleID,
		},
	}

	for _, param := range policyParams {
		pol := generatePolicy(project.ID, param.id, param.name, param.role)
		if err := p.insertCompletePolicy(ctx, &pol, pol.Projects, q); err != nil {
			return err
		}
	}

	if err := p.notifyPolicyChange(ctx, q); err != nil {
		return err
	}

	return nil
}

func generatePolicy(projectID string, id string, name string, role string) storage.Policy {
	return storage.Policy{
		ID:      id,
		Name:    name,
		Members: []storage.Member{},
		Statements: []storage.Statement{
			{
				Effect:    storage.Allow,
				Resources: []string{"*"},
				Projects:  []string{projectID},
				Actions:   []string{},
				Role:      role,
			},
		},
		Projects: []string{projectID},
	}
}

func (p *pg) UpdateProject(ctx context.Context, project *storage.Project) (*storage.Project, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	// Update project if ID found
	// AND there is an intersection between projects and a non-empty projectsFilter
	res, err := p.db.ExecContext(ctx,
		`UPDATE iam_projects SET name=$2
		WHERE id=$1 AND (array_length($3::TEXT[], 1) IS NULL OR id=ANY($3));`,
		project.ID, project.Name, pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}

	err = p.singleRowResultOrNotFoundErr(res)
	if err != nil {
		return nil, err
	}

	// Currently, we don't change anything from what is passed in.
	return project, nil
}

func (p *pg) GetProject(ctx context.Context, id string) (*storage.Project, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	var project storage.Project
	// Update project if ID found
	// AND there is an intersection between projects and a non-empty projectsFilter
	row := p.db.QueryRowContext(ctx, `SELECT query_project($1, $2)`, id, pq.Array(projectsFilter))
	if err := row.Scan(&project); err != nil {
		return nil, p.processError(err)
	}
	return &project, nil
}

func (p *pg) DeleteProject(ctx context.Context, id string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return p.processError(err)
	}

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return err
	}

	// Delete project if ID found AND intersection between projects and projectsFilter
	res, err := tx.ExecContext(ctx,
		`DELETE FROM iam_projects WHERE id=$1 AND (array_length($2::TEXT[], 1) IS NULL OR id=ANY($2));`,
		id, pq.Array(projectsFilter),
	)
	if err != nil {
		return p.processError(err)
	}

	err = p.singleRowResultOrNotFoundErr(res)
	if err != nil {
		// don't error if the project is already in the graveyard.
		// this is necessary since the status might not have been reported
		// to cereal on the first attempt, in which case we want this
		// function to be idempotent.
		if err == storage.ErrNotFound {
			gyRes, _ := tx.ExecContext(ctx, `SELECT id FROM iam_projects_graveyard WHERE id=$1`, id)
			gyErr := p.singleRowResultOrNotFoundErr(gyRes)
			// if we don't find the project in the graveyard, return the original error
			if gyErr != nil {
				return err
			}
			// project found in graveyard
			// abort transaction and report success to cereal. we don't really care about the rollback
			// since nothing will have happened at this point in the transaction in this case.
			// log the error if any for posterity though.
			err := tx.Rollback()
			if err != nil {
				p.logger.Warnf("failed to rollback ProjectDelete when already graveyarded for id %q: %s",
					id, err.Error())
			}
			return nil
		}
		return err
	}

	// insert project into graveyard
	_, err = tx.ExecContext(ctx,
		`INSERT INTO iam_projects_graveyard (id) VALUES ($1)`, id)
	if err != nil {
		return p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return storage.NewTxCommitError(err)
	}

	return nil
}

func (p *pg) RemoveProjectFromGraveyard(ctx context.Context, id string) error {
	_, err := p.db.ExecContext(ctx, `DELETE FROM iam_projects_graveyard WHERE id=$1;`, id)
	if err != nil {
		return p.processError(err)
	}

	return nil
}

// EnsureNoProjectsMissing returns projectassignment.ProjectsMissingError if projects are missing,
// otherwise it returns nil.
func (p *pg) EnsureNoProjectsMissing(ctx context.Context, projectIDs []string) error {
	return p.ensureNoProjectsMissingWithQuerier(ctx, p.db, projectIDs)
}

func (p *pg) ensureNoProjectsMissingWithQuerier(ctx context.Context, q Querier, projectIDs []string) error {
	// Return any input ID that does not exist in the projects table.
	if len(projectIDs) == 0 {
		return nil
	}
	rows, err := p.db.QueryContext(ctx,
		`SELECT id FROM unnest($1::text[]) AS input(id)
			WHERE NOT EXISTS (SELECT * FROM iam_projects p WHERE input.id = p.id);`, pq.Array(projectIDs))
	if err != nil {
		return p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	projectsNotFound := make([]string, 0)
	for rows.Next() {
		var projectIDNotFound string
		if err := rows.Scan(&projectIDNotFound); err != nil {
			return p.processError(err)
		}
		projectsNotFound = append(projectsNotFound, projectIDNotFound)
	}

	if len(projectsNotFound) != 0 {
		return projectassignment.NewProjectsMissingError(projectsNotFound)
	}

	return nil
}

func (p *pg) ListProjects(ctx context.Context) ([]*storage.Project, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	// List all projects that have intersection between projects and projectsFilter
	rows, err := p.db.QueryContext(ctx, "SELECT query_projects($1)", pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	var projects []*storage.Project
	for rows.Next() {
		var project storage.Project
		if err := rows.Scan(&project); err != nil {
			return nil, p.processError(err)
		}
		projects = append(projects, &project)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return projects, nil
}

func (p *pg) insertProjectWithQuerier(ctx context.Context, project *storage.Project, q Querier) error {
	_, err := q.ExecContext(ctx, `INSERT INTO iam_projects (id, name, type) VALUES ($1, $2, $3);`,
		project.ID, project.Name, project.Type.String())
	return err
}
