package postgres

import (
	"context"
	"database/sql"
	"regexp"

	"github.com/lib/pq" // adapter for database/sql
	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
)

var emptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

type postgres struct {
	db          *sql.DB
	logger      logger.Logger
	authzClient authz.AuthorizationServiceClient
}

type querier interface {
	QueryRowContext(context.Context, string, ...interface{}) *sql.Row
}

// New instantiates and returns a postgres storage implementation
func New(logger logger.Logger, migrationConfig migration.Config,
	authzClient authz.AuthorizationServiceClient) (storage.Storage, error) {

	if err := migrationConfig.Migrate(); err != nil {
		return nil, errors.Wrap(err, "database migrations")
	}

	db, err := initPostgresDB(migrationConfig.PGURL.String())
	if err != nil {
		return nil, errors.Wrap(err, "initialize database")
	}

	return &postgres{db, logger, authzClient}, nil
}

func initPostgresDB(pgURL string) (*sql.DB, error) {
	d, err := db.PGOpen(pgURL)
	if err != nil {
		return nil, err
	}

	if err := d.Ping(); err != nil {
		return nil, errors.Wrap(err, "opening database connection")
	}

	return d, nil
}

// StoreTeam saves a team to the DB.
func (p *postgres) StoreTeam(ctx context.Context,
	id string, name string, projects []string) (storage.Team, error) {

	// ensure we do not pass null projects to db and break the "not null" constraint
	if len(projects) == 0 {
		projects = []string{}
	}
	err := p.validateTeamInputs(ctx, id, name, []string{}, projects, false)
	if err != nil {
		return storage.Team{}, err
	}

	team, err := p.insertTeam(ctx, id, name, projects)
	if err != nil {
		return storage.Team{}, err
	}

	return team, nil
}

func (p *postgres) validateTeamInputs(ctx context.Context,
	name string, id string, oldProjects, updatedProjects []string, isUpdateRequest bool) error {
	if emptyOrWhitespaceOnlyRE.MatchString(name) {
		return status.Error(
			codes.InvalidArgument,
			"a team id is required and must contain at least one non-whitespace character")
	}
	if emptyOrWhitespaceOnlyRE.MatchString(id) {
		return status.Error(
			codes.InvalidArgument,
			"a team name is required and must contain at least one non-whitespace character")
	}
	_, err := p.authzClient.ValidateProjectAssignment(ctx, &authz.ValidateProjectAssignmentReq{
		Subjects:        auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
		OldProjects:     oldProjects,
		NewProjects:     updatedProjects,
		IsUpdateRequest: isUpdateRequest,
	})
	if err != nil {
		// return error unaltered because it's already a GRPC status code
		return err
	}
	return nil
}

func (p *postgres) insertTeam(ctx context.Context,
	id string, name string, projects []string) (storage.Team, error) {

	var team storage.Team
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO teams (id, name, projects, created_at, updated_at)
		VALUES ($1, $2, $3, now(), now())
		RETURNING id, name, projects, created_at, updated_at`,
		id, name, pq.Array(projects)).
		Scan(&team.ID, &team.Name, pq.Array(&team.Projects), &team.CreatedAt, &team.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return team, nil
}

func (p *postgres) getTeam(ctx context.Context, q querier, id string) (storage.Team, error) {
	var t storage.Team
	err := q.QueryRowContext(ctx,
		`SELECT t.id, t.name, t.projects, t.updated_at, t.created_at
		FROM teams t
		WHERE t.db_id=team_db_id($1)`, id).
		Scan(&t.ID, &t.Name, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return t, nil
}

// DeleteTeam deletes a team from the DB.
func (p *postgres) DeleteTeam(ctx context.Context, id string) (storage.Team, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var t storage.Team
	err = p.db.QueryRowContext(ctx,
		`DELETE FROM teams t
		WHERE t.id=$1 AND projects_match(t.projects, $2::TEXT[])
		RETURNING t.id, t.name, t.projects, t.created_at, t.updated_at`,
		id, pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Name, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

// EditTeamByName edits the team's name and projects
func (p *postgres) EditTeam(ctx context.Context,
	id string, name string, updatedProjects []string) (storage.Team, error) {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if updatedProjects == nil {
		updatedProjects = []string{}
	}

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var oldProjects []string
	err = tx.QueryRowContext(ctx,
		`SELECT projects FROM teams
		WHERE id = $1 AND projects_match(projects, $2::TEXT[])
		FOR UPDATE`,
		id, pq.Array(projectsFilter)).
		Scan(pq.Array(&oldProjects))
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	err = p.validateTeamInputs(ctx, id, name, oldProjects, updatedProjects, true)
	if err != nil {
		return storage.Team{}, err
	}

	var t storage.Team
	err = tx.QueryRowContext(ctx,
		`UPDATE teams t
		SET name = $2, projects = $3, updated_at = now()
		WHERE t.id = $1 AND projects_match(t.projects, $4::TEXT[])
		RETURNING id, name, projects, created_at, updated_at`,
		id, name, pq.Array(updatedProjects), pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Name, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	if err := tx.Commit(); err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

// GetTeams fetches teams from the DB as an array.
func (p *postgres) GetTeams(ctx context.Context) ([]storage.Team, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return []storage.Team{}, p.processError(err)
	}

	var teams []storage.Team
	// TODO eventually these should be ordered
	rows, err := p.db.QueryContext(ctx,
		`SELECT t.id, t.name, t.projects, t.updated_at, t.created_at
		FROM teams t
		WHERE projects_match(t.projects, $1::TEXT[])`,
		pq.Array(projectsFilter))

	if err != nil {
		return []storage.Team{}, p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		team := storage.Team{}
		if err := rows.Scan(&team.ID, &team.Name, pq.Array(&team.Projects),
			&team.CreatedAt, &team.UpdatedAt); err != nil {
			return nil, err // TODO: don't fail it all? handle this more gracefully?
		}
		teams = append(teams, team)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return teams, nil
}

// RemoveUsers deletes teams_users_association rows by teamID and userID, returning the storage team.
func (p *postgres) RemoveUsers(ctx context.Context, id string, userIDs []string) (updatedUserIDs []string, err error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, p.processError(err)
	}

	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
	}

	var dbID int
	var teamProjects []string
	// ensure the team exists and isn't filtered out by the project filter
	err = tx.QueryRowContext(ctx,
		`SELECT db_id, projects FROM teams
		WHERE id = $1 AND projects_match(projects, $2::TEXT[])`,
		id, pq.Array(projectsFilter)).
		Scan(&dbID, pq.Array(&teamProjects))
	if err != nil {
		return nil, p.processError(err)
	}

	_, err = tx.ExecContext(ctx,
		`DELETE FROM teams_users_associations
		WHERE user_id = ANY($1) AND team_db_id=$2`,
		pq.Array(userIDs), dbID)
	if err != nil {
		return []string{}, p.processError(err)
	}

	row := tx.QueryRowContext(ctx,
		`SELECT array_agg(user_id) FROM teams_users_associations WHERE team_db_id=$1`,
		dbID)
	err = row.Scan(pq.Array(&updatedUserIDs))
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, p.processError(err)
	}

	return updatedUserIDs, nil
}

// GetTeam fetches a team by id.
func (p *postgres) GetTeam(ctx context.Context, id string) (storage.Team, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var t storage.Team
	err = p.db.QueryRowContext(ctx,
		`SELECT t.id, t.name, t.projects, t.updated_at, t.created_at
		FROM teams t
		WHERE t.id = $1 AND projects_match(t.projects, $2::TEXT[])`,
		id, pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Name, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return t, nil
}

// PurgeUserMembership removes all teams_users_associations with the userID provided,
// returning an array of teamIDs for the teams that initially had the provided user
func (p *postgres) PurgeUserMembership(ctx context.Context, userID string) ([]string, error) {

	rows, err := p.db.QueryContext(ctx,
		`WITH moved_row_ids AS (
			DELETE FROM teams_users_associations
				WHERE user_id=$1
			RETURNING team_db_id
		)
		UPDATE teams SET updated_at=NOW()
		WHERE db_id in (SELECT * FROM moved_row_ids)
		RETURNING id`, userID)

	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	var teamsIDsUpdated []string
	for rows.Next() {
		var teamID string
		if err := rows.Scan(&teamID); err != nil {
			return nil, p.processError(err)
		}
		teamsIDsUpdated = append(teamsIDsUpdated, teamID)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return teamsIDsUpdated, nil
}

// AddUsers adds users to a team (adding rows to teams_users_associations and updating
// team updated_at)
func (p *postgres) AddUsers(ctx context.Context,
	id string, userIDs []string) (updatedUserIDs []string, err error) {

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, p.processError(err)
	}

	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
	}

	var dbID int
	var teamProjects []string
	// ensure the team exists and isn't filtered out by the project filter
	err = p.db.QueryRowContext(ctx,
		`SELECT db_id, projects FROM teams
		WHERE id = $1 AND projects_match(projects, $2::TEXT[])`,
		id, pq.Array(projectsFilter)).
		Scan(&dbID, pq.Array(&teamProjects))
	if err != nil {
		return nil, p.processError(err)
	}

	_, err = tx.ExecContext(ctx,
		`INSERT INTO teams_users_associations (team_db_id, user_id, created_at)
				SELECT db_id, unnest($2::TEXT[]), now()
	 			FROM teams
				WHERE db_id=$1
				ON CONFLICT ON CONSTRAINT teams_users_pkey
				DO NOTHING
				`, dbID, pq.Array(userIDs))
	if err != nil {
		return nil, p.processError(err)
	}

	_, err = tx.ExecContext(ctx, `UPDATE teams SET updated_at=NOW()
	WHERE db_id=$1`, dbID)
	if err != nil {
		return nil, p.processError(err)
	}

	row := tx.QueryRowContext(ctx,
		`SELECT array_agg(user_id) FROM teams_users_associations WHERE team_db_id=team_db_id($1)`,
		id)
	err = row.Scan(pq.Array(&updatedUserIDs))
	if err != nil {
		return nil, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return nil, p.processError(err)
	}

	return updatedUserIDs, nil
}

// GetUserIDsForTeam returns the user IDs for all members of the team.
func (p *postgres) GetUserIDsForTeam(ctx context.Context, id string) ([]string, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return nil, p.processError(err)
	}

	var userIDs []string
	row := p.db.QueryRowContext(ctx,
		`SELECT array_agg(user_id)
		FROM teams_users_associations INNER JOIN teams
		ON teams.db_id=teams_users_associations.team_db_id
		WHERE id = $1 AND projects_match(projects, $2::TEXT[])`,
		id, pq.Array(projectsFilter))
	err = row.Scan(pq.Array(&userIDs))
	if err != nil {
		return nil, p.processError(err)
	}

	return userIDs, nil
}

// GetTeamsForUser returns an array of teams that have the provided user
func (p *postgres) GetTeamsForUser(ctx context.Context, userID string) ([]storage.Team, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return []storage.Team{}, p.processError(err)
	}

	teams := []storage.Team{}
	rows, err := p.db.QueryContext(ctx,
		`SELECT t.id, t.name, t.projects, t.created_at, t.updated_at
		FROM teams t
		LEFT JOIN teams_users_associations tu ON tu.team_db_id=t.db_id
		WHERE tu.user_id=$1 AND projects_match(t.projects, $2::TEXT[])`,
		userID, pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()
	for rows.Next() {
		t := storage.Team{}
		if err := rows.Scan(&t.ID, &t.Name, pq.Array(&t.Projects),
			&t.CreatedAt, &t.UpdatedAt); err != nil {
			return nil, p.processError(err)
		}
		teams = append(teams, t)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return teams, nil
}

// PurgeProject removes a project from every team it exists in
func (p *postgres) PurgeProject(ctx context.Context, projectID string) error {
	_, err := p.db.ExecContext(ctx, "UPDATE teams SET projects=array_remove(projects, $1)", projectID)
	if err != nil {
		return p.processError(err)
	}
	return nil
}

func (p *postgres) touchTeam(ctx context.Context, q querier, id string) (storage.Team, error) {
	var t storage.Team
	err := q.QueryRowContext(ctx,
		`UPDATE teams
		SET updated_at = now()
		WHERE db_id = team_db_id($1)
		RETURNING id, name, projects, created_at, updated_at`,
		id).
		Scan(&t.ID, &t.Name, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return t, nil
}

// translate lib/pq error into storage.Err*
func parsePQError(e *pq.Error) error {
	// defined in https://github.com/lib/pq/blob/88edab0803230a3898347e77b474f8c1820a1f20/error.go#L78
	switch e.Code.Name() {
	case "unique_violation":
		return storage.ErrConflict
	case "foreign_key_violation":
		// in this piece of code, a foreign key violation means the team the user
		// should be added to doesn't exist; a "not found" seems like an OK
		// approximation for now
		return storage.ErrNotFound
	}

	switch e.Code {
	case "DRPTM": // Our custom error code for non-deletable teams defined in migration 02
		return storage.ErrCannotDelete
	}
	return e
}

// processError is used to translate DB-related errors into the error types
// defined for our storage implementations
func (p *postgres) processError(err error) error {
	if err, ok := err.(*pq.Error); ok {
		return parsePQError(err)
	}
	if err == sql.ErrNoRows {
		return storage.ErrNotFound
	}
	p.logger.Debugf("unknown error type from database: %v", err)
	return err
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
