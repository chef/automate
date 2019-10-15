package postgres

import (
	"context"
	"database/sql"

	"github.com/lib/pq" // adapter for database/sql
	"github.com/pkg/errors"

	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
)

// WARNING
// TODO (tc): The storage interface is still using V1 verbiage, so
// name is really the ID in V2 terms. We'll refactor at GA when V1 is removed.
// Also, project filtering has only been implemented for storage functions
// that are used by the V2 server.

// TODOs
// - CREATE EXTENSION "uuid-ossp"; (currently done via Makefile for tests)

type postgres struct {
	db                  *sql.DB
	logger              logger.Logger
	dataMigrationConfig datamigration.Config
	authzClient         authz_v2.AuthorizationClient
}

type querier interface {
	QueryRowContext(context.Context, string, ...interface{}) *sql.Row
}

// New instantiates and returns a postgres storage implementation
func New(logger logger.Logger, migrationConfig migration.Config,
	dataMigrationConfig datamigration.Config, isAuthZV2 bool,
	authzClient authz_v2.AuthorizationClient) (storage.Storage, error) {

	if err := migrationConfig.Migrate(); err != nil {
		return nil, errors.Wrap(err, "database migrations")
	}

	if isAuthZV2 {
		if err := dataMigrationConfig.Migrate(); err != nil {
			return nil, errors.Wrap(err, "database v2 data migrations")
		}
	}

	db, err := initPostgresDB(migrationConfig.PGURL.String())
	if err != nil {
		return nil, errors.Wrap(err, "initialize database")
	}

	return &postgres{db, logger, dataMigrationConfig, authzClient}, nil
}

func initPostgresDB(pgURL string) (*sql.DB, error) {
	db, err := sql.Open("postgres", pgURL)
	if err != nil {
		return nil, err
	}

	if err := db.Ping(); err != nil {
		return nil, errors.Wrap(err, "opening database connection")
	}

	return db, nil
}

func (p *postgres) UpgradeToV2(ctx context.Context) error {
	if err := p.dataMigrationConfig.Migrate(); err != nil {
		return errors.Wrap(err, "database v2 data migrations")
	}
	return nil
}

// StoreTeam saves a team to the DB. This is used by IAM v1 ONLY!
func (p *postgres) StoreTeam(ctx context.Context, name string, description string) (storage.Team, error) {
	return p.insertTeam(ctx, name, description, []string{})
}

// StoreTeam saves a team to the DB.
func (p *postgres) StoreTeamWithProjects(ctx context.Context,
	name string, description string, projects []string) (storage.Team, error) {

	// ensure we do not pass null projects to db and break the "not null" constraint
	if len(projects) == 0 {
		projects = []string{}
	} else {
		// will only return an error if authz is in v2.1 mode
		_, err := p.authzClient.ValidateProjectAssignment(ctx, &authz_v2.ValidateProjectAssignmentReq{
			Subjects:    auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
			NewProjects: []string{},
			OldProjects: projects,
		})
		if err != nil {
			// return error unaltered because it's already a GRPC status code
			return storage.Team{}, err
		}
	}

	team, err := p.insertTeam(ctx, name, description, projects)
	if err != nil {
		return storage.Team{}, err
	}

	return team, nil
}

func (p *postgres) insertTeam(ctx context.Context,
	name string, description string, projects []string) (storage.Team, error) {

	var team storage.Team
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO teams (id, name, description, projects, created_at, updated_at)
		VALUES (uuid_generate_v4(), $1, $2, $3, now(), now())
		RETURNING id, name, projects, description, created_at, updated_at`,
		name, description, pq.Array(projects)).
		Scan(&team.ID, &team.Name, pq.Array(&team.Projects), &team.Description, &team.CreatedAt, &team.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return team, nil
}

// GetTeam fetches a team by id. This is used by IAM v1 ONLY!
func (p *postgres) GetTeam(ctx context.Context, teamID uuid.UUID) (storage.Team, error) {
	return p.getTeam(ctx, p.db, teamID)
}

func (p *postgres) getTeam(ctx context.Context, q querier, teamID uuid.UUID) (storage.Team, error) {
	var t storage.Team
	err := q.QueryRowContext(ctx,
		`SELECT t.id, t.name, t.description, t.projects, t.updated_at, t.created_at
		FROM teams t
		WHERE t.db_id=team_db_id($1)`, teamID).
		Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return t, nil
}

// DeleteTeam deletes a team from the DB. This is used by IAM v1 ONLY!
func (p *postgres) DeleteTeam(ctx context.Context, teamID uuid.UUID) (storage.Team, error) {
	var t storage.Team
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM teams WHERE id=$1
		RETURNING id, name, description, projects, created_at, updated_at`, teamID).
		Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

// DeleteTeam deletes a team from the DB.
func (p *postgres) DeleteTeamByName(ctx context.Context, teamName string) (storage.Team, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var t storage.Team
	err = p.db.QueryRowContext(ctx,
		`DELETE FROM teams t
		WHERE t.name=$1 AND projects_match(t.projects, $2::TEXT[])
		RETURNING t.id, t.name, t.description, t.projects, t.created_at, t.updated_at`,
		teamName, pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

// EditTeam does a full update on a database team. This is used by IAM v1 ONLY!
func (p *postgres) EditTeam(ctx context.Context, team storage.Team) (storage.Team, error) {
	var t storage.Team
	// ensure we do not pass null projects to db
	if team.Projects == nil {
		team.Projects = []string{}
	}
	err := p.db.QueryRowContext(ctx,
		`UPDATE teams
		SET name = $2, description = $3, projects = $4, updated_at = now()
		WHERE id = $1
		RETURNING id, name, description, projects, created_at, updated_at`,
		team.ID, team.Name, team.Description, pq.Array(team.Projects)).
		Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

// EditTeamByName edits the team's description and projects by name
func (p *postgres) EditTeamByName(ctx context.Context,
	teamName string, teamDescription string, updatedProjects []string) (storage.Team, error) {

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
		WHERE name = $1 AND projects_match(projects, $2::TEXT[])
		FOR UPDATE;`,
		teamName, pq.Array(projectsFilter)).
		Scan(pq.Array(&oldProjects))
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	// will only return an error if authz is in v2.1 mode
	_, err = p.authzClient.ValidateProjectAssignment(ctx, &authz_v2.ValidateProjectAssignmentReq{
		Subjects:        auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
		OldProjects:     oldProjects,
		NewProjects:     updatedProjects,
		IsUpdateRequest: true,
	})
	if err != nil {
		// return error unaltered because it's already a GRPC status code
		return storage.Team{}, err
	}

	var t storage.Team
	err = tx.QueryRowContext(ctx,
		`UPDATE teams t
		SET description = $2, projects = $3, updated_at = now()
		WHERE t.name = $1 AND projects_match(t.projects, $4::TEXT[])
		RETURNING id, name, projects, description, created_at, updated_at;`,
		teamName, teamDescription, pq.Array(updatedProjects), pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Name, pq.Array(&t.Projects), &t.Description, &t.CreatedAt, &t.UpdatedAt)
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
		`SELECT t.id, t.name, t.description, t.projects, t.updated_at, t.created_at
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
		if err := rows.Scan(&team.ID, &team.Name, &team.Description, pq.Array(&team.Projects),
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
//
//
// NOTE: We aren't filtering on projects here for expediency since the server call
// is already calling GetTeamByName which does project filtering and will bail in the
// server code before we get here if the team in question was filtered, but if we call this
// from a different context we should apply project filtering in this function as well.
// Not doing it to save us a database call since it's not needed currently.
func (p *postgres) RemoveUsers(ctx context.Context, teamID uuid.UUID, userIDs []string) (storage.Team, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	res, err := tx.ExecContext(ctx,
		`DELETE FROM teams_users_associations
		WHERE user_id = ANY($1) AND team_db_id = team_db_id($2)`,
		pq.Array(userIDs), teamID)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	num, err := res.RowsAffected()
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var team storage.Team
	if num == 0 { // no changes
		team, err = p.getTeam(ctx, tx, teamID)
	} else {
		team, err = p.touchTeam(ctx, tx, teamID)
	}
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return team, nil
}

// GetTeamByName fetches a team by name.
func (p *postgres) GetTeamByName(ctx context.Context, teamName string) (storage.Team, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var t storage.Team
	err = p.db.QueryRowContext(ctx,
		`SELECT t.id, t.name, t.description, t.projects, t.updated_at, t.created_at
		FROM teams t
		WHERE t.name = $1 AND projects_match(t.projects, $2::TEXT[]);`,
		teamName, pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return t, nil
}

// PurgeUserMembership removes all teams_users_associations with the userID provided,
// returning an array of teamIDs for the teams that initially had the provided user
func (p *postgres) PurgeUserMembership(ctx context.Context, userID string) ([]uuid.UUID, error) {
	rows, err := p.db.QueryContext(ctx,
		`WITH moved_row_ids AS (
			DELETE FROM teams_users_associations
				WHERE user_id=$1
			RETURNING team_db_id
		)
		UPDATE teams SET updated_at=NOW()
		WHERE db_id in (SELECT * FROM moved_row_ids)
		RETURNING id;`, userID)

	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	var teamsIDsUpdated []uuid.UUID
	for rows.Next() {
		var teamID uuid.UUID
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
//
// NOTE: We aren't filtering on projects here for expediency since the server call
// is already calling GetTeamByName which does project filtering and will bail in the
// server code before we get here if the team in question was filtered, but if we call this
// from a different context we should apply project filtering in this function as well.
// Not doing it to save us a database call since it's not needed currently.
func (p *postgres) AddUsers(ctx context.Context,
	teamID uuid.UUID,
	userIDs []string) (storage.Team, error) {
	var t storage.Team
	err := p.db.QueryRowContext(ctx,
		`WITH moved_row_ids AS (
			INSERT INTO teams_users_associations (team_db_id, user_id, created_at)
				SELECT db_id, unnest($2::TEXT[]), now()
				FROM teams
				WHERE id=$1
			RETURNING team_db_id
		)
		UPDATE teams SET updated_at=NOW()
		WHERE db_id in (SELECT DISTINCT * FROM moved_row_ids)
		RETURNING id, name, description, projects, created_at, updated_at;`, teamID, pq.Array(userIDs)).Scan(
		&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return t, nil
}

// GetUserIDsForTeam returns the user IDs for all members of the team.
//
// NOTE: We aren't filtering on projects here for expediency since the server call
// is already calling GetTeamByName which does project filtering and will bail in the
// server code before we get here if the team in question was filtered, but if we call this
// from a different context we should apply project filtering in this function as well.
// Not doing it to save us a database call since it's not needed currently.
func (p *postgres) GetUserIDsForTeam(ctx context.Context, teamID uuid.UUID) ([]string, error) {
	var users []string
	row := p.db.QueryRowContext(ctx,
		`SELECT array_agg(user_id) FROM teams_users_associations WHERE team_db_id=team_db_id($1)`,
		teamID.String())
	err := row.Scan(pq.Array(&users))
	if err != nil {
		return nil, p.processError(err)
	}
	return users, nil
}

// GetTeamsForUser returns an array of teams that have the provided user
func (p *postgres) GetTeamsForUser(ctx context.Context, userID string) ([]storage.Team, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return []storage.Team{}, p.processError(err)
	}

	teams := []storage.Team{}
	rows, err := p.db.QueryContext(ctx,
		`SELECT t.id, t.name, t.description, t.projects, t.created_at, t.updated_at
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
		if err := rows.Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects),
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

func (p *postgres) touchTeam(ctx context.Context, q querier, teamID uuid.UUID) (storage.Team, error) {
	var t storage.Team
	err := q.QueryRowContext(ctx,
		`UPDATE teams
		SET updated_at = now()
		WHERE db_id = team_db_id($1)
		RETURNING id, name, description, projects, created_at, updated_at`,
		teamID).
		Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
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
