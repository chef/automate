package postgres

import (
	"context"
	"database/sql"

	"github.com/lib/pq" // adapter for database/sql
	"github.com/pkg/errors"

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
}

type querier interface {
	QueryRowContext(context.Context, string, ...interface{}) *sql.Row
}

// New instantiates and returns a postgres storage implementation
func New(logger logger.Logger, migrationConfig migration.Config,
	dataMigrationConfig datamigration.Config, isAuthZV2 bool) (storage.Storage, error) {

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

	return &postgres{db, logger, dataMigrationConfig}, nil
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

func (p *postgres) StoreTeam(ctx context.Context, name string, description string) (storage.Team, error) {
	return p.StoreTeamWithProjects(ctx, name, description, []string{})
}

func (p *postgres) StoreTeamWithProjects(ctx context.Context,
	name string, description string, projects []string) (storage.Team, error) {

	// ensure we do not pass null projects to db and break the "not null" constraint
	if len(projects) == 0 {
		projects = []string{}
	}
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

// GetTeam fetches a database team by id and its users, returning a storage team
// with users
func (p *postgres) GetTeam(ctx context.Context, teamID uuid.UUID) (storage.Team, error) {
	return p.getTeam(ctx, p.db, teamID)
}

func (p *postgres) getTeam(ctx context.Context, q querier, teamID uuid.UUID) (storage.Team, error) {
	var t storage.Team
	err := q.QueryRowContext(ctx,
		`SELECT t.id, t.name, t.description, t.projects, t.updated_at, t.created_at
	    FROM teams t WHERE t.id=$1`, teamID).Scan(
		&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects),
		&t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}
	return t, nil
}

func (p *postgres) DeleteTeam(ctx context.Context, teamID uuid.UUID) (storage.Team, error) {
	var t storage.Team
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM teams WHERE id=$1
		RETURNING id, name, description, projects, created_at, updated_at`,
		teamID).Scan(&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

func (p *postgres) DeleteTeamByName(ctx context.Context, teamName string) (storage.Team, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var t storage.Team
	err = p.db.QueryRowContext(ctx,
		`DELETE FROM teams t WHERE t.name=$1 AND projects_match(t.projects, $2::TEXT[])
			RETURNING t.id, t.name, t.description, t.projects, t.created_at, t.updated_at`,
		teamName, pq.Array(projectsFilter)).Scan(
		&t.ID, &t.Name, &t.Description, pq.Array(&t.Projects), &t.CreatedAt, &t.UpdatedAt,
	)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

// EditTeam does a full update on a database team.
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
	teamName string, teamDescription string, teamProjects []string) (storage.Team, error) {

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var t storage.Team
	if teamProjects == nil {
		teamProjects = []string{}
	}

	err = p.db.QueryRowContext(ctx,
		`UPDATE teams t SET
			description = $2, projects = $3, updated_at = now()
		WHERE t.name = $1 AND projects_match(t.projects, $4::TEXT[])
		RETURNING id, name, projects, description, created_at, updated_at;`,
		teamName, teamDescription, pq.Array(teamProjects), pq.Array(projectsFilter)).
		Scan(&t.ID, &t.Name, pq.Array(&t.Projects), &t.Description, &t.CreatedAt, &t.UpdatedAt)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	return t, nil
}

// GetTeams fetches teams from the database, returning an array of storage teams.
func (p *postgres) GetTeams(ctx context.Context) ([]storage.Team, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return []storage.Team{}, p.processError(err)
	}

	var teams []storage.Team
	// TODO eventually these should be ordered
	rows, err := p.db.QueryContext(ctx,
		`SELECT t.id, t.name, t.description, t.projects, t.updated_at, t.created_at FROM teams t WHERE projects_match(t.projects, $1::TEXT[])`,
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
		WHERE user_id = ANY($1) AND team_id = $2`,
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

// GetTeamByName returns the team by name
func (p *postgres) GetTeamByName(ctx context.Context, teamName string) (storage.Team, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	var t storage.Team
	err = p.db.QueryRowContext(ctx,
		`SELECT t.id, t.name, t.description, t.projects, t.updated_at, t.created_at
			FROM teams t WHERE t.name = $1 AND projects_match(t.projects, $2::TEXT[]);`,
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
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, p.processError(err)
	}

	rows, err := tx.QueryContext(ctx,
		`DELETE FROM teams_users_associations
		WHERE user_id=$1
		RETURNING team_id`, userID)
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
			return nil, err // TODO
		}
		teamsIDsUpdated = append(teamsIDsUpdated, teamID)
	}
	// touch affected teams
	for _, teamID := range teamsIDsUpdated {
		_, err := p.touchTeam(ctx, tx, teamID)
		if err != nil {
			return nil, p.processError(err)
		}
	}
	err = tx.Commit()
	if err != nil {
		return nil, p.processError(err)
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
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return storage.Team{}, p.processError(err)
	}

	doneSomething := false
	for _, userID := range userIDs {
		res, err := tx.ExecContext(ctx,
			`INSERT INTO teams_users_associations (team_id, user_id, created_at)
			VALUES ($1, $2, now())`, teamID, userID)
		if err != nil {
			return storage.Team{}, p.processError(err)
		}
		num, err := res.RowsAffected()
		if err != nil {
			return storage.Team{}, p.processError(err)
		}
		if num > 0 {
			doneSomething = true
		}
	}

	var team storage.Team

	if !doneSomething {
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
		`SELECT array_agg(user_id) FROM teams_users_associations WHERE team_id=$1`,
		teamID.String())
	err := row.Scan(pq.Array(&users))
	if err != nil {
		return nil, p.processError(err)
	}
	return users, nil
}

// GetTeamsForUser returns an array of teams that have the provided user
func (p *postgres) GetTeamsForUser(ctx context.Context, userID string) ([]storage.Team, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return []storage.Team{}, p.processError(err)
	}

	teams := []storage.Team{}
	rows, err := p.db.QueryContext(ctx,
		`SELECT t.id, t.name, t.description, t.projects, t.created_at, t.updated_at FROM teams t
			LEFT JOIN teams_users_associations tu ON tu.team_id=t.id
			WHERE projects_match(t.projects, $2::TEXT[])
			GROUP BY t.id`,
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

	return teams, nil
}

func (p *postgres) touchTeam(ctx context.Context, q querier, teamID uuid.UUID) (storage.Team, error) {
	var t storage.Team
	err := q.QueryRowContext(ctx,
		`UPDATE teams
		SET updated_at = now()
		WHERE id = $1
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

// projectsListFromContext returns the project list from the context.
// In the case that the project list was ["*"], we return an empty list,
// since we do not wish to filter on projects.
func projectsListFromContext(ctx context.Context) ([]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		projectsFilter = []string{}
	}
	return projectsFilter, nil
}
