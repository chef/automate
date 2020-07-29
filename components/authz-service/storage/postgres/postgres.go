package postgres

import (
	"context"
	"database/sql"
	"strings"
	"sync"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	"github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
)

// New returns a new sql.DB connector with the automatic default migrations applied.
func New(ctx context.Context, migConf migration.Config, dataMigConf datamigration.Config) (*sql.DB, error) {
	pgURL := migConf.PGURL.String()

	if err := migConf.Migrate(dataMigConf); err != nil {
		return nil, errors.Wrapf(
			err,
			"apply database migrations from %s",
			migConf.Path,
		)
	}

	db, err := initPostgresDB(ctx, pgURL, migConf.MaxConnections, migConf.MaxIdleConnections)
	if err != nil {
		return nil, errors.Wrapf(
			err,
			"connect to database at %s",
			pgURL,
		)
	}
	return db, nil
}

func initPostgresDB(ctx context.Context, pgURL string, maxConnections int, maxIdleConnections int) (*sql.DB, error) {
	d, err := db.PGOpenContext(ctx, pgURL)
	if err != nil {
		return nil, err
	}

	if maxConnections > 0 {
		d.SetMaxOpenConns(maxConnections)
	}

	if maxIdleConnections > 0 {
		d.SetMaxIdleConns(maxIdleConnections)
	}

	if err := d.PingContext(ctx); err != nil {
		return nil, err
	}

	return d, nil
}

// ProcessError is used to translate DB-related errors into the error types
// defined for our storage implementations.
func ProcessError(err error) error {
	if err == sql.ErrNoRows {
		return storage.ErrNotFound
	}

	// The not found on json unmarshall case
	if strings.HasPrefix(err.Error(), "sql: Scan error on column index 0") &&
		strings.HasSuffix(err.Error(), "not found") {
		return storage.ErrNotFound
	}

	if err, ok := err.(*pq.Error); ok {
		return parsePQError(err)
	}

	return err
}

// parsePQError is able to parse pq specific errors into storage interface errors.
func parsePQError(e *pq.Error) error {
	switch e.Code {
	case "DRPPC": // Custom code: attempt to delete a non-deletable policy defined in migration 02
		return storage.ErrCannotDelete
	case "23505": // Unique violation
		return storage.ErrConflict
	case "23503": // Foreign key violation
		return &storage.ForeignKeyError{Msg: e.Message}
	case "P0002": // Not found in plpgsql ("no_data_found")
		return storage.ErrNotFound
	case "20000": // Not found
		return storage.ErrNotFound
	case "PRJTR": // Custom code: attempt to change a rule's projects that are immutable
		return storage.ErrChangeProjectForRule
	case "RDLTD": // Custom code: attempt to update a rule that is staged for deletion
		return storage.ErrMarkedForDeletion
	case "RLTYP": // Custom code: attempt to update a rule's type that is immutable
		return storage.ErrChangeTypeForRule
	}

	return storage.ErrDatabase
}

type pg struct {
	db           *sql.DB
	engine       engine.Engine
	logger       logger.Logger
	dataMigConf  datamigration.Config
	conninfo     string
	projectLimit int
}

var singletonInstance *pg
var once sync.Once

type Querier interface {
	ExecContext(context.Context, string, ...interface{}) (sql.Result, error)
	QueryContext(context.Context, string, ...interface{}) (*sql.Rows, error)
	QueryRowContext(context.Context, string, ...interface{}) *sql.Row
}

// GetInstance returns the singleton instance. Will be nil if not yet initialized.
func GetInstance() *pg {
	return singletonInstance
}

// Initialize instantiates the singleton postgres storage backend.
// Will only initialize once. Will simply return nil if already initialized.
func Initialize(ctx context.Context, e engine.Engine, l logger.Logger, migConf migration.Config,
	dataMigConf datamigration.Config, projectLimit int) error {

	var err error
	once.Do(func() {
		l.Infof("applying database migrations from %s", migConf.Path)
		var db *sql.DB
		db, err = New(ctx, migConf, dataMigConf)
		singletonInstance = &pg{
			db:           db,
			engine:       e,
			logger:       l,
			dataMigConf:  dataMigConf,
			conninfo:     migConf.PGURL.String(),
			projectLimit: projectLimit}
	})
	return err
}

func (p *pg) notifyPolicyChange(ctx context.Context, q Querier) error {
	// We keep track of an id with each change. This lets us be smart about only updating
	// the OPA rules when it might change.
	_, err := q.ExecContext(ctx, "SELECT notify_policy_change()")
	return err
}

func (p *pg) Reset(ctx context.Context) error {
	if _, err := p.db.ExecContext(ctx,
		`TRUNCATE TABLE iam_policies, iam_members, iam_roles, iam_projects CASCADE;`); err != nil {
		return errors.Wrap(err, "truncate database")
	}

	if err := p.dataMigConf.Reset(); err != nil {
		return errors.Wrap(err, "reset v2 data migrations")
	}

	return nil
}

func (p *pg) Close() error {
	err := errors.Wrap(p.db.Close(), "close database connection")
	// reset the singleton
	once = *new(sync.Once)
	singletonInstance = nil
	return err
}

func (p *pg) singleRowResultOrNotFoundErr(result sql.Result) error {
	count, err := result.RowsAffected()
	if err != nil {
		return p.processError(err)
	}
	if count == 0 {
		return storage.ErrNotFound
	}
	if count > 1 {
		return storage.ErrDatabase
	}
	return nil
}

func (p *pg) getMapOfRuleAssociations(ctx context.Context, q Querier, id string, projectID string) (map[string]bool, error) {
	assocRow := q.QueryRowContext(ctx, "SELECT query_rule_table_associations($1, $2)", id, projectID)
	var associations []string
	if err := assocRow.Scan(pq.Array(&associations)); err != nil {
		return nil, err
	}

	set := make(map[string]bool, len(associations))
	for _, s := range associations {
		set[s] = true
	}
	return set, nil
}

func (p *pg) processError(err error) error {
	p.logger.Debugf("err: %v", err)
	err = ProcessError(err)
	if err == storage.ErrDatabase {
		p.logger.Warnf("unknown error type from database: %v", err)
	}
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
