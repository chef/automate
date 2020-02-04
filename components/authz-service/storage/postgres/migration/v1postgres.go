package postgres

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"encoding/json"
	"time"

	// sql driver for postgres
	pq "github.com/lib/pq"
	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage "github.com/chef/automate/components/authz-service/storage/v1"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
)

type pg struct {
	db     *sql.DB
	logger logger.Logger
}

// these correspond to the database format, and get translated into
// storage.Policy instances when returning them
type dbPolicy struct {
	ID         uuid.UUID
	PolicyData policyMap
	Version    int
	CreatedAt  time.Time
	UpdatedAt  pq.NullTime
}

// policyMap is a special type for handling sql's JSONB type
type policyMap struct {
	// Note: we're only using tags because we want the keys lowercased (would
	// otherwise default to e.g. "Subjects")
	Subjects []string `json:"subjects"`
	Action   string   `json:"action"`
	Resource string   `json:"resource"`
	Effect   string   `json:"effect"`
}

// Value marshals our type into JSONB compatible type ([]byte) by
// implementing lib/pq's driver.Valuer interface.
func (m policyMap) Value() (driver.Value, error) {
	return json.Marshal(m)
}

// Scan extends base Scan function from lib/pq
// It transforms JSONB into our type by implementing
// the sql.Scanner interface.
func (m *policyMap) Scan(src interface{}) error {
	// transform the raw data from the database into type we can unmarshal
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}

	return json.Unmarshal(source, m)
}

// New instantiates the postgres IAM v1 storage backend.
func New(ctx context.Context, l logger.Logger, migConf migration.Config) (storage.Storage, error) {
	l.Infof("applying database migrations from %s", migConf.Path)

	db, err := postgres.New(ctx, migConf, datamigration.Config{})
	if err != nil {
		return nil, err
	}

	return &pg{db: db, logger: l}, nil
}

func (p *pg) StorePolicy(
	ctx context.Context,
	action string, subjects []string, resource string, effect string,
) (*storage.Policy, error) {

	pm := policyMap{
		Subjects: subjects,
		Action:   action,
		Resource: resource,
		Effect:   effect,
	}
	id, err := uuid.NewV4()
	if err != nil {
		return nil, err
	}

	pol := dbPolicy{}
	err = p.db.QueryRowContext(ctx,
		`INSERT INTO policies (id, policy_data, version)
					VALUES ($1, $2, $3)
					RETURNING id, policy_data, version, created_at`,
		id, pm, storage.Version,
	).Scan(&pol.ID, &pol.PolicyData, &pol.Version, &pol.CreatedAt)

	if err != nil {
		return nil, p.processError(err)
	}

	return toStoragePolicy(pol), nil
}

func (p *pg) ListPolicies(ctx context.Context) ([]*storage.Policy, error) {
	rows, err := p.db.QueryContext(ctx,
		`SELECT id, policy_data, version, created_at, updated_at FROM policies`)
	if err != nil {
		return nil, p.processError(err)
	}
	defer fileutils.LogClose(rows, p.logger, "could not close rows")

	storagePolicies := []*storage.Policy{}
	for rows.Next() {
		pol := dbPolicy{}
		if err := rows.Scan(&pol.ID, &pol.PolicyData, &pol.Version, &pol.CreatedAt, &pol.UpdatedAt); err != nil {
			return nil, p.processError(err)
		}

		storagePolicies = append(storagePolicies, toStoragePolicy(pol))
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return storagePolicies, nil
}

func ListPoliciesWithSubjects(ctx context.Context) ([]*storage.Policy, error) {
	rows, err := p.db.QueryContext(ctx,
		`SELECT id, policy_data, version, created_at, updated_at FROM policies WHERE policy_data->'subjects' != '[]'`)
	if err != nil {
		return nil, p.processError(err)
	}
	defer fileutils.LogClose(rows, p.logger, "could not close rows")

	storagePolicies := []*storage.Policy{}
	for rows.Next() {
		pol := dbPolicy{}
		if err := rows.Scan(&pol.ID, &pol.PolicyData, &pol.Version, &pol.CreatedAt, &pol.UpdatedAt); err != nil {
			return nil, p.processError(err)
		}

		storagePolicies = append(storagePolicies, toStoragePolicy(pol))
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return storagePolicies, nil
}

func (p *pg) DeletePolicy(ctx context.Context, id string) (*storage.Policy, error) {
	var pol dbPolicy
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM policies WHERE id = $1
		 RETURNING id, policy_data, version, created_at, updated_at`, id,
	).Scan(&pol.ID, &pol.PolicyData, &pol.Version, &pol.CreatedAt, &pol.UpdatedAt)
	if err != nil {
		return nil, p.processError(err)
	}

	return toStoragePolicy(pol), nil
}

func (p *pg) PurgeSubjectFromPolicies(ctx context.Context, sub string) ([]uuid.UUID, error) {
	// Note: without the WHERE clause subsetting the affected rows to only those
	// that really contain this subject, we'd get an UPDATE response indicating
	// that _every_ row was updated. While this is true (the update operation only
	// hasn't changed stuff!), it's not what we want.
	rows, err := p.db.QueryContext(ctx,
		`UPDATE policies
		 SET policy_data=jsonb_set(policy_data, '{subjects}', (policy_data->'subjects') - $1),
		     updated_at=NOW()
		 WHERE policy_data->'subjects' ? $1
		 AND deletable=TRUE
		 RETURNING id`,
		sub,
	)
	if err != nil {
		return nil, p.processError(err)
	}
	defer fileutils.LogClose(rows, p.logger, "could not close rows")

	var polIDs []uuid.UUID
	for rows.Next() {
		id := uuid.UUID{}
		if err := rows.Scan(&id); err != nil {
			return nil, p.processError(err)
		}

		polIDs = append(polIDs, id)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return polIDs, nil
}

func toStoragePolicy(pol dbPolicy) *storage.Policy {
	p := storage.Policy{
		ID:        pol.ID,
		Subjects:  pol.PolicyData.Subjects,
		Action:    pol.PolicyData.Action,
		Resource:  pol.PolicyData.Resource,
		Effect:    pol.PolicyData.Effect,
		Version:   pol.Version,
		CreatedAt: pol.CreatedAt,
	}
	if pol.UpdatedAt.Valid {
		p.UpdatedAt = pol.UpdatedAt.Time
	}
	return &p
}

func (p *pg) processError(err error) error {
	err = postgres.ProcessError(err)
	if err == storage_errors.ErrDatabase {
		p.logger.Debugf("unknown error type from database: %v", err)
	}
	return err
}
