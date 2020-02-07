package legacy

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"encoding/json"
	"time"

	// sql driver for postgres
	pq "github.com/lib/pq"
	"github.com/pkg/errors"

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

func listPoliciesWithSubjects(ctx context.Context, db *sql.DB) ([]*v1Policy, error) {
	rows, err := db.QueryContext(ctx,
		`SELECT id, policy_data, version, created_at, updated_at FROM policies WHERE policy_data->'subjects' != '[]'`)
	if err != nil {
		return nil, errors.Wrap(err, "v1 listPoliciesWithSubjects")
	}
	defer rows.Close()

	storagePolicies := []*v1Policy{}
	for rows.Next() {
		pol := dbPolicy{}
		if err := rows.Scan(&pol.ID, &pol.PolicyData, &pol.Version, &pol.CreatedAt, &pol.UpdatedAt); err != nil {
			return nil, errors.Wrap(err, "v1 listPoliciesWithSubjects")
		}

		storagePolicies = append(storagePolicies, toStoragePolicy(pol))
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return storagePolicies, nil
}

func toStoragePolicy(pol dbPolicy) *v1Policy {
	p := v1Policy{
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
