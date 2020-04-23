package storage

import (
	"encoding/json"

	"github.com/pkg/errors"
)

// Policy represents a policy definition to be persisted to storage.
type Policy struct {
	ID         string      `json:"id"`
	Name       string      `json:"name"`
	Members    []Member    `json:"members"`
	Statements []Statement `json:"statements"`
	Type       Type        `json:"type"`
	Projects   []string    `json:"projects"`
}

// Scan implements pq Scan interface for an Policy reference
// so we can pull them out of the database directly as the correct type.
func (p *Policy) Scan(src interface{}) error {
	if src == nil {
		return ErrNotFound
	}
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}
	return json.Unmarshal(source, p)
}

// NewPolicy is a factory for creating a Policy storage object that also does
// validation around what a valid policy is in terms of our storage layer.
func NewPolicy(
	id string,
	name string,
	typeVal Type,
	members []Member,
	statements []Statement,
	projects []string,
) (Policy, error) {

	return Policy{
		ID:         id,
		Name:       name,
		Type:       typeVal,
		Members:    members,
		Statements: statements,
		Projects:   projects,
	}, nil
}
