package storage

import (
	"encoding/json"

	"github.com/pkg/errors"
)

// Role represents a role definition to be persisted to storage.
type Role struct {
	ID       string   `json:"id"`
	Name     string   `json:"name"`
	Actions  []string `json:"actions"`
	Type     Type     `json:"type"`
	Projects []string `json:"projects"`
}

// Scan implements pq Scan interface for a Role reference
// so we can pull them out of the database directly as the correct type.
func (p *Role) Scan(src interface{}) error {
	if src == nil {
		return ErrNotFound
	}
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}
	return json.Unmarshal(source, p)
}

// NewRole is a factory for creating a Role storage object that also does
// validation around what a valid role is in terms of our storage layer.
func NewRole(id string, name string, typeVal Type, actions []string, projects []string) (*Role, error) {
	role := &Role{
		ID:       id,
		Name:     name,
		Type:     typeVal,
		Actions:  actions,
		Projects: projects,
	}

	return role, nil
}

// NewUpdateRole is a factory for modifying an existing role.
func NewUpdateRole(id string, name string, actions []string, projects []string) (*Role, error) {
	role := &Role{
		ID:       id,
		Name:     name,
		Actions:  actions,
		Projects: projects,
	}

	return role, nil
}
