package storage

import (
	"encoding/json"

	"github.com/pkg/errors"
)

// Project represents a project definition to be persisted to storage.
type Project struct {
	ID     string `json:"id"`
	Name   string `json:"name"`
	Type   Type   `json:"type"`
	Status string `json:"status"`
}

// Scan implements pq Scan interface for a Project reference
// so we can pull them out of the database directly as the correct type.
func (p *Project) Scan(src interface{}) error {
	if src == nil {
		return ErrNotFound
	}
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}
	return json.Unmarshal(source, p)
}

// NewProject is a factory for creating a Project storage object that also does
// validation around what a valid project is in terms of our storage layer.
func NewProject(
	id string,
	name string,
	typeVal Type,
	status ProjectRulesStatus) (Project, error) {

	return Project{
		ID:     id,
		Name:   name,
		Type:   typeVal,
		Status: status.String(),
	}, nil
}
