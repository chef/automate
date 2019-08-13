package v2

import (
	"encoding/json"
	"regexp"

	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
)

// Project represents a project definition to be persisted to storage.
type Project struct {
	ID     string `json:"id"`
	Name   string `json:"name"`
	Type   Type   `json:"type"`
	Status string `json:"status"`
}

// Note: also catches empty names ("")
var emptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

// Scan implements pq Scan interface for a Project reference
// so we can pull them out of the database directly as the correct type.
func (p *Project) Scan(src interface{}) error {
	if src == nil {
		return storage_errors.ErrNotFound
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
	if emptyOrWhitespaceOnlyRE.MatchString(name) {
		return Project{}, errors.New("a project name must contain non-whitespace characters")
	}
	if emptyOrWhitespaceOnlyRE.MatchString(id) {
		return Project{}, errors.New("a project id must contain non-whitespace characters")
	}

	return Project{
		ID:     id,
		Name:   name,
		Type:   typeVal,
		Status: status.String(),
	}, nil
}
