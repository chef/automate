package v2

import (
	"encoding/json"

	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
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
		return storage_errors.ErrNotFound
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

	err := validatePolicyInputs(id, name, projects)
	if err != nil {
		return Policy{}, err
	}

	return Policy{
		ID:         id,
		Name:       name,
		Type:       typeVal,
		Members:    members,
		Statements: statements,
		Projects:   projects,
	}, nil
}

func validatePolicyInputs(id string, name string, projects []string) error {
	if emptyOrWhitespaceOnlyRE.MatchString(id) {
		return errors.New("a policy id must contain non-whitespace characters")
	}
	if emptyOrWhitespaceOnlyRE.MatchString(name) {
		return errors.New("a policy name must contain non-whitespace characters")
	}
	err := ValidateProjects(projects)
	if err != nil {
		return err
	}
	return nil
}
