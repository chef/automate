package v2

import (
	"encoding/json"

	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
)

// Rule defines an ingest rule for a project.
type Rule struct {
	ID         string      `json:"id"`
	ProjectID  string      `json:"project_id"`
	Name       string      `json:"name"`
	Type       RuleType    `json:"type"`
	Conditions []Condition `json:"conditions"`
	Deleted    bool        `json:"deleted"`
	Status     string      `json:"status"`
}

// Scan implements pq Scan interface for a Rule reference
// so we can pull them out of the database directly as the correct type.
func (p *Rule) Scan(src interface{}) error {
	if src == nil {
		return storage_errors.ErrNotFound
	}
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}
	return json.Unmarshal(source, p)
}

// NewRule is a factory for creating a Rule storage object that also does
// validation around what a valid rule is in terms of our storage layer.
func NewRule(id string, projectID string, name string,
	ruleType RuleType, conditions []Condition) (Rule, error) {

	err := validateRuleInputs(id, projectID, name, ruleType, conditions)
	if err != nil {
		return Rule{}, err
	}

	return Rule{
		ID:         id,
		ProjectID:  projectID,
		Name:       name,
		Type:       ruleType,
		Conditions: conditions,
		Deleted:    false,
	}, nil
}

func validateRuleInputs(id string,
	projectID string, name string, ruleType RuleType, conditions []Condition) error {

	if id == "" {
		return errors.New("a rule must have an id")
	}

	if projectID == "" {
		return errors.New("a rule must have an associated project_id")
	}

	if name == "" {
		return errors.New("a rule must have a name")
	}

	if len(conditions) == 0 {
		return errors.New("a rule must contain at least one condition")
	}

	for _, condition := range conditions {
		if condition.Type != ruleType {
			return errors.New("all condition types must match the parent rule type")
		}
	}

	return nil
}
