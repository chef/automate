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

	if emptyOrWhitespaceOnlyRE.MatchString(id) {
		return errors.New(
			"a rule id is required and must contain at least one non-whitespace character")
	}
	if emptyOrWhitespaceOnlyRE.MatchString(name) {
		return errors.New(
			"a rule name is required and must contain at least one non-whitespace character")
	}
	if emptyOrWhitespaceOnlyRE.MatchString(projectID) {
		return errors.New("a rule id must have an associated project_id")
	}
	if len(conditions) == 0 {
		return errors.New("a rule must contain at least one condition")
	}

	for _, condition := range conditions {
		if err := attributeAllowed(ruleType, condition.Attribute); err != nil {
			return err
		}
	}

	return nil
}

func attributeAllowed(ruleType RuleType, attr ConditionAttribute) error {
	if ruleType == Event {
		if attr != Organization && attr != ChefServer {
			return errors.New("rules of type Event only accept Conditions with attributes Organization or ChefServer")
		}
	}

	// all attributes allowed for Node type
	return nil
}
