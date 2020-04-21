package storage

import (
	"encoding/json"

	"github.com/pkg/errors"
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
		return ErrNotFound
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

	err := validateConditionsForType(ruleType, conditions)
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

func validateConditionsForType(ruleType RuleType, conditions []Condition) error {
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
