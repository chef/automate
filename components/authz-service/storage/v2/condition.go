package v2

import (
	"encoding/json"

	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
)

// Condition defines a condition for an ingest rule for a project.
type Condition struct {
	Value     []string           `json:"value"`
	Attribute ConditionAttribute `json:"attribute"`
	Operator  ConditionOperator  `json:"operator"`
	Type      RuleType           `json:"type"`
}

// Scan implements pq Scan interface for a Condition reference
// so we can pull them out of the database directly as the correct type.
func (p *Condition) Scan(src interface{}) error {
	if src == nil {
		return storage_errors.ErrNotFound
	}
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}
	return json.Unmarshal(source, p)
}

// NewCondition is a factory for creating a Condition storage object that also does
// validation around what a valid condition is in terms of our storage layer.
func NewCondition(ruleType RuleType, value []string,
	attribute ConditionAttribute, operator ConditionOperator) (Condition, error) {

	err := validateConditionInputs(value, attribute, ruleType, operator)
	if err != nil {
		return Condition{}, err
	}

	return Condition{
		Value:     value,
		Attribute: attribute,
		Operator:  operator,
		Type:      ruleType,
	}, nil
}

func validateConditionInputs(value []string,
	attribute ConditionAttribute, ruleType RuleType, operator ConditionOperator) error {

	if len(value) == 0 {
		return errors.New("a condition must specify one or more values")
	}

	if operator == Equals {
		if len(value) != 1 {
			return errors.New("a condition must specify exactly one value for 'equals' operator")
		}
	}

	if ruleType == Event {
		if attribute != Organization && attribute != ChefServer {
			return errors.New("rules of type Event only accept Conditions with attributes Organization or ChefServer")
		}
	}

	return nil
}
