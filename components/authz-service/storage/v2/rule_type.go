package v2

import (
	"fmt"
	"strings"
)

// RuleType is an enum of the types a project rule can be.
type RuleType int

const (
	Node RuleType = iota
	Event
)

func (r RuleType) String() string {
	if r < Node || r > Event {
		panic(fmt.Sprintf("unknown value from iota RuleType on String() conversion: %d", r))
	}

	return ruleTypeStringValues()[r]
}

// NewRuleType converts a string to a RuleType or returns an error.
func NewRuleType(in string) (RuleType, error) {
	switch in {
	case "node":
		return Node, nil
	case "event":
		return Event, nil
	default:
		return Node,
			fmt.Errorf("rule type must be one of %q, you passed %s",
				ruleTypeStringValues(), in)
	}
}

// UnmarshalJSON implements json unmarshalling for an RuleType reference
// so we can pull them out of the database directly as the correct type.
func (r *RuleType) UnmarshalJSON(b []byte) error {
	// After byte conversion, things coming out of db as
	// '"node"' and '"event"'.
	result, err := NewRuleType(strings.Trim(string(b), "\""))
	if err != nil {
		return err
	}
	*r = result
	return nil
}

func ruleTypeStringValues() [2]string {
	return [...]string{
		"node",
		"event",
	}
}
