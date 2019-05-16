package v2

import (
	"fmt"
	"strings"
)

// ConditionOperator is an enum of operators a project rule condition can have.
// This is an enum because we are planning on adding more eventually.
type ConditionOperator int

const (
	MemberOf ConditionOperator = iota
)

func (c ConditionOperator) String() string {
	if c != MemberOf {
		panic(fmt.Sprintf("unknown value from iota ConditionOperator on String() conversion: %d", c))
	}

	return conditionOperatorStringValues()[c]
}

// NewConditionOperator converts a string to a ConditionOperator or returns an error.
func NewConditionOperator(in string) (ConditionOperator, error) {
	switch in {
	case "member-of":
		return MemberOf, nil
	default:
		return MemberOf,
			fmt.Errorf("condition operator type must be one of %q, you passed %s",
				conditionOperatorStringValues(), in)
	}
}

// UnmarshalJSON implements json unmarshalling for a ConditionOperator reference
// so we can pull them out of the database directly as the correct type.
func (c *ConditionOperator) UnmarshalJSON(b []byte) error {
	result, err := NewConditionOperator(strings.Trim(string(b), "\""))
	if err != nil {
		return err
	}
	*c = result
	return nil
}

func conditionOperatorStringValues() [1]string {
	return [...]string{
		"member-of",
	}
}
