package v2

import (
	"fmt"
	"strings"
)

// ConditionOperator is an enum of operators a project rule condition can have.
type ConditionOperator int

const (
	Equals ConditionOperator = iota
	MemberOf
)

func (c ConditionOperator) String() string {
	if c < Equals || c > MemberOf {
		panic(fmt.Sprintf("unknown value from iota ConditionOperator on String() conversion: %d", c))
	}

	return conditionOperatorStringValues()[c]
}

// NewConditionOperator converts a string to a ConditionOperator or returns an error.
func NewConditionOperator(in string) (ConditionOperator, error) {
	switch in {
	case "equals":
		return Equals, nil
	case "member-of":
		return MemberOf, nil
	default:
		return Equals,
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

func conditionOperatorStringValues() [2]string {
	return [...]string{
		"equals",
		"member-of",
	}
}
