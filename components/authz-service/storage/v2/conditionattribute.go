package v2

import (
	"fmt"
	"strings"
)

// ConditionAttribute is an enum of attributes a project rule condition can be.
type ConditionAttribute int

const (
	Organization ConditionAttribute = iota
	ChefServer
	Environment
	ChefRole
	ChefTag
	PolicyName
	PolicyGroup
)

func (c ConditionAttribute) String() string {
	if c < Organization || c > PolicyGroup {
		panic(fmt.Sprintf("unknown value from iota ConditionAttribute on String() conversion: %d", c))
	}

	return conditionAttributeStringValues()[c]
}

// NewConditionAttribute converts a string to an ConditionAttribute or returns an error.
func NewConditionAttribute(in string) (ConditionAttribute, error) {
	switch in {
	case "organization":
		return Organization, nil
	case "chef-server":
		return ChefServer, nil
	case "environment":
		return Environment, nil
	case "role":
		return ChefRole, nil
	case "chef-tag":
		return ChefTag, nil
	case "policy-name":
		return PolicyName, nil
	case "policy-group":
		return PolicyGroup, nil
	default:
		return Organization,
			fmt.Errorf("condition attribute type must be one of %q, you passed %s",
				conditionAttributeStringValues(), in)
	}
}

// UnmarshalJSON implements json unmarshalling for an RuleType reference
// so we can pull them out of the database directly as the correct type.
func (c *ConditionAttribute) UnmarshalJSON(b []byte) error {
	// After byte conversion, things coming out of db as
	// '"node"' and '"event"'.
	result, err := NewConditionAttribute(strings.Trim(string(b), "\""))
	if err != nil {
		return err
	}
	*c = result
	return nil
}

func conditionAttributeStringValues() [7]string {
	return [...]string{
		"organization",
		"chef-server",
		"environment",
		"role",
		"chef-tag",
		"policy-name",
		"policy-group",
	}
}
