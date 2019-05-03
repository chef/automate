package v2

import (
	"fmt"
	"strings"

	"github.com/pkg/errors"

	constants "github.com/chef/automate/components/authz-service/constants/v2"
)

func ValidateProjects(projects []string) error {
	for _, project := range projects {
		if project == constants.UnassignedProjectID {
			return errors.Errorf("%q cannot explicitly be set. "+
				"If you wish to create an object in %q, you should pass no projects on creation.",
				constants.UnassignedProjectID, constants.UnassignedProjectID)
		}
	}
	return nil
}

// Type is an enum to denote custom or chef-managed policy.
type Type int

const (
	// Custom represents a policy created by the enduser.
	Custom Type = iota
	// ChefManaged represents a policy created by Chef Software.
	ChefManaged
	// System represents an underlying policy that allows Automate to function correctly.
	// System
)

const (
	customTypeString  = "custom"
	managedTypeString = "chef-managed"
)

var strValues = [...]string{
	customTypeString,
	managedTypeString,
}

func (t Type) String() string {
	if t < Custom || t > ChefManaged {
		panic(fmt.Sprintf("unknown value from iota Type on String() conversion: %d", t))
	}

	return strValues[t]
}

// NewType converts a string to a Type or returns an error.
func NewType(in string) (Type, error) {
	switch in {
	case customTypeString:
		return Custom, nil
	case managedTypeString:
		return ChefManaged, nil
	default:
		return Custom, fmt.Errorf("policy type must be one of %q, you passed %q", strValues, in)
	}
}

// UnmarshalJSON implements json unmarshalling for a Type reference
// so we can pull them out of the database directly as the correct type.
func (t *Type) UnmarshalJSON(b []byte) error {
	// After byte conversion, things coming out of db as
	// customTypeString or managedTypeString.
	result, err := NewType(strings.Trim(string(b), "\""))
	if err != nil {
		return err
	}
	*t = result
	return nil
}
