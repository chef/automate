package v2

import (
	"fmt"
	"strings"
)

// Effect is an enum of allow or deny for use in Statements.
type Effect int

const (
	// Allow represents the allow case for a Statement Effect.
	Allow Effect = iota
	// Deny represents the deny case for a Statement Effect.
	Deny
)

func (e Effect) String() string {
	strValues := [...]string{
		"allow",
		"deny",
	}

	if e < Allow || e > Deny {
		panic(fmt.Sprintf("unknown value from iota Effect on String() conversion: %d", e))
	}

	return strValues[e]
}

// NewEffect converts a string to an Effect or returns an error.
func NewEffect(in string) (Effect, error) {
	switch in {
	case "allow":
		return Allow, nil
	case "deny":
		return Deny, nil
	default:
		return Allow, fmt.Errorf("effect must be one of 'allow' or 'deny', you passed %s", in)
	}
}

// UnmarshalJSON implements json unmarshalling for an Effect reference
// so we can pull them out of the database directly as the correct type.
func (e *Effect) UnmarshalJSON(b []byte) error {
	// After byte conversion, things coming out of db as
	// '"deny"' and '"allow"'.
	result, err := NewEffect(strings.Trim(string(b), "\""))
	if err != nil {
		return err
	}
	*e = result
	return nil
}
