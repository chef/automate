package v2

import (
	"fmt"
)

// ProjectRulesStatus is an enum that represents the states
// a project's rules can be in.
type ProjectRulesStatus int

const (
	// The project has rules but all are applied
	Applied ProjectRulesStatus = iota
	// The project has rules and at least one is staged
	EditsPending
	// The project has no staged or applied rules
	NoRules
	// RulesStatusError occurs in some error situation
	RulesStatusError
)

func (c ProjectRulesStatus) String() string {
	if c < Applied || c > RulesStatusError {
		panic(fmt.Sprintf("unknown value from iota ProjectRulesStatus on String() conversion: %d", c))
	}

	return projectRulesStatusStringValues()[c]
}

func projectRulesStatusStringValues() [4]string {
	return [...]string{
		"applied",
		"edits-pending",
		"no-rules",
		"rules-status-error",
	}
}
