package v2

import (
	"fmt"
)

// ProjectRulesStatus is an enum that represents the states
// a project's rules can be in.
type ProjectRulesStatus int

const (
	// RulesStatusError occurs in some error situation
	RulesStatusError ProjectRulesStatus = iota
	// The project has rules but all are applied
	Applied
	// The project has rules and at least one is staged
	EditsPending
	// The project has no staged or applied rules
	NoRules
)

func (c ProjectRulesStatus) String() string {
	if c < RulesStatusError || c > NoRules {
		panic(fmt.Sprintf("unknown value from iota ProjectRulesStatus on String() conversion: %d", c))
	}

	return projectRulesStatusStringValues()[c]
}

func projectRulesStatusStringValues() [4]string {
	return [...]string{
		"rules-status-error",
		"applied",
		"edits-pending",
		"no-rules",
	}
}
