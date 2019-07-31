package v2

import (
	"errors"
)

// Statement must have at least a role OR a non-empty actions list
type Statement struct {
	Actions   []string `json:"actions"`
	Resources []string `json:"resources"`
	Role      string   `json:"role"`
	Projects  []string `json:"projects"`
	Effect    Effect   `json:"effect"`
}

// NewStatement is a factory for creating a Statement storage object that also does
// validation around what a valid statement is in terms of our storage layer.
func NewStatement(effect Effect, role string, projects, resources, actions []string) (Statement, error) {
	if actions == nil {
		actions = []string{}
	}
	if resources == nil {
		resources = []string{}
	}

	if role == "" && len(actions) == 0 {
		return Statement{},
			errors.New("unsupported statement variant: you must pass a role, a non-empty array of actions, or both")
	}

	return Statement{
		Effect:    effect,
		Role:      role,
		Projects:  projects,
		Actions:   actions,
		Resources: resources,
	}, nil
}
