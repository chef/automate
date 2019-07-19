package v2

import (
	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Statement must have at least a role OR a non-empty actions list
type Statement struct {
	ID        uuid.UUID `json:"id"`
	Actions   []string  `json:"actions"`
	Resources []string  `json:"resources"`
	Role      string    `json:"role"`
	Projects  []string  `json:"projects"`
	Effect    Effect    `json:"effect"`
}

// NewStatement is a factory for creating a Statement storage object that also does
// validation around what a valid statement is in terms of our storage layer.
// It also generates a new ID for our statement.
func NewStatement(effect Effect, role string, projects, resources, actions []string) (Statement, error) {
	if actions == nil {
		actions = []string{}
	}

	if role == "" && len(actions) == 0 {
		return Statement{},
			errors.New("unsupported statement variant: you must pass a role, a non-empty array of actions, or both")
	}

	id, err := uuid.New()
	if err != nil {
		return Statement{}, storage_errors.ErrGenerateUUID
	}

	return Statement{
		Effect:    effect,
		Role:      role,
		Projects:  projects,
		Actions:   actions,
		Resources: resources,
		ID:        id,
	}, nil
}
