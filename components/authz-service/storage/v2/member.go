package v2

import (
	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Member represents a member that can be added / removed from a policy.
type Member struct {
	ID   uuid.UUID `json:"id"`
	Name string    `json:"name"`
}

// NewMember is a factory for creating a Member storage object.
func NewMember(name string) (Member, error) {
	if name == "" {
		return Member{},
			errors.New("member cannot have an empty name")
	}

	id, err := uuid.New()
	if err != nil {
		return Member{}, storage_errors.ErrGenerateUUID
	}

	return Member{
		ID:   id,
		Name: name,
	}, nil
}

// MemberSliceToStringSlice returns a slice of the
// names of members or an empty string slice if
// the member array is of length zero or nil.
func MemberSliceToStringSlice(m []Member) []string {
	memberSlice := make([]string, len(m))
	for i, member := range m {
		memberSlice[i] = member.Name
	}
	return memberSlice
}
