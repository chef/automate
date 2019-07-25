package v2

import (
	"github.com/pkg/errors"
)

// Member represents a member that can be added / removed from a policy.
type Member struct {
	Name string `json:"name"`
}

// NewMember is a factory for creating a Member storage object.
func NewMember(name string) (Member, error) {
	if name == "" {
		return Member{},
			errors.New("member cannot have an empty name")
	}

	return Member{
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
