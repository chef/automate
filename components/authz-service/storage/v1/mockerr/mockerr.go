package mockerr

import (
	"context"
	"errors"

	storage "github.com/chef/automate/components/authz-service/storage/v1"
	uuid "github.com/chef/automate/lib/uuid4"
)

type mockerr struct {
}

// New returns a new mockerr used for testing that
// implements the Storage interface and always returns an error.
func New() storage.Storage {
	return &mockerr{}
}

func (m *mockerr) StorePolicy(context.Context, string, []string, string, string) (*storage.Policy, error) {
	return nil, errors.New("StorePolicy mock error")
}

func (m *mockerr) ListPolicies(context.Context) ([]*storage.Policy, error) {
	return nil, errors.New("ListPolicies mock error")
}

func (m *mockerr) ListPoliciesWithSubjects(context.Context) ([]*storage.Policy, error) {
	return nil, errors.New("ListPoliciesWithSubjects mock error")
}

func (m *mockerr) DeletePolicy(context.Context, string) (*storage.Policy, error) {
	return nil, errors.New("DeletePolicy mock error")
}

func (m *mockerr) PurgeSubjectFromPolicies(context.Context, string) ([]uuid.UUID, error) {
	return nil, errors.New("PurgeSubjectFromPolicies mock error")
}
