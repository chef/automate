package memstore

import (
	"context"
	"sync"
	"time"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	storage "github.com/chef/automate/components/authz-service/storage/v1"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
)

type memstore struct {
	logger   logger.Logger
	policies map[uuid.UUID]storage.Policy
	mutex    sync.Mutex
}

func New(l logger.Logger) (storage.Storage, error) {
	mem := &memstore{
		policies: make(map[uuid.UUID]storage.Policy),
		mutex:    sync.Mutex{},
		logger:   l,
	}

	if err := mem.bootstrapInitialPolicies(); err != nil {
		return nil, err
	}

	return mem, nil
}

func (m *memstore) Reset(_ context.Context) error {
	m.policies = make(map[uuid.UUID]storage.Policy)
	return m.bootstrapInitialPolicies()
}

func (m *memstore) StorePolicy(
	_ context.Context,
	action string, subjects []string, resource string, effect string) (*storage.Policy, error) {
	id := uuid.Must(uuid.NewV4())
	policy := storage.Policy{
		Action:    action,
		ID:        id,
		Resource:  resource,
		Subjects:  subjects,
		Effect:    effect,
		Version:   storage.Version,
		CreatedAt: time.Now(),
	}

	m.mutex.Lock()
	m.policies[id] = policy
	m.mutex.Unlock()

	return &policy, nil
}

func (m *memstore) ListPolicies(_ context.Context) ([]*storage.Policy, error) {
	policyArr := make([]*storage.Policy, len(m.policies))

	m.mutex.Lock()
	i := 0
	for _, policy := range m.policies {
		getNewPointer := policy
		policyArr[i] = &getNewPointer
		i++
	}
	m.mutex.Unlock()

	return policyArr, nil
}

func (m *memstore) ListPoliciesWithSubjects(_ context.Context) ([]*storage.Policy, error) {
	policyArr := make([]*storage.Policy, len(m.policies))

	m.mutex.Lock()
	i := 0
	for _, policy := range m.policies {
		if len(policy.Subjects) != 0 {
			getNewPointer := policy
			policyArr[i] = &getNewPointer
			i++
		}
	}
	m.mutex.Unlock()

	return policyArr, nil
}

func (m *memstore) DeletePolicy(_ context.Context, id string) (*storage.Policy, error) {
	u, err := uuid.FromString(id)
	if err != nil {
		return nil, err
	}

	if storage.IsNonDeletablePolicy(id) {
		return nil, storage_errors.ErrCannotDelete
	}

	m.mutex.Lock()
	var found bool
	var policy storage.Policy
	policy, found = m.policies[u]
	m.mutex.Unlock()

	if !found {
		return nil, storage_errors.ErrNotFound
	}

	m.mutex.Lock()
	delete(m.policies, u)
	m.mutex.Unlock()

	m.logger.Debugf("successfully deleted policy %v", policy)
	return &policy, nil
}

func (m *memstore) PurgeSubjectFromPolicies(_ context.Context, sub string) ([]uuid.UUID, error) {
	m.mutex.Lock()
	defer m.mutex.Unlock()

	found := []uuid.UUID{}
	for _, pol := range m.policies {
		if storage.IsNonDeletablePolicy(pol.ID.String()) {
			m.logger.Debugf("Not removing subject from policy %s, it's non-deletable", pol.ID.String())
			continue // skip
		}
		if new := remove(pol.Subjects, sub); len(new) < len(pol.Subjects) {
			m.logger.Debugf("Removed subject from policy %s", pol.ID.String())
			found = append(found, pol.ID)
			pol.Subjects = new
			pol.UpdatedAt = time.Now()
			m.policies[pol.ID] = pol
		}
	}

	return found, nil
}

func remove(xs []string, y string) []string {
	i := idx(xs, y)
	if i >= 0 {
		return append(xs[:i], xs[i+1:]...)
	}
	return xs
}

func idx(xs []string, y string) int {
	for i := range xs {
		if xs[i] == y {
			return i
		}
	}
	return -1
}

func (m *memstore) bootstrapInitialPolicies() error {
	defaultPolicies, err := storage.DefaultPolicies()
	if err != nil {
		return err
	}
	for _, pol := range defaultPolicies {
		m.mutex.Lock()
		m.policies[pol.ID] = *pol
		m.mutex.Unlock()
	}
	return nil
}
