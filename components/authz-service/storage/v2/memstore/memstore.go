package memstore

import (
	"context"

	cache "github.com/patrickmn/go-cache"
	"github.com/pkg/errors"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
)

type State struct {
	policies *cache.Cache
	roles    *cache.Cache
	projects *cache.Cache
	ms       storage.MigrationStatus
}

var ErrTypeAssertionFailed = errors.New("type assertion failed: could not convert interface{} to *storage.Policy")

func New() *State {
	return &State{
		policies: cache.New(cache.NoExpiration, -1 /* never run cleanup */),
		roles:    cache.New(cache.NoExpiration, -1),
		projects: cache.New(cache.NoExpiration, -1),
	}
}

func (s *State) CreatePolicy(_ context.Context, inputPol *storage.Policy) (*storage.Policy, error) {
	for _, item := range s.policies.Items() {
		if pol, ok := item.Object.(*storage.Policy); ok &&
			(pol.ID == inputPol.ID) {
			return nil, storage_errors.ErrConflict
		}
	}
	copyPol := *inputPol
	if err := s.policies.Add(inputPol.ID, &copyPol, cache.NoExpiration); err != nil {
		return nil, storage_errors.ErrConflict
	}

	return &copyPol, nil
}

func (s *State) PurgeSubjectFromPolicies(_ context.Context, sub string) ([]string, error) {
	var affected []string
	for _, item := range s.policies.Items() {
		if pol, ok := item.Object.(*storage.Policy); ok {
			newMembers := []storage.Member{}
			for _, member := range pol.Members {
				if member.Name != sub {
					newMembers = append(newMembers, member)
				} else {
					affected = append(affected, pol.ID)
				}
			}
			pol.Members = newMembers
		}
	}
	return affected, nil
}

func (s *State) ListPolicies(context.Context) ([]*storage.Policy, error) {
	items := s.policies.Items()
	pols := []*storage.Policy{}

	for _, item := range items {
		if pol, ok := item.Object.(*storage.Policy); ok {
			pols = append(pols, pol)
		}
	}

	return pols, nil
}

func (s *State) GetPolicy(_ context.Context, policyID string) (pol *storage.Policy, err error) {
	item, exists := s.policies.Get(policyID)

	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	pol, ok := item.(*storage.Policy)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	return pol, nil
}

func (s *State) ListPolicyMembers(_ context.Context, policyID string) ([]storage.Member, error) {
	item, exists := s.policies.Get(policyID)

	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	pol, ok := item.(*storage.Policy)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	return pol.Members, nil
}

func (s *State) AddPolicyMembers(
	_ context.Context, policyID string, members []storage.Member) ([]storage.Member, error) {

	item, exists := s.policies.Get(policyID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}
	pol, ok := item.(*storage.Policy)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	existing := make(map[string]int)
	for _, member := range pol.Members {
		existing[member.Name] = 1
	}

	for _, memberToAdd := range members {
		if _, ok := existing[memberToAdd.Name]; !ok {
			pol.Members = append(pol.Members, memberToAdd)
		}
	}

	return pol.Members, nil
}

func (s *State) UpdatePolicy(_ context.Context, p *storage.Policy) (*storage.Policy, error) {
	item, exists := s.policies.Get(p.ID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	pol, ok := item.(*storage.Policy)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	pol.ID = p.ID
	pol.Name = p.Name
	pol.Type = p.Type
	pol.Members = p.Members
	pol.Statements = p.Statements
	pol.Projects = p.Projects

	err := s.policies.Replace(p.ID, pol, cache.NoExpiration) // persist change
	return pol, err
}

func (s *State) ReplacePolicyMembers(
	ctx context.Context, policyID string, members []storage.Member) ([]storage.Member, error) {

	item, exists := s.policies.Get(policyID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}
	pol, ok := item.(*storage.Policy)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}
	pol.Members = members

	return members, nil
}

func (s *State) RemovePolicyMembers(ctx context.Context,
	policyID string, membersToRemove []storage.Member) ([]storage.Member, error) {

	item, exists := s.policies.Get(policyID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}
	pol, ok := item.(*storage.Policy)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	tmpMembers := []storage.Member{}
	removing := make(map[string]int)
	for _, m := range membersToRemove {
		removing[m.Name] = 1
	}
	for _, member := range pol.Members {
		if _, ok := removing[member.Name]; !ok {
			tmpMembers = append(tmpMembers, member)
		}
	}
	pol.Members = tmpMembers

	return tmpMembers, nil
}

func (s *State) DeletePolicy(ctx context.Context, policyID string) error {
	_, err := s.GetPolicy(ctx, policyID)

	if err != nil {
		return err
	}

	s.policies.Delete(policyID)
	return nil
}

func (s *State) CreateProject(_ context.Context, project *storage.Project) (*storage.Project, error) {
	if err := s.projects.Add(project.ID, project, cache.NoExpiration); err != nil {
		return nil, storage_errors.ErrConflict
	}

	return project, nil
}

func (s *State) UpdateProject(_ context.Context, project *storage.Project) (*storage.Project, error) {
	item, exists := s.projects.Get(project.ID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	resp, ok := item.(*storage.Project)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	resp.ID = project.ID
	resp.Name = project.Name
	resp.Type = project.Type

	err := s.projects.Replace(project.ID, resp, cache.NoExpiration) // persist change
	return resp, err
}

func (s *State) GetProject(_ context.Context, id string) (*storage.Project, error) {
	item, exists := s.projects.Get(id)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	project, ok := item.(*storage.Project)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	return project, nil
}

func (s *State) DeleteProject(ctx context.Context, id string) error {
	_, err := s.GetProject(ctx, id)

	if err != nil {
		return err
	}

	s.projects.Delete(id)
	return nil
}

func (s *State) ListProjects(context.Context) ([]*storage.Project, error) {
	items := s.projects.Items()
	projects := make([]*storage.Project, 0, len(items))

	for _, item := range items {
		if p, ok := item.Object.(*storage.Project); ok {
			// Note(sr): !ok should NEVER happen here, really
			projects = append(projects, p)
		}
	}

	return projects, nil
}

func (s *State) CreateRole(_ context.Context, role *storage.Role) (*storage.Role, error) {
	if err := s.roles.Add(role.ID, role, cache.NoExpiration); err != nil {
		return nil, storage_errors.ErrConflict
	}

	return role, nil
}

func (s *State) ListRoles(context.Context) ([]*storage.Role, error) {
	items := s.roles.Items()
	roles := []*storage.Role{}

	for _, item := range items {
		if role, ok := item.Object.(*storage.Role); ok {
			roles = append(roles, role)
		}
	}

	return roles, nil
}

func (s *State) GetRole(_ context.Context, roleID string) (role *storage.Role, err error) {
	item, exists := s.roles.Get(roleID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	role, ok := item.(*storage.Role)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	return role, nil
}

func (s *State) DeleteRole(ctx context.Context, roleID string) error {
	_, err := s.GetRole(ctx, roleID)

	if err != nil {
		return err
	}

	s.roles.Delete(roleID)
	return nil
}

func (s *State) UpdateRole(_ context.Context, r *storage.Role) (*storage.Role, error) {
	item, exists := s.roles.Get(r.ID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	role, ok := item.(*storage.Role)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	role.ID = r.ID
	role.Name = r.Name
	role.Actions = r.Actions
	role.Projects = r.Projects

	err := s.roles.Replace(r.ID, role, cache.NoExpiration) // persist change
	return role, err
}

func (s *State) Reset(ctx context.Context) error {
	s.policies.Flush()
	s.roles.Flush()
	s.projects.Flush()

	return nil
}

func (s *State) ApplyV2DataMigrations(context.Context) error {
	return nil
}

func (s *State) Close() error {
	return nil
}

func (s *State) Success(context.Context) error {
	s.ms = storage.Successful
	return nil
}

func (s *State) SuccessBeta1(context.Context) error {
	s.ms = storage.SuccessfulBeta1
	return nil
}

func (s *State) InProgress(context.Context) error {
	s.ms = storage.InProgress
	return nil
}

func (s *State) Failure(context.Context) error {
	s.ms = storage.Failed
	return nil
}

func (s *State) MigrationStatus(context.Context) (storage.MigrationStatus, error) {
	return s.ms, nil
}

// PoliciesCache is used in testing
func (s *State) PoliciesCache() *cache.Cache {
	return s.policies
}

// RolesCache is used in testing
func (s *State) RolesCache() *cache.Cache {
	return s.roles
}

// ProjectsCache is used in testing
func (s *State) ProjectsCache() *cache.Cache {
	return s.projects
}

func (s *State) Pristine(context.Context) error {
	s.ms = storage.Pristine
	return nil
}
