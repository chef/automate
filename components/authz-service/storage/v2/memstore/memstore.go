package memstore

import (
	"context"
	"sync/atomic"

	cache "github.com/patrickmn/go-cache"
	"github.com/pkg/errors"

	constants_v2 "github.com/chef/automate/components/authz-service/constants/v2"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	v2 "github.com/chef/automate/components/authz-service/storage/v2"
)

type State struct {
	policyChangeID int64 // DO NOT MOVE, must be 64-bit aligned for atomic increment
	changeManager  *policyChangeNotifierManager
	policies       *cache.Cache
	roles          *cache.Cache
	projects       *cache.Cache
	rules          *cache.Cache
	ms             storage.MigrationStatus
}

var ErrTypeAssertionFailed = errors.New("type assertion failed: could not convert interface{} to *storage.Policy")

func New() *State {
	s := &State{
		policies:       cache.New(cache.NoExpiration, -1 /* never run cleanup */),
		roles:          cache.New(cache.NoExpiration, -1),
		projects:       cache.New(cache.NoExpiration, -1),
		rules:          cache.New(cache.NoExpiration, -1),
		policyChangeID: 0,
		changeManager:  newPolicyChangeNotifierManager(),
	}
	return s
}

func (s *State) bumpPolicyVersion() {
	atomic.AddInt64(&s.policyChangeID, int64(1))
	s.changeManager.notifyChange()
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

	s.bumpPolicyVersion()
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

	s.bumpPolicyVersion()
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

	s.bumpPolicyVersion()
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
	if err != nil {
		return nil, err
	}

	s.bumpPolicyVersion()
	return pol, nil
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

	s.bumpPolicyVersion()
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

	s.bumpPolicyVersion()
	return tmpMembers, nil
}

func (s *State) DeletePolicy(ctx context.Context, policyID string) error {
	_, err := s.GetPolicy(ctx, policyID)

	if err != nil {
		return err
	}

	s.policies.Delete(policyID)
	s.bumpPolicyVersion()

	return nil
}

func (s *State) GetPolicyChangeID(_ context.Context) (string, error) {
	return string(atomic.LoadInt64(&s.policyChangeID)), nil
}

func (s *State) GetPolicyChangeNotifier(ctx context.Context) (v2.PolicyChangeNotifier, error) {
	notifier := s.changeManager.register()
	return notifier, nil
}

func (s *State) CreateRule(_ context.Context, rule *storage.Rule) (*storage.Rule, error) {
	rule.Status = "applied"
	if err := s.rules.Add(rule.ID, rule, cache.NoExpiration); err != nil {
		return nil, storage_errors.ErrConflict
	}
	s.bumpPolicyVersion()
	return rule, nil
}

func (s *State) UpdateRule(_ context.Context, rule *storage.Rule) (*storage.Rule, error) {
	item, exists := s.rules.Get(rule.ID)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}
	existingRule, ok := item.(*storage.Rule)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	if existingRule.ProjectID != rule.ProjectID {
		return nil, storage_errors.ErrChangeProjectForRule
	}

	if err := s.rules.Replace(rule.ID, rule, cache.NoExpiration); err != nil {
		return nil, storage_errors.ErrConflict
	}
	s.bumpPolicyVersion()
	return rule, nil
}

func (s *State) GetStagedOrAppliedRule(_ context.Context, id string) (*storage.Rule, error) {
	item, exists := s.rules.Get(id)
	if !exists {
		return nil, storage_errors.ErrNotFound
	}

	rule, ok := item.(*storage.Rule)
	if !ok {
		return nil, ErrTypeAssertionFailed
	}

	return rule, nil
}

func (s *State) ListStagedAndAppliedRules(_ context.Context) ([]*storage.Rule, error) {
	items := s.rules.Items()
	rules := []*storage.Rule{}

	for _, item := range items {
		if rule, ok := item.Object.(*storage.Rule); ok {
			rules = append(rules, rule)
		}
	}

	return rules, nil
}

func (s *State) ListRules(_ context.Context) ([]*storage.Rule, error) {
	items := s.rules.Items()
	rules := []*storage.Rule{}

	for _, item := range items {
		if rule, ok := item.Object.(*storage.Rule); ok && rule.Status == "applied" {
			rules = append(rules, rule)
		}
	}

	return rules, nil
}

func (s *State) ListRulesForProject(_ context.Context, projectID string) ([]*storage.Rule, error) {
	items := s.rules.Items()
	rules := []*storage.Rule{}

	for _, item := range items {
		if rule, ok := item.Object.(*storage.Rule); ok {
			if rule.ProjectID == projectID {
				rules = append(rules, rule)
			}
		}
	}

	return rules, nil
}

func (s *State) DeleteRule(ctx context.Context, id string) error {
	_, err := s.GetStagedOrAppliedRule(ctx, id)

	if err != nil {
		return err
	}

	s.rules.Delete(id)
	s.bumpPolicyVersion()
	return nil
}

func (s *State) ApplyStagedRules(ctx context.Context) error {
	// TODO
	return nil
}

func (s *State) CreateProject(_ context.Context, project *storage.Project) (*storage.Project, error) {
	if project.Type == storage.Custom {
		items := s.projects.Items()
		projects := make([]*storage.Project, 0, len(items))

		for _, item := range items {
			if p, ok := item.Object.(*storage.Project); ok {
				if p.Type == storage.Custom {
					projects = append(projects, p)
				}
			}
		}

		if len(projects) >= constants_v2.MaxProjects {
			return nil, storage_errors.ErrMaxProjectsExceeded
		}
	}

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

	s.bumpPolicyVersion()
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

	s.bumpPolicyVersion()
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
	if err != nil {
		return nil, err
	}
	s.bumpPolicyVersion()
	return role, nil
}

func (s *State) Reset(ctx context.Context) error {
	s.policies.Flush()
	s.roles.Flush()
	s.projects.Flush()

	s.bumpPolicyVersion()
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
	if s.ms == storage.InProgress {
		s.ms = storage.Failed
		return nil
	}
	return errors.New("cannot transition to failure")
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

// RulesCache is used in testing
func (s *State) RulesCache() *cache.Cache {
	return s.rules
}
func (s *State) Pristine(context.Context) error {
	s.ms = storage.Pristine
	return nil
}
