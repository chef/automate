package v2

import (
	"context"

	constants "github.com/chef/automate/components/authz-service/constants"
	constants_v2 "github.com/chef/automate/components/authz-service/constants/v2"
)

// Storage is the interface that both our postgres and memstore storage
// solutions implement for consistency. The memstore implementation is
// only used for ease of testing.
type Storage interface {
	policyStorage
	roleStorage
	projectStorage
	ruleStorage

	// Reset allows "factory-resetting" IAM v2 policies
	Reset(context.Context) error

	// Close closes the connection to the backend
	Close() error
}

type PolicyChangeNotification struct{}

type PolicyChangeNotifier interface {
	C() <-chan PolicyChangeNotification
	Close() error
}

type policyStorage interface {
	ReplacePolicyMembers(context.Context, string, []Member) ([]Member, error)
	RemovePolicyMembers(context.Context, string, []Member) ([]Member, error)
	DeletePolicy(context.Context, string) error
	RemoveProjectFromGraveyard(context.Context, string) error
	CreatePolicy(context.Context, *Policy, bool) (*Policy, error)
	ListPolicies(context.Context) ([]*Policy, error)
	GetPolicy(context.Context, string) (*Policy, error)
	UpdatePolicy(context.Context, *Policy) (*Policy, error)
	ListPolicyMembers(context.Context, string) ([]Member, error)
	AddPolicyMembers(context.Context, string, []Member) ([]Member, error)

	// Removes passed subject from all 'members' fields, returning affected
	// policies
	PurgeSubjectFromPolicies(ctx context.Context, subject string) ([]string, error)

	GetPolicyChangeID(context.Context) (string, error)
	GetPolicyChangeNotifier(context.Context) (PolicyChangeNotifier, error)
}

type roleStorage interface {
	CreateRole(context.Context, *Role, bool) (*Role, error)
	DeleteRole(context.Context, string) error
	UpdateRole(context.Context, *Role) (*Role, error)
	ListRoles(context.Context) ([]*Role, error)
	GetRole(context.Context, string) (*Role, error)
}

type projectStorage interface {
	UpdateProject(context.Context, *Project) (*Project, error)
	CreateProject(context.Context, *Project, bool) (*Project, error)
	GetProject(context.Context, string) (*Project, error)
	DeleteProject(context.Context, string) error
	ListProjects(context.Context) ([]*Project, error)
	EnsureNoProjectsMissing(context.Context, []string) error
}

type ruleStorage interface {
	CreateRule(context.Context, *Rule) (*Rule, error)
	GetStagedOrAppliedRule(ctx context.Context, projectID string, ruleID string) (*Rule, error)
	UpdateRule(context.Context, *Rule) (*Rule, error)
	ListRules(context.Context) ([]*Rule, error)
	ListStagedAndAppliedRules(context.Context) ([]*Rule, error)
	DeleteRule(ctx context.Context, projectID string, ruleID string) error
	ListRulesForProject(context.Context, string) ([]*Rule, ProjectRulesStatus, error)
	ApplyStagedRules(context.Context) error
	FetchAppliedRulesByProjectIDs(context.Context) (map[string][]*Rule, error)
}

// DefaultPolicies shipped with IAM v2, and also the set of policies to which we
// factory-reset our storage.
func DefaultPolicies() ([]Policy, error) {

	s1, err := NewStatement(Allow, "", []string{}, []string{"*"}, []string{"*"})
	if err != nil {
		return nil, err
	}
	s2, err := NewStatement(Deny, "", []string{}, []string{"iam:policies:" + constants_v2.AdminPolicyID},
		[]string{"iam:policies:delete", "iam:policies:update"})
	if err != nil {
		return nil, err
	}

	// editor policy statements
	s3, err := NewStatement(Allow, constants_v2.EditorRoleID, []string{}, []string{"*"}, []string{})
	if err != nil {
		return nil, err
	}

	// viewer policy statements
	s4, err := NewStatement(Allow, constants_v2.ViewerRoleID, []string{}, []string{"*"}, []string{})
	if err != nil {
		return nil, err
	}

	// ingest policy statements
	s5, err := NewStatement(Allow, constants_v2.IngestRoleID, []string{}, []string{"*"}, []string{})
	if err != nil {
		return nil, err
	}

	typeManaged, err := NewType("chef-managed")
	if err != nil {
		return nil, err
	}

	member, err := NewMember(constants.LocalAdminsTeamSubject)
	if err != nil {
		return nil, err
	}

	editors, err := NewMember(constants.LocalEditorsTeamSubject)
	if err != nil {
		return nil, err
	}

	viewers, err := NewMember(constants.LocalViewersTeamSubject)
	if err != nil {
		return nil, err
	}

	adminPol := Policy{
		ID:         constants_v2.AdminPolicyID,
		Name:       "Administrator",
		Members:    []Member{member},
		Statements: []Statement{s1, s2},
		Type:       typeManaged,
	}

	editorPol := Policy{
		ID:         constants_v2.EditorPolicyID,
		Name:       "Editors",
		Members:    []Member{editors},
		Statements: []Statement{s3},
		Type:       typeManaged,
	}

	viewerPol := Policy{
		ID:         constants_v2.ViewerPolicyID,
		Name:       "Viewers",
		Members:    []Member{viewers},
		Statements: []Statement{s4},
		Type:       typeManaged,
	}

	ingestPol := Policy{
		ID:         constants_v2.IngestPolicyID,
		Name:       "Ingest",
		Members:    []Member{},
		Statements: []Statement{s5},
		Type:       typeManaged,
	}

	return []Policy{adminPol, editorPol, viewerPol, ingestPol}, nil
}

// DefaultProjects defines the default Chef-managed projects provided on storage reset
// At present, this list contains internally required projects only, hidden from the user.
func DefaultProjects() []Project {
	allProjects := Project{
		ID:   constants_v2.AllProjectsID,
		Name: "All Projects",
		Type: ChefManaged,
	}

	unassignedProject := Project{
		ID:   constants_v2.UnassignedProjectID,
		Name: constants_v2.UnassignedProjectID,
		Type: ChefManaged,
	}

	return []Project{allProjects, unassignedProject}
}

func DefaultProjectIDs() []string {
	projects := DefaultProjects()
	ids := make([]string, len(projects))
	for i := range projects {
		ids[i] = projects[i].ID
	}
	return ids
}
