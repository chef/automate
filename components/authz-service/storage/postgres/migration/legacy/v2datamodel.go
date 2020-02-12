package legacy

import (
	"encoding/json"
	"fmt"
	"strings"

	constants_v2 "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v2"
	"github.com/pkg/errors"
)

// Member represents a member that can be added / removed from a policy.
type v2Member struct {
	Name string `json:"name"`
}

func newV2Member(name string) (v2Member, error) {
	if name == "" {
		return v2Member{},
			errors.New("member cannot have an empty name")
	}

	return v2Member{
		Name: name,
	}, nil
}

// Type is an enum to denote custom or chef-managed policy.
type v2Type int

const (
	// Custom represents a policy created by the enduser.
	Custom v2Type = iota
	// ChefManaged represents a policy created by Chef Software.
	ChefManaged
	// System represents a policy that is only loaded directly into OPA
	// to allow Automate to function correctly without revealing Automate's
	// internal policies to the customer
	// This type is only used in the OPA cache (not in API or database)
	System
)

// NewType converts a string to a Type or returns an error.
func NewType(in string) (v2Type, error) {
	switch in {
	case customTypeString:
		return Custom, nil
	case managedTypeString:
		return ChefManaged, nil
	case systemTypeString:
		return System, nil
	default:
		return Custom, fmt.Errorf("policy type must be one of %q, you passed %q", strValues, in)
	}
}

// UnmarshalJSON implements json unmarshalling for a Type reference
// so we can pull them out of the database directly as the correct type.
func (t *v2Type) UnmarshalJSON(b []byte) error {
	// After byte conversion, things coming out of db as
	// customTypeString or managedTypeString.
	result, err := NewType(strings.Trim(string(b), "\""))
	if err != nil {
		return err
	}
	*t = result
	return nil
}

const (
	customTypeString  = "custom"
	managedTypeString = "chef-managed"
	systemTypeString  = "system"
)

var strValues = [...]string{
	customTypeString,
	managedTypeString,
	systemTypeString,
}

func (t v2Type) String() string {
	if t < Custom || t > System {
		panic(fmt.Sprintf("unknown value from iota Type on String() conversion: %d", t))
	}

	return strValues[t]
}

type v2Role struct {
	ID       string   `json:"id"`
	Name     string   `json:"name"`
	Actions  []string `json:"actions"`
	Type     v2Type   `json:"type"`
	Projects []string `json:"projects"`
}

// Scan implements pq Scan interface for a Role reference
// so we can pull them out of the database directly as the correct type.
func (p *v2Role) Scan(src interface{}) error {
	if src == nil {
		return errors.New("not found")
	}
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}
	return json.Unmarshal(source, p)
}

// Policy represents a policy definition to be persisted to storage.
type v2Policy struct {
	ID         string        `json:"id"`
	Name       string        `json:"name"`
	Members    []v2Member    `json:"members"`
	Statements []v2Statement `json:"statements"`
	Type       v2Type        `json:"type"`
	Projects   []string      `json:"projects"`
}

// Scan implements pq Scan interface for an Policy reference
// so we can pull them out of the database directly as the correct type.
func (p *v2Policy) Scan(src interface{}) error {
	if src == nil {
		return errors.New("not found")
	}
	source, ok := src.([]byte)
	if !ok {
		return errors.New("type assertion .([]byte) failed")
	}
	return json.Unmarshal(source, p)
}

func newV2Policy(
	id string,
	name string,
	typeVal v2Type,
	members []v2Member,
	statements []v2Statement,
	projects []string,
) (v2Policy, error) {

	if id == "" {
		return v2Policy{}, errors.New("missing id")
	}
	if name == "" {
		return v2Policy{}, errors.New("missing name")
	}

	return v2Policy{
		ID:         id,
		Name:       name,
		Type:       typeVal,
		Members:    members,
		Statements: statements,
		Projects:   projects,
	}, nil
}

type v2Statement struct {
	Actions   []string `json:"actions"`
	Resources []string `json:"resources"`
	Role      string   `json:"role"`
	Projects  []string `json:"projects"`
	Effect    v2Effect `json:"effect"`
}

// Effect is an enum of allow or deny for use in Statements.
type v2Effect int

const (
	// Allow represents the allow case for a Statement Effect.
	Allow v2Effect = iota
	// Deny represents the deny case for a Statement Effect.
	Deny
)

func (e v2Effect) String() string {
	strValues := [...]string{
		"allow",
		"deny",
	}

	if e < Allow || e > Deny {
		panic(fmt.Sprintf("unknown value from iota Effect on String() conversion: %d", e))
	}

	return strValues[e]
}

// NewEffect converts a string to an Effect or returns an error.
func NewEffect(in string) (v2Effect, error) {
	switch in {
	case "allow":
		return Allow, nil
	case "deny":
		return Deny, nil
	default:
		return Allow, fmt.Errorf("effect must be one of 'allow' or 'deny', you passed %s", in)
	}
}

// UnmarshalJSON implements json unmarshalling for an Effect reference
// so we can pull them out of the database directly as the correct type.
func (e *v2Effect) UnmarshalJSON(b []byte) error {
	// After byte conversion, things coming out of db as
	// '"deny"' and '"allow"'.
	result, err := NewEffect(strings.Trim(string(b), "\""))
	if err != nil {
		return err
	}
	*e = result
	return nil
}

func newV2Statement(effect v2Effect, role string, projects, resources, actions []string) v2Statement {
	return v2Statement{
		Effect:    effect,
		Role:      role,
		Projects:  projects,
		Actions:   actions,
		Resources: resources,
	}
}

// DefaultRoles defines the default Chef-managed roles provided on storage reset
func defaultRoles() []v2Role {
	owner := v2Role{
		ID:      constants_v2.OwnerRoleID,
		Name:    "Owner",
		Actions: []string{"*"},
		Type:    ChefManaged,
	}

	editor := v2Role{
		ID:   constants_v2.EditorRoleID,
		Name: "Editor",
		Actions: []string{
			"infra:*",
			"compliance:*",
			"system:*",
			"event:*",
			"ingest:*",
			"secrets:*",
			"telemetry:*",
		},
		Type: ChefManaged,
	}

	viewer := v2Role{
		ID:   constants_v2.ViewerRoleID,
		Name: "Viewer",
		Actions: []string{
			"secrets:*:get",
			"secrets:*:list",
			"infra:*:get",
			"infra:*:list",
			"compliance:*:get",
			"compliance:*:list",
			"system:*:get",
			"system:*:list",
			"event:*:get",
			"event:*:list",
			"ingest:*:get",
			"ingest:*:list",
		},
		Type: ChefManaged,
	}

	ingest := v2Role{
		ID:   constants_v2.IngestRoleID,
		Name: "Ingest",
		Actions: []string{
			"infra:ingest:*",
			"compliance:profiles:get",
			"compliance:profiles:list",
		},
		Type: ChefManaged,
	}

	return []v2Role{owner, editor, viewer, ingest}
}

// v2DefaultPolicies shipped with IAM v2, and also the set of policies to which we
// factory-reset our storage.
func v2DefaultPolicies() []v2Policy {
	// admin policy statements
	s1 := newV2Statement(Allow, "", []string{}, []string{"*"}, []string{"*"})
	s2 := newV2Statement(Deny, "", []string{}, []string{"iam:policies:" + constants_v2.AdminPolicyID},
		[]string{"iam:policies:delete", "iam:policies:update"})

	// editor policy statements
	s3 := newV2Statement(Allow, constants_v2.EditorRoleID, []string{}, []string{"*"}, []string{})

	// viewer policy statements
	s4 := newV2Statement(Allow, constants_v2.ViewerRoleID, []string{}, []string{"*"}, []string{})

	// ingest policy statements
	s5 := newV2Statement(Allow, constants_v2.IngestRoleID, []string{}, []string{"*"}, []string{})

	admin := v2Member{Name: constants_v2.LocalAdminsTeamSubject}
	editors := v2Member{Name: constants_v2.LocalEditorsTeamSubject}
	viewers := v2Member{Name: constants_v2.LocalViewersTeamSubject}

	adminPol := v2Policy{
		ID:         constants_v2.AdminPolicyID,
		Name:       "Administrator",
		Members:    []v2Member{admin},
		Statements: []v2Statement{s1, s2},
		Type:       ChefManaged,
	}

	editorPol := v2Policy{
		ID:         constants_v2.EditorPolicyID,
		Name:       "Editors",
		Members:    []v2Member{editors},
		Statements: []v2Statement{s3},
		Type:       ChefManaged,
	}

	viewerPol := v2Policy{
		ID:         constants_v2.ViewerPolicyID,
		Name:       "Viewers",
		Members:    []v2Member{viewers},
		Statements: []v2Statement{s4},
		Type:       ChefManaged,
	}

	ingestPol := v2Policy{
		ID:         constants_v2.IngestPolicyID,
		Name:       "Ingest",
		Members:    []v2Member{},
		Statements: []v2Statement{s5},
		Type:       ChefManaged,
	}

	return []v2Policy{adminPol, editorPol, viewerPol, ingestPol}
}
