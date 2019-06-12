package engine

import (
	"context"

	v2 "github.com/chef/automate/components/authz-service/storage/v2"
)

// Engine abstracts different decision engines.
//
// Currently, a full-featured engine needs to support both v1 and v2. These are
// split to allow for a better decoupling in wiring up things -- i.e., the v2
// authz and policy implementations only require V2Authorizer and V2Writer,
// respectively. Our OPA implementation of engine.Engine supports all of those
// interfaces, and can thus be plugged into either of those constructors.
type Engine interface {
	V1Engine

	// V2Authorizer and V2Writer are never used together (the authz section of the
	// service needs V2Authorizer, the policy section cares about V2Writer), so we
	// collect them here instead of introducing a V2Engine interface.
	V2Authorizer
	V2pXWriter
	ProjectRulesRetriever
}

type V2pXWriter interface {
	V2Writer
	V2p1Writer
}

// V1Engine is the interface representing the IAM v1 backing engine methods:
// updating the engine store (engine.Writer) and querying it for certain
// authorization requests (engine.Authorizer).
type V1Engine interface {
	Authorizer
	Writer
}

// Authorizer is the interface for asking whether a concrete SAR tuple is
// authorized or not; and for filtering a given set of resource/action pairs,
// together with subjects, according to its authorization status.
type Authorizer interface {
	IsAuthorized(context.Context, Subjects, Action, Resource) (bool, error)

	// FilterAuthorizedPairs returns the sublist of authorized Pairs from the
	// passed-in list. It takes the subjects into account, and follows the same
	// logic a sequence of IsAuthorized() calls would use.
	FilterAuthorizedPairs(context.Context, Subjects, []Pair) ([]Pair, error)
}

type V2Authorizer interface {

	// V2IsAuthorized returns an allow/deny decision
	// allowed by the subjects/action/resource tuple.
	V2IsAuthorized(context.Context, Subjects, Action, Resource) (bool, error)

	// V2ProjectsAuthorized returns a subset of the requested projects (Projects)
	// allowed by the subjects/action/resource tuple.
	V2ProjectsAuthorized(context.Context, Subjects, Action, Resource, Projects) ([]string, error)

	// FilterAuthorizedProjects returns a sublist of the passed-in pairs
	// allowed by the subjects.
	V2FilterAuthorizedPairs(context.Context, Subjects, []Pair, bool) ([]Pair, error)

	// V2FilterAuthorizedProjects returns a list of allowed projects
	// for the given subjects
	V2FilterAuthorizedProjects(context.Context, Subjects) ([]string, error)
}

// Writer is the interface for writing policies to a decision engine
type Writer interface {
	SetPolicies(context.Context, map[string]interface{}) error
}

type V2Writer interface {
	V2SetPolicies(context.Context, map[string]interface{}, map[string]interface{}) error
}

type V2p1Writer interface {
	V2p1SetPolicies(context.Context, map[string]interface{}, map[string]interface{}) error
	SetRules(context.Context, map[string][]v2.Rule) error
}

type ProjectRulesRetriever interface {
	ListProjectMappings(context.Context) (map[string][]v2.Rule, error)
}

// Subjects contains the requestor and all the teams they're a member of.
// The strings are namespaced, so for the requestor, this would be
//     "user:type:EXTERNALID"
// and for their teams
//     "team:type:TEAMID"
type Subjects []string

// Subject is a convenience function, allowing us to pass
//     engine.Subject("team:local:admin")
// instead of
//     engine.Subjects([]string{"team:local:admin"})
// as argument to `IsAuthorized`
func Subject(subs ...string) Subjects {
	return subs
}

// A list of requested projects
type Projects []string

// ProjectList is a convenience function, allowing us to pass
//     engine.ProjectList("project-9")
// instead of
//     engine.Projects([]string{"project-9"})
// as argument to `FilterAuthorizedProjects`
func ProjectList(projects ...string) []string {
	return projects
}

// Action is how the subject wants to interact with the resource
type Action string

// Resource is what the subject is attempting to interact with
type Resource string

// Project is the input query's REQUESTED project
// i.e. the project selected in the project filter.
// TODO: make this an array!!
type Project string

// Pair is a convenience type for filtering a set of pairs according to their authorization
type Pair struct {
	Resource Resource `json:"resource"`
	Action   Action   `json:"action"`
}
