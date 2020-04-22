package engine

import (
	"context"
)

// Engine abstracts different decision engines.
type Engine interface {
	// Authorizer and Writer are never used together (the authz section of the
	// service needs Authorizer, the policy section cares about Writer), so we
	// collect them here instead of introducing a Engine interface.
	Authorizer
	Writer
}

type Authorizer interface {
	// ProjectsAuthorized returns a subset of the requested projects (Projects)
	// allowed by the subjects/action/resource tuple.
	ProjectsAuthorized(context.Context, Subjects, Action, Resource, Projects) ([]string, error)

	// FilterAuthorizedProjects returns a sublist of the passed-in pairs
	// allowed by the subjects.
	FilterAuthorizedPairs(context.Context, Subjects, []Pair) ([]Pair, error)

	// FilterAuthorizedProjects returns a list of allowed projects
	// for the given subjects
	FilterAuthorizedProjects(context.Context, Subjects) ([]string, error)
}

type Writer interface {
	SetPolicies(context.Context, map[string]interface{}, map[string]interface{}) error
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
