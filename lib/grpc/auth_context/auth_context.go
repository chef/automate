package auth_context

import (
	"context"
	"errors"
	"strings"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"google.golang.org/grpc/metadata"
)

type (
	contextSubsKey     struct{}
	contextResKey      struct{}
	contextActKey      struct{}
	contextPolicyKey   struct{}
	contextProjectsKey struct{}
)

var (
	subjectsKey = contextSubsKey{}
	resourceKey = contextResKey{}
	actionKey   = contextActKey{}
	policyKey   = contextPolicyKey{}
	projectsKey = contextProjectsKey{}
)

var (
	ErrParseAuthContext = errors.New("could not parse auth context")
)

var (
	// Must match authz-service/../constants.go#AllProjectsExternalID
	AllProjectsKey = "*"
)

// Properties is a container for metadata available from the context.
type Properties struct {
	Subjects []string
	Projects []string
	Resource string
	Action   string
}

// NewContext returns a new `context.Context` that holds a reference
// to the provided properties
func NewContext(ctx context.Context, subs []string, projects []string,
	res, act string) context.Context {

	ctx = context.WithValue(ctx, subjectsKey, subs)
	ctx = context.WithValue(ctx, projectsKey, projects)
	ctx = context.WithValue(ctx, resourceKey, res)
	ctx = context.WithValue(ctx, actionKey, act)
	return ctx
}

// NewOutgoingContext translates previously injected auth_context info into
// GRPC metadata, to be consumed by the downstream service.
func NewOutgoingContext(ctx context.Context) context.Context {
	auth := FromContext(ctx)
	md := metadata.MD{
		"subjects": auth.Subjects,
		"projects": auth.Projects,
		"resource": {auth.Resource},
		"action":   {auth.Action},
	}
	return metadata.NewOutgoingContext(ctx, md)
}

// NewOutgoingProjectsContext translates previously injected auth_context info into
// GRPC metadata, to be consumed by the downstream service, but only for projects.
func NewOutgoingProjectsContext(ctx context.Context) context.Context {
	auth := FromContext(ctx)
	md := metadata.MD{
		"projects": auth.Projects,
	}
	return metadata.NewOutgoingContext(ctx, md)
}

// ProjectsFromMetadata extracts the requested projects from (incoming) metadata
// as provided by grpc-gateway (so, it's expecting, and removing,
// grpc-gateway's key prefixes).
func ProjectsFromMetadata(md metadata.MD) []string {
	projectHeaderEntries := md.Get(runtime.MetadataPrefix + "projects")
	if projectHeaderEntries == nil {
		projectHeaderEntries = []string{}
	}
	ps := []string{}
	keys := make(map[string]bool)
	for _, entry := range projectHeaderEntries {
		for _, project := range strings.Split(entry, ",") {
			newProject := strings.TrimSpace(project)
			if !keys[newProject] {
				keys[newProject] = true
				ps = append(ps, newProject)
			}
		}
	}
	return ps
}

// ContextWithoutProjects removes any projects from the incoming GRPC metadata
// attached to the context. Following attempts to read projects from incoming
// metadata will yield nothing -- so this can be used in handlers that should
// not do projects filtering, but would do that if the key was present.
func ContextWithoutProjects(ctx context.Context) context.Context {
	md, ok := metadata.FromIncomingContext(ctx)
	if ok {
		delete(md, "projects")
	}
	return ctx
}

// FromContext returns the auth data previously associated with `ctx`,
// or `nil` or "" if a piece of the information could not be found.
func FromContext(ctx context.Context) *Properties {
	var subs, projs []string
	var res, act string
	if s, ok := ctx.Value(subjectsKey).([]string); ok {
		subs = s
	}
	if p, ok := ctx.Value(projectsKey).([]string); ok {
		projs = p
	}
	if r, ok := ctx.Value(resourceKey).(string); ok {
		res = r
	}
	if a, ok := ctx.Value(actionKey).(string); ok {
		act = a
	}
	return &Properties{
		Subjects: subs,
		Resource: res,
		Action:   act,
		Projects: projs,
	}
}

// Subjects returns the auth subjects associated with `ctx`,
// or `nil` if it could not be found.
func Subjects(ctx context.Context) (subs []string) {
	if s, ok := ctx.Value(subjectsKey).([]string); ok {
		subs = s
	}
	return
}

// ProjectsFromIncomingContext parses the projects from the incoming
// request context. It throws an error if there is an issue parsing the
// context.
func ProjectsFromIncomingContext(ctx context.Context) ([]string, error) {
	authProps := FromContext(FromIncomingMetadata(ctx))
	if authProps == nil {
		return nil, ErrParseAuthContext
	}
	return authProps.Projects, nil
}

// AllProjectsRequested takes in the project filter list
// and returns true if the list is a single entry of *.
func AllProjectsRequested(projectsFilter []string) bool {
	return len(projectsFilter) == 1 && projectsFilter[0] == AllProjectsKey
}

// FromIncomingMetadata translates auth info provided by GRPC metadata into
// auth_context's representation, to be retrieved via auth_context.FromContext.
func FromIncomingMetadata(ctx context.Context) context.Context {
	var res, act string
	md, ok := metadata.FromIncomingContext(ctx)
	if !ok {
		return ctx
	}
	subs := md["subjects"]
	projs := md["projects"]
	if r, ok := md["resource"]; ok && len(r) > 0 {
		res = r[0]
	}
	if a, ok := md["action"]; ok && len(a) > 0 {
		act = a[0]
	}
	return NewContext(ctx, subs, projs, res, act)
}
