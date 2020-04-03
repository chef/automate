package auth_context_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/metadata"

	"github.com/chef/automate/lib/grpc/auth_context"
)

func TestRoundTrippingThroughContext(t *testing.T) {
	tests := map[string][]string{
		"no subjects":  {},
		"one subject":  {"user:local:alice"},
		"two subjects": {"user:local:alice", "team:local:admins"},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			auth := auth_context.FromContext(auth_context.NewContext(ctx, tc,
				[]string{"projects"}, "res", "act"))
			assert.ElementsMatch(t, tc, auth.Subjects)
			assert.Equal(t, []string{"projects"}, auth.Projects)
			assert.Equal(t, "res", auth.Resource)
			assert.Equal(t, "act", auth.Action)
		})
	}
}

func TestOutgoingMetadata(t *testing.T) {
	tests := map[string]struct {
		input    []string
		expected metadata.MD
	}{
		"no subjects": {
			[]string{},
			metadata.MD{
				"resource": []string{"res"},
				"action":   []string{"act"},
				"subjects": nil,
				"projects": []string{"projects"},
			},
		},
		"one subject": {
			[]string{"user:local:alice"},
			metadata.MD{
				"resource": []string{"res"},
				"action":   []string{"act"},
				"subjects": []string{"user:local:alice"},
				"projects": []string{"projects"},
			},
		},
		"two subjects": {
			[]string{"user:local:alice", "team:local:admins"},
			metadata.MD{
				"subjects": []string{"user:local:alice", "team:local:admins"},
				"resource": []string{"res"},
				"action":   []string{"act"},
				"projects": []string{"projects"},
			},
		},
		"one subject with spaces": {
			[]string{"user:local:alice schmidt"},
			metadata.MD{
				"subjects": []string{"user:local:alice schmidt"},
				"resource": []string{"res"},
				"action":   []string{"act"},
				"projects": []string{"projects"},
			},
		},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			ctx = auth_context.NewOutgoingContext(auth_context.NewContext(ctx, tc.input, []string{"projects"}, "res", "act"))
			md, ok := metadata.FromOutgoingContext(ctx)
			require.True(t, ok)
			assert.Equal(t, tc.expected, md)
		})
	}
}

func TestProjectsFromIncomingContext(t *testing.T) {
	tests := map[string]struct {
		input []string
	}{
		"a single project": {
			[]string{"project1"},
		},
		"multiple projects": {
			[]string{"project1", "project2", "project3"},
		},
		"no projects": {
			[]string{},
		},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			ctx := context.Background()
			gatewayContext := auth_context.NewOutgoingProjectsContext(auth_context.NewContext(ctx, []string{}, tc.input, "res", "act"))
			projects, err := auth_context.ProjectsFromIncomingContext(gatewayContext)
			require.NoError(t, err)
			assert.Equal(t, tc.input, projects)
		})
	}
}

func TestRoundTripMetadata(t *testing.T) {
	tests := map[string][]string{
		"no subjects":  {},
		"one subject":  {"user:local:alice"},
		"two subjects": {"user:local:alice", "team:local:admins"},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			ctx = auth_context.NewOutgoingContext(auth_context.NewContext(ctx,
				tc, []string{"projects"}, "res", "act"))
			md, ok := metadata.FromOutgoingContext(ctx)
			require.True(t, ok)
			ctx = metadata.NewIncomingContext(ctx, md)
			auth := auth_context.FromContext(auth_context.FromIncomingMetadata(ctx))
			assert.ElementsMatch(t, tc, auth.Subjects)
			assert.Equal(t, "res", auth.Resource)
			assert.Equal(t, "act", auth.Action)
		})
	}
}

func TestProjectsFromMetadata(t *testing.T) {
	tests := map[string]metadata.MD{
		"simple case":     metadata.Pairs("grpcgateway-projects", "foo,bar,baz"),
		"duplicates":      metadata.Pairs("grpcgateway-projects", "foo,bar,baz,baz"),
		"spaces":          metadata.Pairs("grpcgateway-projects", " foo  , bar,baz  "),
		"multiple values": metadata.Pairs("grpcgateway-projects", "foo", "grpcgateway-projects", "baz,bar"),
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			assert.ElementsMatch(t, []string{"foo", "bar", "baz"},
				auth_context.ProjectsFromMetadata(tc))
		})
	}
}
