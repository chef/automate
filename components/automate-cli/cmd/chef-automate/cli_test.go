package main

import (
	"context"
	"testing"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/deployment"
)

type mockVersionManifestClient struct {
	poison bool
	t      *testing.T
	err    error
}

func (m *mockVersionManifestClient) ManifestVersion(ctx context.Context, in *api.ManifestVersionRequest, opts ...grpc.CallOption) (*api.ManifestVersionResponse, error) {
	if m.poison {
		require.Fail(m.t, "unexpected call")
	}
	return nil, m.err
}

func TestVersionCheck(t *testing.T) {
	t.Run("Command executes if we cannot get the version from the server", func(t *testing.T) {
		cmdName := "test-runs-if-server-fails-fails"
		ran := false

		RootCmd.AddCommand(&cobra.Command{
			Use: cmdName,
			Run: func(*cobra.Command, []string) {
				ran = true
			},
			Annotations: map[string]string{
				NoRequireRootAnnotation: NoRequireRootAnnotation,
			},
		})
		injectedVersionClient = &mockVersionManifestClient{
			err: errors.New("Bad things"),
		}

		RootCmd.SetArgs([]string{cmdName})
		RootCmd.Execute()

		assert.True(t, ran)
	})

	t.Run("Version check is skipped if it has the annotation", func(t *testing.T) {
		cmdName := "test-skip-if-annotated"
		ran := false

		RootCmd.AddCommand(&cobra.Command{
			Use: cmdName,
			Annotations: map[string]string{
				NoRequireRootAnnotation:  NoRequireRootAnnotation,
				NoCheckVersionAnnotation: NoCheckVersionAnnotation,
			},
			Run: func(*cobra.Command, []string) {
				ran = true
			},
		})
		injectedVersionClient = &mockVersionManifestClient{
			t:      t,
			poison: true,
		}

		RootCmd.SetArgs([]string{cmdName})
		RootCmd.Execute()

		assert.True(t, ran)
	})

	t.Run("Version check is skipped for help command", func(t *testing.T) {
		cmdName := "help"

		injectedVersionClient = &mockVersionManifestClient{
			t:      t,
			poison: true,
		}

		RootCmd.SetArgs([]string{cmdName})
		RootCmd.Execute()
	})
}
