package integration

import (
	"context"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/selfupdater/updatesource"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
)

func CreateCLIUpdaterDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "cli-updater",
		// TODO(jaym): remove-this-tag-after-merge and "skip-for-deep-upgrade" can be removed after a promotion to acceptance
		Tags: diagnostics.Tags{"deployment-service", "cli", "skip-for-deep-upgrade", "remove-this-tag-after-merge"},
		Verify: func(t diagnostics.VerificationTestContext) {
			connection, err := client.Connection(500 * time.Millisecond)
			require.NoError(t, err, "Unable to connect to server to get its version. Skipping version check.")
			defer func() {
				_ = connection.Close()
			}()

			updater := updatesource.DeploymentService(connection)
			desiredVersion, err := updater.DesiredVersion(context.Background())
			require.NoError(t, err)
			_, version, err := updater.FetchLatest(context.Background())
			require.NoError(t, err, "Failed to fetch latest executable")
			assert.Equal(t, desiredVersion, version)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateCLIUpdaterDiagnostic(),
	)
}
