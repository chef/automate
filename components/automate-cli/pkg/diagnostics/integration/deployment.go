package integration

import (
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/lib/io/fileutils"
)

type chefServerSave struct {
	ChefServerEnabled bool `json:"chef_server_enabled"`
}

type versionResponse struct {
	Services []serviceVersion `json:"services"`
}

type serviceVersion struct {
	Name    string `json:"name"`
	Origin  string `json:"origin"`
	Version string `json:"version"`
	Release string `json:"release"`
}

func CreateDeploymentDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "deployment",
		Tags: diagnostics.Tags{"deployment", "skip-for-deep-upgrade"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			resp := versionResponse{}

			err := MustJSONDecodeSuccess(
				tstCtx.DoLBRequest("/api/v1/deployment/service_versions")).WithValue(&resp)
			if err != nil {
				return errors.Wrap(err, "could not query service_versions")
			}

			csEnabled := false
			for _, s := range resp.Services {
				if s.Name == "automate-cs-nginx" {
					csEnabled = true
				}
			}

			tstCtx.SetValue("deployment-chef-server-enabled",
				chefServerSave{ChefServerEnabled: csEnabled})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := chefServerSave{}
			require.NoError(tstCtx, tstCtx.GetValue("deployment-chef-server-enabled", &loaded),
				"Generated context was not found")

			binlinks := []string{"/bin/chef-automate"}
			if loaded.ChefServerEnabled {
				binlinks = append(binlinks, "/bin/knife", "/bin/chef-server-ctl")
			}

			for _, p := range binlinks {
				isSym, err := fileutils.IsSymlink(p)
				assert.NoError(tstCtx, err, "stat failed for %s", p)
				assert.True(tstCtx, isSym, "%s should be a symlink", p)
			}
		},
		Cleanup: func(diagnostics.TestContext) error {
			return nil
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(CreateDeploymentDiagnostic())
}
