package integration

import (
	"encoding/json"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

type cfgmgmtJobsConfigSave struct {
	Ran bool `json:"ran"`
}

// CreateCfgmgmtJobsConfigDiagnostic creates a diagnostic the checks to make
// sure the ingest service job config is correctly set after calling stop
// It assumes people haven't stopped it already
func CreateCfgmgmtJobsConfigDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "cfgmgmt-jobs-config",
		Tags: diagnostics.Tags{"cfgmgmt", "skip-for-deep-upgrade"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			reqPath := "/api/v0/retention/nodes/missing-nodes/config"

			resp, err := tstCtx.DoLBRequest(
				reqPath,
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONObjectBody(struct {
					Running bool `json:"running"`
				}{
					Running: false,
				}),
			)

			if err != nil {
				return errors.Wrapf(err, "Failed to POST %s", reqPath)
			}

			defer func() {
				_ = resp.Body.Close()
			}()
			if resp.StatusCode != 200 {
				return errors.Errorf("Failed to POST %s. Received status code %d", reqPath, resp.StatusCode)
			}

			tstCtx.SetValue("cfgmgmt-jobs-config", cfgmgmtJobsConfigSave{Ran: true})

			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := cfgmgmtJobsConfigSave{}
			err := tstCtx.GetValue("cfgmgmt-jobs-config", &loaded)
			require.NoError(tstCtx, err, "Generated context was not found")

			reqPath := "/api/v0/retention/nodes/status"
			resp, err := tstCtx.DoLBRequest(reqPath)

			require.NoError(tstCtx, err, "Failed to GET %s", reqPath)
			defer func() {
				_ = resp.Body.Close()
			}()

			type Job struct {
				Running *bool   `json:"running"`
				Name    *string `json:"name"`
			}

			type cfgmgmtJobStatusResponse struct {
				Jobs []*Job `json:"jobs"`
			}

			respUnmarshalled := cfgmgmtJobStatusResponse{}
			err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)
			require.NoError(tstCtx, err, "Failed to decode json for GET %s", reqPath)
			require.NotNil(tstCtx, respUnmarshalled.Jobs, "GET %s response missing jobs field", reqPath)

			changeFound := false
			for _, job := range respUnmarshalled.Jobs {
				require.NotNil(tstCtx, *job.Running, "GET %s response missing running field", reqPath)
				require.NotNil(tstCtx, *job.Name, "GET %s response missing name field", reqPath)
				if *job.Name == "missing_nodes" && *job.Running {
					changeFound = true
				}
			}

			assert.False(tstCtx, changeFound, "the missing nodes job should have running equal false")
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			reqPath := "/api/v0/retention/nodes/missing-nodes/config"
			resp, err := tstCtx.DoLBRequest(
				reqPath,
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONObjectBody(struct {
					Running bool `json:"running"`
				}{
					Running: true,
				}),
			)

			if err != nil {
				return errors.Wrapf(err, "Failed to POST %s", reqPath)
			}

			defer func() {
				_ = resp.Body.Close()
			}()
			if resp.StatusCode != 200 {
				return errors.Errorf("Failed to POST %s. Received status code %d", reqPath, resp.StatusCode)
			}

			return nil
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateCfgmgmtJobsConfigDiagnostic(),
	)
}
