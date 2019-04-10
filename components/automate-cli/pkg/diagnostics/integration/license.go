package integration

import (
	"encoding/json"
	"fmt"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
)

type licenseSave struct {
	LicenseID string `json:"license_id"`
}

type telemetryConfigResp struct {
	LicenseID string `json:"license_id"`
}

// CreateLicenseDiagnostic create the diagnostic struct for the license service
// It will disabled telemetry. It verifies that telemetry is disabled, along with
// a license is present
func CreateLicenseDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "license",
		Generate: func(tstCtx diagnostics.TestContext) error {
			currentConfig, err := telemetryConfig(tstCtx)
			if err != nil {
				return errors.Wrap(err, "Could not get current telemetry config")
			}

			tstCtx.SetValue("license", licenseSave{
				LicenseID: currentConfig.LicenseID,
			})
			return nil
		},

		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := licenseSave{}
			err := tstCtx.GetValue("license", &loaded)
			require.NoError(tstCtx, err, "Generated context was not found")

			resp, err := telemetryConfig(tstCtx)
			require.NoError(tstCtx, err)
			assert.Equal(tstCtx, loaded.LicenseID, resp.LicenseID, "Unexpected license id")
		},

		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := licenseSave{}
			err := tstCtx.GetValue("license", &loaded)
			if err != nil {
				return errors.Wrap(err, "Generated context was not found")
			}

			return err
		},
	}
}

func telemetryConfig(tstCtx diagnostics.TestContext) (*telemetryConfigResp, error) {
	reqPath := fmt.Sprintf("/api/v0/telemetry/config")
	resp, err := tstCtx.DoLBRequest(reqPath)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to GET %s", reqPath)
	}

	defer func() {
		_ = resp.Body.Close()
	}()

	if resp.StatusCode != 200 {
		return nil, errors.Errorf("Failed to GET %s with status code %d", reqPath, resp.StatusCode)
	}

	respUnmarshalled := telemetryConfigResp{}
	err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to decode GET %s", reqPath)
	}
	return &respUnmarshalled, nil
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateLicenseDiagnostic(),
	)
}
