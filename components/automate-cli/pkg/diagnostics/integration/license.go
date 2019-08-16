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

type licenseStatusResp struct {
	LicenseID string `json:"license_id"`
}

// CreateLicenseDiagnostic returns a diagnostic test that tests the
// license status API endpoint. It does not add a new license but
// rather only verifies that the applied license hasn't changed.
func CreateLicenseDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "license",
		Generate: func(tstCtx diagnostics.TestContext) error {
			licenseID, err := getLicenseID(tstCtx)
			if err != nil {
				return errors.Wrap(err, "Could not get current license ID")
			}

			tstCtx.SetValue("license", licenseSave{
				LicenseID: licenseID,
			})
			return nil
		},

		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := licenseSave{}
			err := tstCtx.GetValue("license", &loaded)
			require.NoError(tstCtx, err, "Generated context was not found")

			licenseID, err := getLicenseID(tstCtx)
			require.NoError(tstCtx, err)
			assert.Equal(tstCtx, loaded.LicenseID, licenseID, "Unexpected license id")
		},
		Cleanup: func(diagnostics.TestContext) error { return nil },
	}
}

func getLicenseID(tstCtx diagnostics.TestContext) (string, error) {
	reqPath := fmt.Sprintf("/api/v0/license/status")
	resp, err := tstCtx.DoLBRequest(reqPath)
	if err != nil {
		return "", errors.Wrapf(err, "Failed to GET %s", reqPath)
	}

	defer func() {
		_ = resp.Body.Close()
	}()

	if resp.StatusCode != 200 {
		return "", errors.Errorf("Failed to GET %s with status code %d", reqPath, resp.StatusCode)
	}

	respUnmarshalled := licenseStatusResp{}
	err = json.NewDecoder(resp.Body).Decode(&respUnmarshalled)
	if err != nil {
		return "", errors.Wrapf(err, "Failed to decode GET %s", reqPath)
	}
	return respUnmarshalled.LicenseID, nil
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateLicenseDiagnostic(),
	)
}
