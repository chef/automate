package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createRoleTemplate = `
{
	"id":"{{ .ID }}",
	"name":"{{ .ID }} test role",
	"actions": ["{{ .Action }}"]
}
`

// RoleInfo represents the role parameters, including v1 and v2 fields.
type RoleInfo struct {
	Role struct {
		ID      string   `json:"id"`
		Name    string   `json:"name"`
		Actions []string `json:"actions"`
	}
}

type generatedRoleData struct {
	ID      string   `json:"id"`
	Actions []string `json:"actions"`
	Skipped bool     `json:"skipped"`
}

// CreateRole creates a role with the given id and action
func CreateRole(tstCtx diagnostics.TestContext, id string, action string) (*RoleInfo, error) {
	roleInfo := RoleInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/apis/iam/v2/roles",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createRoleTemplate, struct {
				ID     string
				Action string
			}{
				ID:     id,
				Action: action,
			}),
		)).WithValue(&roleInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not create role")
	}

	return &roleInfo, nil
}

// GetRole fetches the given role
func GetRole(tstCtx diagnostics.TestContext, id string) (*RoleInfo, error) {
	roleInfo := RoleInfo{}
	err := MustJSONDecodeSuccess(tstCtx.DoLBRequest(
		fmt.Sprintf("/apis/iam/v2/roles/%s", id),
	)).WithValue(&roleInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not fetch role")
	}
	return &roleInfo, nil
}

// DeleteRole deletes the role with the given id
func DeleteRole(tstCtx diagnostics.TestContext, id string) error {
	if err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/roles/%s", id),
			lbrequest.WithMethod("DELETE"),
		)).WithValue(&struct{}{}); err != nil {
		return errors.Wrap(err, "Could not delete role")
	}
	return nil
}

// CreateIAMRolesDiagnostic creates the diagnostic struct for iam roles
func CreateIAMRolesDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "iam-roles",
		Tags: diagnostics.Tags{"iam"},
		Skip: func(tstCtx diagnostics.TestContext) (bool, string, error) {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return false, "", err
			}

			// the Skip function is run before each step: Generate, Verify, and Cleanup
			loaded := generatedRoleData{}
			err = tstCtx.GetValue("iam-roles", &loaded)
			if err != nil {
				// this is the first time running this diagnostic,
				// so we save whether or not we're skipping it
				tstCtx.SetValue("iam-roles", &generatedRoleData{
					Skipped: !isV2,
				})
			} else {
				// the diagnostic has been run before, so we keep track of its saved values
				tstCtx.SetValue("iam-roles", &generatedRoleData{
					ID:      loaded.ID,
					Actions: loaded.Actions,
					Skipped: loaded.Skipped,
				})
			}

			return !isV2, "requires IAM v2", nil
		},
		Generate: func(tstCtx diagnostics.TestContext) error {
			roleInfo, err := CreateRole(tstCtx,
				fmt.Sprintf("iam-roles-%s", TimestampName()), "system:serviceVersion:get")
			if err != nil {
				return err
			}

			loaded := generatedRoleData{}
			err = tstCtx.GetValue("iam-roles", &loaded)
			if err != nil {
				return errors.Wrap(err, "could not find generated context")
			}

			tstCtx.SetValue("iam-roles", &generatedRoleData{
				ID:      roleInfo.Role.ID,
				Actions: roleInfo.Role.Actions,
				Skipped: loaded.Skipped,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := generatedRoleData{}
			err := tstCtx.GetValue("iam-roles", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")
			if loaded.Skipped {
				// this happens in the v1->v2 force upgrade scenario:
				// when we run generate diagnostic data while on v1
				// then force-upgrade to v2
				// then run verify and cleanup on that data
				// for roles, since the Generate step was skipped
				// there is nothing to verify on v2
				return
			}

			roleInfo, err := GetRole(tstCtx, loaded.ID)
			require.NoError(tstCtx, err)

			assert.Equal(tstCtx, loaded.ID, roleInfo.Role.ID)
			assert.Equal(tstCtx, loaded.Actions, roleInfo.Role.Actions)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := generatedRoleData{}
			err := tstCtx.GetValue("iam-roles", &loaded)
			if loaded.Skipped {
				// if diagnostic was run on v1, generating roles was skipped
				// so nothing to clean up here
				return nil
			}
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}

			return DeleteRole(tstCtx, loaded.ID)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateIAMRolesDiagnostic(),
	)
}
