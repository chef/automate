package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createProjectTemplate = `
{
	"id":"{{ .ID }}",
	"name":"{{ .ID }} test project"
}
`

// ProjectInfo represents the project parameters, including v1 and v2 fields.
type ProjectInfo struct {
	Project struct {
		ID   string `json:"id"`
		Name string `json:"name"`
	}
}

type authProjectSave struct {
	ID string `json:"id"`
}

// CreateRandomProject creates a project
func CreateRandomProject(tstCtx diagnostics.TestContext) (*ProjectInfo, error) {
	projectInfo := ProjectInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/apis/iam/v2/projects",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createProjectTemplate, struct {
				ID string
			}{
				ID: TimestampName(),
			}),
		)).WithValue(&projectInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not create project")
	}

	return &projectInfo, nil
}

// GetProject fetches the given project
func GetProject(tstCtx diagnostics.TestContext, id string) (*ProjectInfo, error) {
	projectInfo := ProjectInfo{}
	err := MustJSONDecodeSuccess(tstCtx.DoLBRequest(
		fmt.Sprintf("/apis/iam/v2/projects/%s", id),
	)).WithValue(&projectInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not fetch project")
	}
	return &projectInfo, nil
}

// DeleteProject deletes the project with the given id
func DeleteProject(tstCtx diagnostics.TestContext, id string) error {
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/projects/%s", id),
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not delete project")
	}
	return nil
}

// CreateAuthProjectsDiagnostic create the diagnostic struct for auth projects
func CreateAuthProjectsDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-projects",
		Tags: diagnostics.Tags{"auth"},
		Skip: func(tstCtx diagnostics.TestContext) (bool, string, error) {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return false, "", err
			}
			return !isV2, "requires IAM v2", nil
		},
		Generate: func(tstCtx diagnostics.TestContext) error {
			projectInfo, err := CreateRandomProject(tstCtx)
			if err != nil {
				return err
			}

			tstCtx.SetValue("auth-projects", &authProjectSave{
				ID: projectInfo.Project.ID,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := authProjectSave{}
			err := tstCtx.GetValue("auth-projects", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")

			projectInfo, err := GetProject(tstCtx, loaded.ID)
			require.NoError(tstCtx, err)

			assert.Equal(tstCtx, loaded.ID, projectInfo.Project.ID)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := authProjectSave{}
			err := tstCtx.GetValue("auth-projects", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}

			return DeleteProject(tstCtx, loaded.ID)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateAuthProjectsDiagnostic(),
	)
}
