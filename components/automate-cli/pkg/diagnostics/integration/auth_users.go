package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createUserTemplate = `
{
	"name":"{{ .Name }}",
	"username":"{{ .Name }}",
	"password":"{{ .Password }}"
}
`

// UserInfo represents the user parameters. Password will not
// be provided when fetching, only when creating.
type UserInfo struct {
	ID       string `json:"id"`
	Name     string `json:"name"`
	Username string `json:"username"`
	Password string
}

// CreateRandomUser creates a random user
func CreateRandomUser(tstCtx diagnostics.TestContext) (*UserInfo, error) {
	userInfo := UserInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/api/v0/auth/users",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createUserTemplate, struct {
				Name     string
				Password string
			}{
				Name:     TimestampName(),
				Password: Password(),
			}),
		)).WithValue(&userInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to create user")
	}

	return &userInfo, nil
}

// GetUser fetches the given user
func GetUser(tstCtx diagnostics.TestContext, username string) (*UserInfo, error) {
	userInfo := UserInfo{}
	reqPath := fmt.Sprintf("/api/v0/auth/users/%s", username)
	err := MustJSONDecodeSuccess(tstCtx.DoLBRequest(reqPath)).WithValue(&userInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to fetch user")
	}
	return &userInfo, nil
}

// DeleteUser user deletes the given user
func DeleteUser(tstCtx diagnostics.TestContext, username string) error {
	reqPath := fmt.Sprintf("/api/v0/auth/users/%s", username)
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			reqPath,
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Failed to delete user")
	}
	return nil
}

type authUsersSave struct {
	ID       string `json:"id"`
	Username string `json:"username"`
}

// CreateAuthUsersDiagnostic create the diagnostic struct for auth users
func CreateAuthUsersDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-users",
		Tags: diagnostics.Tags{"auth"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			userInfo, err := CreateRandomUser(tstCtx)
			if err != nil {
				return err
			}

			tstCtx.SetValue("auth-users", &authUsersSave{
				ID:       userInfo.ID,
				Username: userInfo.Username,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := authUsersSave{}
			err := tstCtx.GetValue("auth-users", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")
			userInfo, err := GetUser(tstCtx, loaded.Username)
			require.NoError(tstCtx, err)
			assert.Equal(tstCtx, loaded.ID, userInfo.ID)
			assert.Equal(tstCtx, loaded.Username, userInfo.Username)

		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := authUsersSave{}
			err := tstCtx.GetValue("auth-users", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}
			return DeleteUser(tstCtx, loaded.Username)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateAuthUsersDiagnostic(),
	)
}
