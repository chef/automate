package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createV1UserTemplate = `
{
	"username":"{{ .Name }}",
	"name":"{{ .Name }} test user",
	"password":"{{ .Password }}"
}
`

const createV2UserTemplate = `
{
	"id":"{{ .ID }}",
	"name":"{{ .ID }} test user",
	"password":"{{ .Password }}"
}
`

// UserInfo represents the user parameters, including v1 and v2 fields.
// Password will be provided when only when creating, not when fetching.
type UserInfo struct {
	ID       string `json:"id"`
	Name     string `json:"name"`
	Username string `json:"username"`
	Password string
	IsV2     bool
}

// V2UserInfo represents the nested response from the v2 User API.
type V2UserInfo struct {
	User UserInfo
}

type iamUserSave struct {
	ID       string `json:"id"`
	Username string `json:"username"`
	IsV2     bool   `json:"is_v2"`
}

// CreateRandomUser creates a random user
func CreateRandomUser(tstCtx diagnostics.TestContext, id string) (*UserInfo, error) {
	var err error
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return nil, err
	}

	userInfo := UserInfo{IsV2: isV2}
	if isV2 {
		v2UserInfo := V2UserInfo{User: userInfo}
		err = MustJSONDecodeSuccess(
			tstCtx.DoLBRequest(
				"/apis/iam/v2/users",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONStringTemplateBody(createV2UserTemplate, struct {
					ID       string
					Password string
				}{
					ID:       id,
					Password: Password(),
				}),
			)).WithValue(&v2UserInfo)
		userInfo = v2UserInfo.User
	} else {
		err = MustJSONDecodeSuccess(
			tstCtx.DoLBRequest(
				"/api/v0/auth/users",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONStringTemplateBody(createV1UserTemplate, struct {
					Name     string
					Password string
				}{
					Name:     TimestampName(),
					Password: Password(),
				}),
			)).WithValue(&userInfo)
	}

	if err != nil {
		return nil, errors.Wrap(err, "Could not create user")
	}
	return &userInfo, nil
}

// GetUser fetches the given user
func GetUser(tstCtx diagnostics.TestContext, id string) (*UserInfo, error) {
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return nil, err
	}

	userInfo := UserInfo{}
	if isV2 {
		v2UserInfo := V2UserInfo{User: userInfo}
		err = MustJSONDecodeSuccess(tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/users/%s", id),
		)).WithValue(&v2UserInfo)
		userInfo = v2UserInfo.User
	} else {
		err = MustJSONDecodeSuccess(tstCtx.DoLBRequest(
			fmt.Sprintf(fmt.Sprintf("/api/v0/auth/users/%s", id)),
		)).WithValue(&userInfo)
	}

	if err != nil {
		return nil, errors.Wrap(err, "Could not fetch user")
	}
	return &userInfo, nil
}

// DeleteUser user deletes the given user
func DeleteUser(tstCtx diagnostics.TestContext, username string) error {
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return err
	}

	var reqPath string
	if isV2 {
		reqPath = fmt.Sprintf("/apis/iam/v2/users/%s", username)
	} else {
		reqPath = fmt.Sprintf("/api/v0/auth/users/%s", username)
	}

	err = MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			reqPath,
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not delete user")
	}
	return nil
}

// GetUserID determines which identifier to use when fetching the user, since
// the ID field is different across IAM v1 and and v2
func GetUserID(user iamUserSave) (string, error) {
	if !user.IsV2 {
		// if the user was saved as a v1 user, its identifier is its Username
		return user.Username, nil
	}

	return user.ID, nil
}

// CreateIAMUsersDiagnostic create the diagnostic struct for iam users
func CreateIAMUsersDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "iam-users",
		Tags: diagnostics.Tags{"iam"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			userInfo, err := CreateRandomUser(tstCtx, fmt.Sprintf("iam-users-%s", TimestampName()))
			if err != nil {
				return err
			}

			tstCtx.SetValue("iam-users", &iamUserSave{
				ID:       userInfo.ID,
				Username: userInfo.Username,
				IsV2:     userInfo.IsV2,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := iamUserSave{}
			err := tstCtx.GetValue("iam-users", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")

			id, err := GetUserID(loaded)
			require.NoError(tstCtx, err, "Could not determine user identifier")
			userInfo, err := GetUser(tstCtx, id)
			require.NoError(tstCtx, err)

			isV2, err := tstCtx.IsIAMV2()
			require.NoError(tstCtx, err)
			if isV2 {
				assert.Equal(tstCtx, id, userInfo.ID)
			} else {
				assert.Equal(tstCtx, id, userInfo.Username)
			}
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := iamUserSave{}
			err := tstCtx.GetValue("iam-users", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}

			id, err := GetUserID(loaded)
			if err != nil {
				return errors.Wrap(err, "Could not determine user identifier")
			}
			return DeleteUser(tstCtx, id)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateIAMUsersDiagnostic(),
	)
}
