package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createV1TokenTemplate = `
{
	"description":"{{ .Description }}",
	"active": true
}
`

const createV2TokenTemplate = `
{
	"id":"{{ .ID }}",
	"name":"{{ .ID }} test token"
}
`

// TokenInfo represents the token parameters, including v1 and v2 fields.
type TokenInfo struct {
	ID          string `json:"id"`
	Name        string `json:"name"`
	Description string `json:"description"`
	Value       string `json:"value"`
	Active      bool   `json:"active"`
}

// V2TokenInfo represents the nested response from the v2 Token API.
type V2TokenInfo struct {
	Token TokenInfo
}

type authTokenSave struct {
	ID string `json:"id"`
}

// CreateRandomToken creates a token
func CreateRandomToken(tstCtx diagnostics.TestContext, id string) (*TokenInfo, error) {
	var err error
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return nil, err
	}

	tokenInfo := TokenInfo{}
	if isV2 {
		v2TokenInfo := V2TokenInfo{Token: tokenInfo}
		err = MustJSONDecodeSuccess(
			tstCtx.DoLBRequest(
				"/apis/iam/v2/tokens",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONStringTemplateBody(createV2TokenTemplate, struct {
					ID string
				}{
					ID: id,
				}),
			)).WithValue(&v2TokenInfo)
		tokenInfo = v2TokenInfo.Token
	} else {
		err = MustJSONDecodeSuccess(
			tstCtx.DoLBRequest(
				"/api/v0/auth/tokens",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONStringTemplateBody(createV1TokenTemplate, struct {
					Description string
				}{
					Description: TimestampName(),
				}),
			)).WithValue(&tokenInfo)
	}

	if err != nil {
		return nil, errors.Wrap(err, "Could not create token")
	}

	return &tokenInfo, nil
}

// GetToken fetches the given token
func GetToken(tstCtx diagnostics.TestContext, id string) (*TokenInfo, error) {
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return nil, err
	}

	tokenInfo := TokenInfo{}
	if isV2 {
		v2TokenInfo := V2TokenInfo{Token: tokenInfo}
		err = MustJSONDecodeSuccess(tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/tokens/%s", id),
		)).WithValue(&v2TokenInfo)
		tokenInfo = v2TokenInfo.Token
	} else {
		err = MustJSONDecodeSuccess(tstCtx.DoLBRequest(
			fmt.Sprintf(fmt.Sprintf("/api/v0/auth/tokens/%s", id)),
		)).WithValue(&tokenInfo)
	}

	if err != nil {
		return nil, errors.Wrap(err, "Could not fetch token")
	}
	return &tokenInfo, nil
}

// DeleteToken deletes the token with the given id
func DeleteToken(tstCtx diagnostics.TestContext, id string) error {
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return err
	}

	var reqPath string
	if isV2 {
		reqPath = fmt.Sprintf("/apis/iam/v2/tokens/%s", id)
	} else {
		reqPath = fmt.Sprintf("/api/v0/auth/tokens/%s", id)
	}

	err = MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			reqPath,
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not delete token")
	}
	return nil
}

// CreateAuthTokensDiagnostic create the diagnostic struct for auth tokens
func CreateAuthTokensDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-tokens",
		Tags: diagnostics.Tags{"auth"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			tokenInfo, err := CreateRandomToken(tstCtx,
				fmt.Sprintf("auth-tokens-%s", TimestampName()))
			if err != nil {
				return err
			}

			tstCtx.SetValue("auth-tokens", &authTokenSave{
				ID: tokenInfo.ID,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := authTokenSave{}
			err := tstCtx.GetValue("auth-tokens", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")

			tokenInfo, err := GetToken(tstCtx, loaded.ID)
			require.NoError(tstCtx, err)

			assert.Equal(tstCtx, loaded.ID, tokenInfo.ID)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := authTokenSave{}
			err := tstCtx.GetValue("auth-tokens", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}

			return DeleteToken(tstCtx, loaded.ID)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateAuthTokensDiagnostic(),
	)
}
