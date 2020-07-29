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

type generatedTokenData struct {
	ID string `json:"id"`
}

// CreateToken creates a token with the given id
func CreateToken(tstCtx diagnostics.TestContext, id string) (*TokenInfo, error) {
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
			fmt.Sprintf("/api/v0/auth/tokens/%s", id),
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

	if err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			reqPath,
			lbrequest.WithMethod("DELETE"),
		)).WithValue(&struct{}{}); err != nil {
		return errors.Wrap(err, "Could not delete token")
	}
	return nil
}

// CreateIAMTokensDiagnostic creates the diagnostic struct for iam tokens
func CreateIAMTokensDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "iam-tokens",
		Tags: diagnostics.Tags{"iam"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			tokenInfo, err := CreateToken(tstCtx,
				fmt.Sprintf("iam-tokens-%s", TimestampName()))
			if err != nil {
				return err
			}

			tstCtx.SetValue("iam-tokens", &generatedTokenData{
				ID: tokenInfo.ID,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := generatedTokenData{}
			err := tstCtx.GetValue("iam-tokens", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")

			tokenInfo, err := GetToken(tstCtx, loaded.ID)
			require.NoError(tstCtx, err)

			assert.Equal(tstCtx, loaded.ID, tokenInfo.ID)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := generatedTokenData{}
			err := tstCtx.GetValue("iam-tokens", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}

			return DeleteToken(tstCtx, loaded.ID)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateIAMTokensDiagnostic(),
	)
}
