package integration

import (
	"fmt"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
	"go.uber.org/multierr"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createV1PolicyTemplateStr = `
{
	"action": "{{ .Action }}",
	"subjects": ["token:{{ .TokenID }}"],
	"resource": "{{ .Resource }}"
}
`

// PolicyInfo contains info about a created policy
type PolicyInfo struct {
	ID string `json:"id"`
}

type authV1PoliciesSave struct {
	TokenID    string `json:"token_id"`
	TokenValue string `json:"token_value"`
	PolicyID   string `json:"policy_id"`
}

// CreateV1PolicyOnToken creates a v1 policy on a given tokenID
func CreateV1PolicyOnToken(tstCtx diagnostics.TestContext, tokenID string, action string, resource string) (*PolicyInfo, error) {
	policyInfo := PolicyInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/api/v0/auth/policies",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createV1PolicyTemplateStr, struct {
				Action   string
				TokenID  string
				Resource string
			}{
				Action:   action,
				TokenID:  tokenID,
				Resource: resource,
			}),
		)).WithValue(&policyInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not create policy")
	}

	return &policyInfo, nil
}

// DeletePolicy deletes a policy with the given id
func DeletePolicy(tstCtx diagnostics.TestContext, id string) error {
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return err
	}

	var reqPath string
	if isV2 {
		reqPath = fmt.Sprintf("/apis/iam/v2/policies/%s", id)
	} else {
		reqPath = fmt.Sprintf("/api/v0/auth/policies/%s", id)
	}

	err = MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			reqPath,
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not delete policy")
	}
	return nil
}

// CreateAuthV1PoliciesDiagnostic create the diagnostic struct for auth tokens
// and v1 policies.
func CreateAuthV1PoliciesDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-policies-v1",
		Tags: diagnostics.Tags{"auth"},
		Skip: func(tstCtx diagnostics.TestContext) (bool, string, error) {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return false, "", err
			}
			return isV2, "requires IAM v1", nil
		},
		Generate: func(tstCtx diagnostics.TestContext) error {
			tokenInfo, err := CreateRandomToken(tstCtx,
				fmt.Sprintf("auth-policies-v1-%s", TimestampName()))
			if err != nil {
				return err
			}

			policyInfo, err := CreateV1PolicyOnToken(tstCtx, tokenInfo.ID, "read", "service_info:version")
			if err != nil {
				return err
			}

			tstCtx.SetValue("auth-policies-v1", authV1PoliciesSave{
				TokenID:    tokenInfo.ID,
				PolicyID:   policyInfo.ID,
				TokenValue: tokenInfo.Value,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := authV1PoliciesSave{}
			err := tstCtx.GetValue("auth-policies-v1", &loaded)
			require.NoError(tstCtx, err, "Could not load generated context")
			err = MustJSONDecodeSuccess(
				tstCtx.DoLBRequest(
					"/api/v0/gateway/version",
					lbrequest.WithAuthToken(loaded.TokenValue),
				)).Error()
			require.NoError(tstCtx, err, "Expected to be able to read gateway version")
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := authV1PoliciesSave{}
			err := tstCtx.GetValue("auth-policies-v1", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not load generated context")
			}

			return multierr.Combine(
				DeletePolicy(tstCtx, loaded.PolicyID),
				DeleteToken(tstCtx, loaded.TokenID),
			)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateAuthV1PoliciesDiagnostic(),
	)
}
