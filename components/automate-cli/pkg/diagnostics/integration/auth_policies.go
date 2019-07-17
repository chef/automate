package integration

import (
	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
	"go.uber.org/multierr"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

type authPoliciesSave struct {
	TokenID  string `json:"token_id"`
	Token    string `json:"token"`
	PolicyID string `json:"policy_id"`
}

// CreateAuthPoliciesDiagnostic create the diagnostic struct for auth tokens
// and v1 policies.
func CreateAuthPoliciesDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-policies",
		Tags: diagnostics.Tags{"auth", "iam-v1"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			tokenInfo, err := CreateRandomToken(tstCtx)
			if err != nil {
				return err
			}

			policyInfo, err := CreatePolicyOnToken(tstCtx, tokenInfo.ID, "read", "service_info:version")
			if err != nil {
				return err
			}

			tstCtx.SetValue("auth-policies", authPoliciesSave{
				TokenID:  tokenInfo.ID,
				PolicyID: policyInfo.ID,
				Token:    tokenInfo.Token,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := authPoliciesSave{}
			err := tstCtx.GetValue("auth-policies", &loaded)
			require.NoError(tstCtx, err, "Could not load generated context")
			err = MustJSONDecodeSuccess(
				tstCtx.DoLBRequest(
					"/api/v0/gateway/version",
					lbrequest.WithAuthToken(loaded.Token),
				)).Error()
			require.NoError(tstCtx, err, "Expected to be able to read gateway version")
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := authPoliciesSave{}
			err := tstCtx.GetValue("auth-policies", &loaded)
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
		CreateAuthPoliciesDiagnostic(),
	)
}
