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

const createV2PolicyTemplateStr = `
{
	"id": "{{ .ID }}",
	"name": "{{ .ID }} test policy",
	"members": ["token:{{ .TokenID }}"],
	"statements": [
		{
			"effect": "ALLOW",
			"actions": ["{{ .Action }}"],
			"projects": ["*"]
		}
	]
}
`

// PolicyInfo contains info about a created policy
type PolicyInfo struct {
	ID string `json:"id"`
}

// V2PolicyInfo represents the nested response from the v2 Policy API.
type V2PolicyInfo struct {
	Policy PolicyInfo
}

// CreatePolicyOnToken creates a policy on a given tokenID
func CreatePolicyOnToken(tstCtx diagnostics.TestContext, tokenID string, action string, resource string) (*PolicyInfo, error) {
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return nil, err
	}

	policyInfo := PolicyInfo{}
	if isV2 {
		v2PolicyInfo := V2PolicyInfo{Policy: policyInfo}
		err = MustJSONDecodeSuccess(
			tstCtx.DoLBRequest(
				"/apis/iam/v2/policies",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONStringTemplateBody(createV2PolicyTemplateStr, struct {
					ID      string
					Action  string
					TokenID string
				}{
					ID:      TimestampName(),
					Action:  action,
					TokenID: tokenID,
				}),
			)).WithValue(&v2PolicyInfo)
		policyInfo = v2PolicyInfo.Policy
	} else {
		err = MustJSONDecodeSuccess(
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
	}

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

type authPoliciesSave struct {
	TokenID    string `json:"token_id"`
	TokenValue string `json:"token_value"`
	PolicyID   string `json:"policy_id"`
}

// CreateAuthPoliciesDiagnostic create the diagnostic struct for auth tokens
// and v1 policies.
func CreateAuthPoliciesDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-policies",
		Tags: diagnostics.Tags{"auth"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return err
			}

			tokenInfo, err := CreateRandomToken(tstCtx)
			if err != nil {
				return err
			}

			var action string
			var resource string
			if isV2 {
				action = "system:serviceVersion:get"
				resource = "*"
			} else {
				action = "read"
				resource = "service_info:version"
			}
			// this helper knows how to use the correct version of the policy API
			// but we need to pass it the correctly versioned action and resource pair
			policyInfo, err := CreatePolicyOnToken(tstCtx, tokenInfo.ID, action, resource)
			if err != nil {
				return err
			}

			tstCtx.SetValue("auth-policies", authPoliciesSave{
				TokenID:    tokenInfo.ID,
				PolicyID:   policyInfo.ID,
				TokenValue: tokenInfo.Value,
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
					lbrequest.WithAuthToken(loaded.TokenValue),
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
