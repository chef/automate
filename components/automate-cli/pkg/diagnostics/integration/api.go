package integration

import (
	"fmt"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const tokenCreateTemplateStr = `
{
	"description": "{{ .Description }}",
	"active": true
}
`

// TokenInfo holds information about a created token
type TokenInfo struct {
	ID          string `json:"id"`
	Description string `json:"description"`
	Active      bool   `json:"active"`
	Token       string `json:"value"`
}

// CreateRandomToken creates a token
func CreateRandomToken(tstCtx diagnostics.TestContext) (*TokenInfo, error) {
	tokenInfo := TokenInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/api/v0/auth/tokens",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(tokenCreateTemplateStr, struct {
				Description string
			}{
				Description: TimestampName(),
			}),
		)).WithValue(&tokenInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not create token")
	}

	return &tokenInfo, nil
}

// DeleteToken deletes the token with the given id
func DeleteToken(tstCtx diagnostics.TestContext, id string) error {
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/api/v0/auth/tokens/%s", id),
			lbrequest.WithMethod("DELETE"),
			lbrequest.WithJSONStringTemplateBody(tokenCreateTemplateStr, struct {
				Description string
			}{
				Description: TimestampName(),
			}),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not delete token")
	}

	return nil
}

const policyCreateTemplateStr = `
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

// CreatePolicyOnToken creates a policy on a given tokenID
func CreatePolicyOnToken(tstCtx diagnostics.TestContext, tokenID string, action string, resource string) (*PolicyInfo, error) {
	policyInfo := PolicyInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/api/v0/auth/policies",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(policyCreateTemplateStr, struct {
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
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/api/v0/auth/policies/%s", id),
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not create policy")
	}

	return nil
}
