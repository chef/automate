package integration

import (
	"fmt"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
	"go.uber.org/multierr"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createV2PolicyTemplateStr = `
{
	"id": "{{ .ID }}",
	"name": "{{ .ID }} test policy",
	"members": [
		"token:{{ .TokenID }}",
		"user:local:{{ .UserID }}",
		"team:local:{{ .TeamID }}"
	],
	"statements": [
		{
			"effect": "ALLOW",
			"role": "{{ .RoleID }}",
			"projects": ["{{ .ProjectID }}"]
		}
	]
}
`

// V2PolicyInfo represents the nested response from the v2 Policy API.
type V2PolicyInfo struct {
	Policy PolicyInfo
}

// PolicyParameters represents policy inputs
type PolicyParameters struct {
	TokenID   string
	TeamID    string
	UserID    string
	RoleID    string
	ProjectID string
	RuleID    string
}

type generatedV2PolicyData struct {
	TokenID    string `json:"token_id"`
	TokenValue string `json:"token_value"`
	PolicyID   string `json:"policy_id"`
	TeamID     string `json:"team_id"`
	UserID     string `json:"user_id"`
	RoleID     string `json:"role_id"`
	ProjectID  string `json:"project_id"`
	RuleID     string `json:"rule_id"`
	Skipped    bool   `json:"skipped"`
}

// CreateV2Policy creates a policy using the given policy fields
func CreateV2Policy(tstCtx diagnostics.TestContext, pol PolicyParameters) (*PolicyInfo, error) {
	policyInfo := PolicyInfo{}
	v2PolicyInfo := V2PolicyInfo{Policy: policyInfo}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/apis/iam/v2/policies",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createV2PolicyTemplateStr, struct {
				ID        string
				TokenID   string
				UserID    string
				TeamID    string
				RoleID    string
				ProjectID string
			}{
				ID:        TimestampName(),
				TokenID:   pol.TokenID,
				UserID:    pol.UserID,
				TeamID:    pol.TeamID,
				RoleID:    pol.RoleID,
				ProjectID: pol.ProjectID,
			}),
		)).WithValue(&v2PolicyInfo)
	policyInfo = v2PolicyInfo.Policy

	if err != nil {
		return nil, errors.Wrap(err, "Could not create policy")
	}

	return &policyInfo, nil
}

// CreateIAMV2PoliciesDiagnostic creates the diagnostic struct for v2 policies.
func CreateIAMV2PoliciesDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "iam-policies-v2",
		Tags: diagnostics.Tags{"iam"},
		Skip: func(tstCtx diagnostics.TestContext) (bool, string, error) {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return false, "", err
			}

			// the Skip function is run before each step: Generate, Verify, and Cleanup
			loaded := generatedV2PolicyData{}
			err = tstCtx.GetValue("iam-policies-v2", &loaded)
			if err != nil {
				// this is the first time running this diagnostic,
				// so we save whether or not we're skipping it
				tstCtx.SetValue("iam-policies-v2", &generatedV2PolicyData{
					Skipped: !isV2,
				})
			} else {
				// the diagnostic has been run before, so we keep track of its saved values
				tstCtx.SetValue("iam-policies-v2", &generatedV2PolicyData{
					TokenID:    loaded.TokenID,
					TokenValue: loaded.TokenValue,
					PolicyID:   loaded.PolicyID,
					TeamID:     loaded.TeamID,
					UserID:     loaded.UserID,
					RoleID:     loaded.RoleID,
					ProjectID:  loaded.ProjectID,
					RuleID:     loaded.RuleID,
					Skipped:    loaded.Skipped,
				})
			}

			return !isV2, "requires IAM v2", nil
		},
		Generate: func(tstCtx diagnostics.TestContext) error {
			// use a specific ID prefix so there are no conflicts with other tests.
			// different resources can have the same ID
			id := fmt.Sprintf("iam-v2-%s", TimestampName())

			// generate all the components of the policy
			tokenInfo, err := CreateToken(tstCtx, id)
			if err != nil {
				return err
			}
			teamInfo, err := CreateTeam(tstCtx, id)
			if err != nil {
				return err
			}
			userInfo, err := CreateUser(tstCtx, id)
			if err != nil {
				return err
			}
			roleInfo, err := CreateRole(tstCtx, id, "system:serviceVersion:get")
			if err != nil {
				return err
			}
			projectInfo, err := CreateProjectWithRule(tstCtx, id)
			if err != nil {
				return err
			}

			pol := PolicyParameters{
				TokenID:   tokenInfo.ID,
				TeamID:    teamInfo.Team.ID,
				UserID:    userInfo.ID,
				RoleID:    roleInfo.Role.ID,
				ProjectID: projectInfo.Project.ID,
				RuleID:    projectInfo.Project.Rule.ID,
			}

			policyInfo, err := CreateV2Policy(tstCtx, pol)
			if err != nil {
				return err
			}

			loaded := generatedV2PolicyData{}
			err = tstCtx.GetValue("iam-policies-v2", &loaded)
			if err != nil {
				return errors.Wrap(err, "could not find generated context")
			}

			tstCtx.SetValue("iam-policies-v2", generatedV2PolicyData{
				TokenID:    pol.TokenID,
				TokenValue: tokenInfo.Value,
				PolicyID:   policyInfo.ID,
				TeamID:     pol.TeamID,
				UserID:     pol.UserID,
				RoleID:     pol.RoleID,
				ProjectID:  pol.ProjectID,
				RuleID:     projectInfo.Project.Rule.ID,
				Skipped:    loaded.Skipped,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			// in case there was an upgrade, check for v1 policy data
			v1loaded := generatedV1PolicyData{}
			err := tstCtx.GetValue("iam-policies-v1", &v1loaded)
			// if generate was run while on v1 and cleanup was skipped,
			// we can verify that the v1 policy was migrated to v2
			if err == nil {
				// the policy should exist as a migrated v2 policy
				err = MustJSONDecodeSuccess(
					tstCtx.DoLBRequest(
						fmt.Sprintf("/apis/iam/v2/policies/%s", v1loaded.PolicyID),
					)).WithValue(&struct{}{})
				require.NoError(tstCtx, err, "Expected to be able to read gateway version")

				err = MustJSONDecodeSuccess(
					tstCtx.DoLBRequest(
						"/api/v0/gateway/version",
						lbrequest.WithAuthToken(v1loaded.TokenValue),
					)).WithValue(&struct{}{})
				require.NoError(tstCtx, err, "Expected to be able to read gateway version")
			}

			v2loaded := generatedV2PolicyData{}
			err = tstCtx.GetValue("iam-policies-v2", &v2loaded)
			require.NoError(tstCtx, err, "Could not find generated context")
			if v2loaded.Skipped {
				// in the v1->v2 force upgrade scenario:
				// we only migrate v1 policy data,
				// we do not generate v2 policies
				return
			}

			// assert v2 policy is enforced by fetching the gateway version with the member token.
			// this endpoint corresponds to the "system:serviceVersion:get" action
			// (which is the policy's role's action).
			err = MustJSONDecodeSuccess(
				tstCtx.DoLBRequest(
					"/api/v0/gateway/version",
					lbrequest.WithAuthToken(v2loaded.TokenValue),
				)).WithValue(&struct{}{})
			require.NoError(tstCtx, err, "Expected to be able to read gateway version")

			type Statement struct {
				Role      string
				Resources []string
				Projects  []string
				Effect    string
			}
			resp := struct {
				Policy struct {
					ID, Type   string
					Members    []string
					Statements []Statement
				}
			}{}
			expectedStmts := []Statement{
				{
					Role:      v2loaded.RoleID,
					Resources: []string{"*"},
					Projects:  []string{v2loaded.ProjectID},
					Effect:    "ALLOW",
				},
			}
			err = MustJSONDecodeSuccess(tstCtx.DoLBRequest("/apis/iam/v2/policies/" + v2loaded.PolicyID)).
				WithValue(&resp)
			require.NoError(tstCtx, err, "Expected to be able to retrieve stored IAM v2 policy")
			require.Equal(tstCtx, v2loaded.PolicyID, resp.Policy.ID)
			require.Equal(tstCtx, "CUSTOM", resp.Policy.Type)
			require.Contains(tstCtx, resp.Policy.Members, fmt.Sprintf("token:%s", v2loaded.TokenID))
			require.ElementsMatch(tstCtx, expectedStmts, resp.Policy.Statements)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := generatedV2PolicyData{}
			err := tstCtx.GetValue("iam-policies-v2", &loaded)
			if loaded.Skipped {
				// if diagnostic was run on v1, generating v2 policies was skipped
				// so nothing to clean up here
				return nil
			}
			if err != nil {
				return errors.Wrap(err, "Could not load generated context")
			}

			return multierr.Combine(
				DeletePolicy(tstCtx, loaded.PolicyID),
				DeleteToken(tstCtx, loaded.TokenID),
				DeleteTeam(tstCtx, loaded.TeamID),
				DeleteUser(tstCtx, loaded.UserID),
				DeleteRole(tstCtx, loaded.RoleID),
				DeleteProjectWithRule(tstCtx, loaded.ProjectID, loaded.RuleID),
			)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateIAMV2PoliciesDiagnostic(),
	)
}
