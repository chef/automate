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
		"user:local:{{ .UserID}}", 
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
}

type iamV2PoliciesSave struct {
	TokenID    string `json:"token_id"`
	TokenValue string `json:"token_value"`
	PolicyID   string `json:"policy_id"`
	TeamID     string `json:"team_id"`
	UserID     string `json:"user_id"`
	RoleID     string `json:"role_id"`
	ProjectID  string `json:"project_id"`
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

// CreateIAMV2PoliciesDiagnostic create the diagnostic struct for v2 policies.
func CreateIAMV2PoliciesDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "iam-policies-v2",
		Tags: diagnostics.Tags{"iam"},
		Skip: func(tstCtx diagnostics.TestContext) (bool, string, error) {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return false, "", err
			}
			return !isV2, "requires IAM v2", nil
		},
		Generate: func(tstCtx diagnostics.TestContext) error {
			// use a specific ID prefix so there are no conflicts with others tests
			// different resources can have the same ID
			id := fmt.Sprintf("iam-policies-v2-%s", TimestampName())

			// generate all the components of the policy
			tokenInfo, err := CreateRandomToken(tstCtx, id)
			if err != nil {
				return err
			}
			teamInfo, err := CreateRandomTeam(tstCtx, id)
			if err != nil {
				return err
			}
			userInfo, err := CreateRandomUser(tstCtx, id)
			if err != nil {
				return err
			}
			roleInfo, err := CreateRandomRole(tstCtx, id, "system:serviceVersion:get")
			if err != nil {
				return err
			}
			projectInfo, err := CreateRandomProjectWithRule(tstCtx, id)
			if err != nil {
				return err
			}

			pol := PolicyParameters{
				TokenID:   tokenInfo.ID,
				TeamID:    teamInfo.Team.ID,
				UserID:    userInfo.ID,
				RoleID:    roleInfo.Role.ID,
				ProjectID: projectInfo.Project.ID,
			}

			policyInfo, err := CreateV2Policy(tstCtx, pol)
			if err != nil {
				return err
			}

			tstCtx.SetValue("iam-policies-v2", iamV2PoliciesSave{
				TokenID:    pol.TokenID,
				TokenValue: tokenInfo.Value,
				PolicyID:   policyInfo.ID,
				TeamID:     pol.TeamID,
				UserID:     pol.UserID,
				RoleID:     pol.RoleID,
				ProjectID:  pol.ProjectID,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := iamV2PoliciesSave{}
			err := tstCtx.GetValue("iam-policies-v2", &loaded)
			require.NoError(tstCtx, err, "Could not load generated context")

			// assert policy is enforced
			err = MustJSONDecodeSuccess(
				tstCtx.DoLBRequest(
					"/api/v0/gateway/version",
					lbrequest.WithAuthToken(loaded.TokenValue),
				)).Error()
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
					Role:      loaded.RoleID,
					Resources: []string{"*"},
					Projects:  []string{loaded.ProjectID},
					Effect:    "ALLOW",
				},
			}
			err = MustJSONDecodeSuccess(tstCtx.DoLBRequest("/apis/iam/v2/policies/" + loaded.PolicyID)).
				WithValue(&resp)
			require.NoError(tstCtx, err, "Expected to be able to retrieve stored IAM v2 policy")
			require.Equal(tstCtx, loaded.PolicyID, resp.Policy.ID)
			require.Equal(tstCtx, "CUSTOM", resp.Policy.Type)
			require.Contains(tstCtx, resp.Policy.Members, fmt.Sprintf("token:%s", loaded.TokenID))
			require.ElementsMatch(tstCtx, expectedStmts, resp.Policy.Statements)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := iamV2PoliciesSave{}
			err := tstCtx.GetValue("iam-policies-v2", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not load generated context")
			}

			return multierr.Combine(
				DeletePolicy(tstCtx, loaded.PolicyID),
				DeleteToken(tstCtx, loaded.TokenID),
				DeleteTeam(tstCtx, loaded.TeamID),
				DeleteUser(tstCtx, loaded.UserID),
				DeleteRole(tstCtx, loaded.RoleID),
				DeleteProject(tstCtx, loaded.ProjectID),
			)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateIAMV2PoliciesDiagnostic(),
	)
}
