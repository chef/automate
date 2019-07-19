package integration

import (
	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
	uuid "github.com/chef/automate/lib/uuid4"
)

type save struct {
	PolicyID string `json:"id"`
	RoleID   string `json:"role_id"`
}

// This is used to ensure the response body is valid JSON, where we don't
// actually care about the content.
type empty struct{}

const roleCreateTemplateStr = `
{
  "id": "{{ .ID }}",
  "name": "{{ .Name }}",
	"actions": ["test:svc:someroleaction", "test:svc:someotherroleaction"]
}
`

const v2PolicyCreateTemplateStr = `
{
  "id": "{{ .ID }}",
  "name": "{{ .Name }}",
  "members": ["user:local:testuser", "team:local:testteam"],
  "statements": [
    {
      "effect": "DENY",
      "role": "{{ .RoleID }}"
    },
    {
      "effect": "ALLOW",
      "actions": ["test:svc:someaction", "test:svc:otheraction"]
    }
  ]
}
`

// CreateIAMV2Diagnostic create the diagnostic struct for IAM v2 data.
func CreateIAMV2Diagnostic() diagnostics.Diagnostic {
	policyID := "test-policy-" + uuid.Must(uuid.NewV4()).String()
	policyName := "This is a test IAM v2 backup and restore policy."
	policyType := "CUSTOM"
	roleID := "test-role-" + uuid.Must(uuid.NewV4()).String()
	roleName := "This is a test IAM v2 backup and restore role."

	return diagnostics.Diagnostic{
		Name: "iam-v2",
		Tags: diagnostics.Tags{"auth", "iam-v2", "skip-for-deep-upgrade"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			tstCtx.SetValue("iam-v2-policy-id", save{PolicyID: policyID, RoleID: roleID})
			err := MustJSONDecodeSuccess(
				tstCtx.DoLBRequest("/apis/iam/v2beta/roles",
					lbrequest.WithMethod("POST"),
					lbrequest.WithJSONStringTemplateBody(roleCreateTemplateStr,
						struct{ ID, Name string }{ID: roleID, Name: roleName}),
				)).WithValue(&empty{})

			if err != nil {
				return errors.Wrap(err, "Could not create role for use in IAM v2 policy")
			}

			err = MustJSONDecodeSuccess(
				tstCtx.DoLBRequest("/apis/iam/v2beta/policies",
					lbrequest.WithMethod("POST"),
					lbrequest.WithJSONStringTemplateBody(v2PolicyCreateTemplateStr,
						struct{ ID, Name, RoleID string }{ID: policyID, Name: policyName, RoleID: roleID}),
				)).WithValue(&empty{})
			return errors.Wrap(err, "Could not create IAM v2 policy")
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := save{}
			require.NoError(tstCtx, tstCtx.GetValue("iam-v2-policy-id", &loaded),
				"Generated context was not found")

			type Statement struct {
				Resources []string
				Actions   []string
				Role      string
				Effect    string
			}
			resp := struct {
				Policy struct {
					Id, Name, Type string
					Statements     []Statement
				}
			}{}
			expectedStmts := []Statement{
				{
					Actions:   []string{"test:svc:someaction", "test:svc:otheraction"},
					Resources: []string{"*"},
					Effect:    "ALLOW",
				},
				{
					Role:      loaded.RoleID,
					Actions:   []string{},
					Resources: []string{"*"},
					Effect:    "DENY",
				},
			}
			err := MustJSONDecodeSuccess(tstCtx.DoLBRequest("/apis/iam/v2beta/policies/" + loaded.PolicyID)).
				WithValue(&resp)
			require.NoError(tstCtx, err, "Expected to be able to retrieve stored IAM v2 policy")
			require.Equal(tstCtx, policyName, resp.Policy.Name)
			require.Equal(tstCtx, loaded.PolicyID, resp.Policy.Id)
			require.Equal(tstCtx, policyType, resp.Policy.Type)
			require.ElementsMatch(tstCtx, expectedStmts, resp.Policy.Statements)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := save{}
			if err := tstCtx.GetValue("iam-v2-policy-id", &loaded); err != nil {
				return errors.Wrap(err, "Generated context was not found")
			}

			err := MustJSONDecodeSuccess(
				tstCtx.DoLBRequest("/apis/iam/v2beta/roles/"+loaded.RoleID,
					lbrequest.WithMethod("DELETE")),
			).WithValue(&empty{})

			if err != nil {
				return errors.Wrap(err, "Could not delete role used in IAM v2 policy")
			}

			err = MustJSONDecodeSuccess(
				tstCtx.DoLBRequest("/apis/iam/v2beta/policies/"+loaded.PolicyID,
					lbrequest.WithMethod("DELETE")),
			).WithValue(&empty{})
			return errors.Wrap(err, "Could not delete IAM v2 policy")
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(CreateIAMV2Diagnostic())
}
