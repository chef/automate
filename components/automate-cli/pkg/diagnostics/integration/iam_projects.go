package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createProjectTemplate = `
{
	"id":"{{ .ID }}",
	"name":"{{ .ID }} test project"
}
`

const createRuleTemplate = `
{
	"id":"{{ .ID }}",
	"name":"{{ .ID }} test rule",
	"type": "NODE",
	"project_id": "{{ .ProjectID }}",
	"conditions": [
		{
			"attribute": "CHEF_SERVER",
			"operator": "EQUALS",
			"values": ["testing"]
		}
	]
}
`

// ProjectInfo represents the project parameters
type ProjectInfo struct {
	Project struct {
		ID   string `json:"id"`
		Name string `json:"name"`
		Rule Rule
	}
}

// Rule represents the rule parameters
type Rule struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

// RuleInfo represents the nested rule response
type RuleInfo struct {
	Rule Rule
}

// RulesInfo represents the nested rules response
type RulesInfo struct {
	Rules []Rule
}

type iamProjectSave struct {
	ID     string `json:"id"`
	RuleID string `json:"rule_id"`
}

// CreateRandomProjectWithRule creates a project with an associated rule
func CreateRandomProjectWithRule(tstCtx diagnostics.TestContext) (*ProjectInfo, error) {
	projectInfo := ProjectInfo{}
	id := TimestampName()
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/apis/iam/v2/projects",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createProjectTemplate, struct {
				ID string
			}{
				ID: id,
			}),
		)).WithValue(&projectInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not create project")
	}

	ruleResp := RuleInfo{}
	err = MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/projects/%s/rules", id),
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createRuleTemplate, struct {
				ID        string
				ProjectID string
			}{
				ID:        fmt.Sprintf("rule-%s", TimestampName()),
				ProjectID: projectInfo.Project.ID,
			}),
		)).WithValue(&ruleResp)

	if err != nil {
		return nil, errors.Wrap(err, "Could not create rule")
	}
	projectInfo.Project.Rule = ruleResp.Rule

	return &projectInfo, nil
}

// GetProject fetches the given project and its associated rule
func GetProject(tstCtx diagnostics.TestContext, id string) (*ProjectInfo, error) {
	projectInfo := ProjectInfo{}
	err := MustJSONDecodeSuccess(tstCtx.DoLBRequest(
		fmt.Sprintf("/apis/iam/v2/projects/%s", id),
	)).WithValue(&projectInfo)
	if err != nil {
		return nil, errors.Wrap(err, "Could not fetch project")
	}

	rulesInfo := RulesInfo{}
	err = MustJSONDecodeSuccess(tstCtx.DoLBRequest(
		fmt.Sprintf("/apis/iam/v2/projects/%s/rules", id),
	)).WithValue(&rulesInfo)
	if err != nil {
		return nil, errors.Wrap(err, "Could not fetch rules")
	}
	// there's only one created rule
	projectInfo.Project.Rule = rulesInfo.Rules[0]

	return &projectInfo, nil
}

// DeleteProject deletes the project with the given id
func DeleteProject(tstCtx diagnostics.TestContext, id string) error {
	// any associated rules are cascaded deleted with the project
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/projects/%s", id),
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not delete project")
	}
	return nil
}

// CreateIAMProjectsDiagnostic create the diagnostic struct for iam projects
func CreateIAMProjectsDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "iam-projects",
		Tags: diagnostics.Tags{"iam"},
		Skip: func(tstCtx diagnostics.TestContext) (bool, string, error) {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return false, "", err
			}
			return !isV2, "requires IAM v2", nil
		},
		Generate: func(tstCtx diagnostics.TestContext) error {
			projectInfo, err := CreateRandomProjectWithRule(tstCtx)
			if err != nil {
				return err
			}

			tstCtx.SetValue("iam-projects", &iamProjectSave{
				ID:     projectInfo.Project.ID,
				RuleID: projectInfo.Project.Rule.ID,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := iamProjectSave{}
			err := tstCtx.GetValue("iam-projects", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")

			projectInfo, err := GetProject(tstCtx, loaded.ID)
			require.NoError(tstCtx, err)

			assert.Equal(tstCtx, loaded.ID, projectInfo.Project.ID)
			assert.Equal(tstCtx, loaded.RuleID, projectInfo.Project.Rule.ID)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := iamProjectSave{}
			err := tstCtx.GetValue("iam-projects", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}

			return DeleteProject(tstCtx, loaded.ID)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateIAMProjectsDiagnostic(),
	)
}
