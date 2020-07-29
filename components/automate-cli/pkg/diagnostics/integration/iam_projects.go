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
	"name":"{{ .ID }} test project",
	"skip_policies":true
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

type generatedProjectData struct {
	ID      string `json:"id"`
	RuleID  string `json:"rule_id"`
	Skipped bool   `json:"skipped"`
}

// CreateProjectWithRule creates a project using the given id with an associated rule
func CreateProjectWithRule(tstCtx diagnostics.TestContext, id string) (*ProjectInfo, error) {
	projectInfo := ProjectInfo{}
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
				ID:        fmt.Sprintf("%s-rule-1", id),
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

// DeleteProjectWithRule deletes the project with the given id and its associated rule
func DeleteProjectWithRule(tstCtx diagnostics.TestContext, projectID, ruleID string) error {
	if err := MustJSONDecodeSuccess(tstCtx.DoLBRequest(
		fmt.Sprintf("/apis/iam/v2/projects/%s/rules/%s", projectID, ruleID),
		lbrequest.WithMethod("DELETE"),
	)).WithValue(&struct{}{}); err != nil {
		return errors.Wrap(err, "Could not delete rule")
	}

	if err := MustJSONDecodeSuccess(tstCtx.DoLBRequest(
		fmt.Sprintf("/apis/iam/v2/projects/%s", projectID),
		lbrequest.WithMethod("DELETE"),
	)).WithValue(&struct{}{}); err != nil {
		return errors.Wrap(err, "Could not delete project")
	}
	return nil
}

// CreateIAMProjectsDiagnostic creates the diagnostic struct for iam projects
func CreateIAMProjectsDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "iam-projects",
		Tags: diagnostics.Tags{"iam"},
		Skip: func(tstCtx diagnostics.TestContext) (bool, string, error) {
			isV2, err := tstCtx.IsIAMV2()
			if err != nil {
				return false, "", err
			}

			// the Skip function is run before each step: Generate, Verify, and Cleanup
			loaded := generatedProjectData{}
			err = tstCtx.GetValue("iam-projects", &loaded)
			if err != nil {
				// this is the first time running this diagnostic,
				// so we save whether or not we're skipping it
				tstCtx.SetValue("iam-projects", &generatedProjectData{
					Skipped: !isV2,
				})
			} else {
				// the diagnostic has been run before, so we keep track of its saved values
				tstCtx.SetValue("iam-projects", &generatedProjectData{
					ID:      loaded.ID,
					RuleID:  loaded.RuleID,
					Skipped: loaded.Skipped,
				})
			}

			return !isV2, "requires IAM v2", nil
		},
		Generate: func(tstCtx diagnostics.TestContext) error {
			projectInfo, err := CreateProjectWithRule(tstCtx,
				fmt.Sprintf("iam-projects-%s", TimestampName()))
			if err != nil {
				return err
			}

			loaded := generatedProjectData{}
			err = tstCtx.GetValue("iam-projects", &loaded)
			if err != nil {
				return errors.Wrap(err, "could not find generated context")
			}

			tstCtx.SetValue("iam-projects", &generatedProjectData{
				ID:      projectInfo.Project.ID,
				RuleID:  projectInfo.Project.Rule.ID,
				Skipped: loaded.Skipped,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := generatedProjectData{}
			err := tstCtx.GetValue("iam-projects", &loaded)
			require.NoError(tstCtx, err, "Could not find generated context")
			if loaded.Skipped {
				// This happens in the v1->v2 force upgrade scenario:
				// when we run "Generate diagnostic data" while on v1,
				// then force-upgrade to v2,
				// then run Verify and Cleanup on that data.
				// Since the Generate step was skipped,
				// there is nothing to verify on v2.
				return
			}

			projectInfo, err := GetProject(tstCtx, loaded.ID)
			require.NoError(tstCtx, err)

			assert.Equal(tstCtx, loaded.ID, projectInfo.Project.ID)
			assert.Equal(tstCtx, loaded.RuleID, projectInfo.Project.Rule.ID)
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := generatedProjectData{}
			err := tstCtx.GetValue("iam-projects", &loaded)
			if loaded.Skipped {
				// if diagnostic was run on v1, generating projects was skipped
				// so nothing to clean up here
				return nil
			}
			if err != nil {
				return errors.Wrap(err, "Could not find generated context")
			}

			return DeleteProjectWithRule(tstCtx, loaded.ID, loaded.RuleID)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateIAMProjectsDiagnostic(),
	)
}
