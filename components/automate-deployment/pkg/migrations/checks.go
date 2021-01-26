package migrations

import (
	"github.com/chef/automate/lib/stringutils"
)

// CheckWorkflowNotDeployed checks to make sure workflow is not deployed
func CheckWorkflowNotDeployed() Check {
	return Check{
		Name:        "workflow-not-deployed",
		Description: "Chef Workflow has reached EOL and will no longer be deployed by Automate",
		Run: func(t TestProbe) (CheckStatus, error) {
			if stringutils.SliceContains(t.DeployedProducts(), "workflow") {
				return CheckStatus{
					Success:     false,
					Message:     "Workflow is deployed",
					Remediation: "TODO: Add remediation steps",
				}, nil
			}
			return CheckStatus{
				Success: true,
				Message: "Workflow is not deployed",
			}, nil
		},
	}
}
