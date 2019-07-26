package authz

import (
	"fmt"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
)

const (
	// ProjectUpdateIDTag - The tag used to designate the project update job
	ProjectUpdateIDTag = "ProjectUpdateID"
)

// JobStatus - the current status of a job
type JobStatus struct {
	Completed             bool
	PercentageComplete    float32
	EstimatedEndTimeInSec int64
}

// FindSlowestJobStatus - merge multiple job statuses into one.
// Return the status that is going to finish last.
func FindSlowestJobStatus(jobStatuses []JobStatus) JobStatus {
	combinedJobStatus := JobStatus{
		Completed:          true,
		PercentageComplete: 1.0,
	}
	for _, jobStatus := range jobStatuses {
		if !jobStatus.Completed {
			combinedJobStatus.Completed = false
			if jobStatus.EstimatedEndTimeInSec > combinedJobStatus.EstimatedEndTimeInSec {
				combinedJobStatus.EstimatedEndTimeInSec = jobStatus.EstimatedEndTimeInSec
				combinedJobStatus.PercentageComplete = jobStatus.PercentageComplete
			}
		}
	}

	return combinedJobStatus
}

// GetProjectUpdateID - get the project update ID from the event message
func GetProjectUpdateID(event *automate_event.EventMsg) (string, error) {
	if event.Data != nil && event.Data.Fields != nil &&
		event.Data.Fields["ProjectUpdateID"] != nil &&
		event.Data.Fields["ProjectUpdateID"].GetStringValue() != "" {
		return event.Data.Fields["ProjectUpdateID"].GetStringValue(), nil
	}

	return "", fmt.Errorf("Event Msg does not have a ProjectUpdateID eventID: %q",
		event.EventID)
}

func ConvertProjectTaggingRulesToEsParams(projectTaggingRules map[string]*iam_v2.ProjectRules) map[string]interface{} {
	esProjectCollection := make([]map[string]interface{}, len(projectTaggingRules))
	projectIndex := 0
	for projectName, projectRules := range projectTaggingRules {
		esRuleCollection := make([]map[string]interface{}, len(projectRules.Rules))
		for ruleIndex, rule := range projectRules.Rules {
			esConditionCollection := make([]map[string]interface{}, len(rule.Conditions))

			for conditionIndex, condition := range rule.Conditions {
				chefServers := []string{}
				organizations := []string{}
				environments := []string{}
				roles := []string{}
				chefTags := []string{}
				policyGroups := []string{}
				policyNames := []string{}
				switch condition.Attribute {
				case iam_v2.ProjectRuleConditionAttributes_CHEF_SERVERS:
					chefServers = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS:
					organizations = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_CHEF_ENVIRONMENTS:
					environments = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_ROLES:
					roles = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_CHEF_TAGS:
					chefTags = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_POLICY_GROUP:
					policyGroups = condition.Values
				case iam_v2.ProjectRuleConditionAttributes_POLICY_NAME:
					policyNames = condition.Values
				}
				esConditionCollection[conditionIndex] = map[string]interface{}{
					"chefServers":   chefServers,
					"organizations": organizations,
					"environments":  environments,
					"roles":         roles,
					"chefTags":      chefTags,
					"policyGroups":  policyGroups,
					"policyNames":   policyNames,
				}

			}
			esRuleCollection[ruleIndex] = map[string]interface{}{
				"conditions": esConditionCollection,
			}
		}

		esProjectCollection[projectIndex] = map[string]interface{}{
			"name":  projectName,
			"rules": esRuleCollection,
		}
		projectIndex++
	}

	return map[string]interface{}{"projects": esProjectCollection}
}
