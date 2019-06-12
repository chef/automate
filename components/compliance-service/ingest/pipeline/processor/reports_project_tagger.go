package processor

import (
	"context"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/stringutils"
	"github.com/sirupsen/logrus"
)

// BundleReportProjectTagger - Build a project tagger processor for InSpec reports
func BundleReportProjectTagger(authzClient iam_v2.ProjectsClient) message.CompliancePipe {
	return func(in <-chan message.Compliance) <-chan message.Compliance {
		return reportProjectTagger(in, authzClient)
	}
}

// This processor is bundling all the messages that are currently in the 'in' channel. The bundling of messages
// decreases the number of times the authz-service is called for project rules. The way it works is when a message
// comes in, we make a call to the authz-service for the rules. We use these rules for all the messages that are
// currently in the queue. The 'bundleSize' is the number of messages that can use the current project rules from authz.
// When the bundleSize zero or less we need to refetch the project rules.
func reportProjectTagger(in <-chan message.Compliance, authzClient iam_v2.ProjectsClient) <-chan message.Compliance {
	out := make(chan message.Compliance, 100)
	go func() {
		bundleSize := 0
		var projectRulesCollection map[string]*iam_v2.ProjectRules
		for msg := range in {
			if isScanJob(msg) {
				bundleSize--
			} else {
				if bundleSize <= 0 {
					bundleSize = len(in)
					logrus.WithFields(logrus.Fields{
						"message_id": msg.Report.ReportUuid,
						"bundleSize": bundleSize,
					}).Debug("BundleProjectTagging - Update Project rules")
					projectRulesCollection = getProjectRulesFromAuthz(msg.Ctx, authzClient)
				} else {
					// Skip
					bundleSize--
				}

				projectTags := findMatchingProjects(msg.InspecReport, projectRulesCollection)

				msg.InspecReport.Projects = projectTags
				msg.InspecSummary.Projects = projectTags
			}

			out <- msg
		}
		close(out)
	}()

	return out
}

func isScanJob(msg message.Compliance) bool {
	return len(msg.InspecReport.JobID) > 0
}

func getProjectRulesFromAuthz(ctx context.Context, authzClient iam_v2.ProjectsClient) map[string]*iam_v2.ProjectRules {
	projectsCollection, err := authzClient.ListRulesForAllProjects(ctx, &iam_v2.ListRulesForAllProjectsReq{})

	if err != nil {
		// If there is an error getting the project rules from authz crash the service.
		logrus.WithError(err).Fatal("Could not fetch project rules from authz")
	}

	return projectsCollection.ProjectRules
}

func findMatchingProjects(report *relaxting.ESInSpecReport, projects map[string]*iam_v2.ProjectRules) []string {
	matchingProjects := make([]string, 0)

	for projectName, projectRules := range projects {
		if reportMatchesRules(report, projectRules.Rules) {
			matchingProjects = append(matchingProjects, projectName)
		}
	}

	return matchingProjects
}

// Only one rule has to match for the entire project to match (ORed together).
func reportMatchesRules(report *relaxting.ESInSpecReport, rules []*iam_v2.ProjectRule) bool {
	for _, rule := range rules {
		if rule.Type == iam_v2.ProjectRuleTypes_NODE && reportMatchesAllConditions(report, rule.Conditions) {
			return true
		}
	}

	return false
}

// All the conditions must be true for a rule to be true (ANDed together).
// If there are no conditions then the rule is false
func reportMatchesAllConditions(report *relaxting.ESInSpecReport, conditions []*iam_v2.Condition) bool {
	if len(conditions) == 0 {
		return false
	}

	for _, condition := range conditions {
		switch condition.Attribute {
		case iam_v2.ProjectRuleConditionAttributes_CHEF_ENVIRONMENTS:
			if !stringutils.SliceContains(condition.Values, report.Environment) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_ROLES:
			foundMatch := false
			for _, projectRole := range condition.Values {
				if stringutils.SliceContains(report.Roles, projectRole) {
					foundMatch = true
					break
				}
			}
			if !foundMatch {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_SERVERS:
			if !stringutils.SliceContains(condition.Values, report.SourceFQDN) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_ORGS:
			if !stringutils.SliceContains(condition.Values, report.OrganizationName) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_POLICY_GROUP:
			if !stringutils.SliceContains(condition.Values, report.PolicyGroup) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_POLICY_NAME:
			if !stringutils.SliceContains(condition.Values, report.PolicyName) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_TAGS:
			foundMatch := false
			for _, projectChefTag := range condition.Values {
				if stringutils.SliceContains(report.ChefTags, projectChefTag) {
					foundMatch = true
					break
				}
			}
			if !foundMatch {
				return false
			}
		default:
			return false
		}
	}

	return true
}
