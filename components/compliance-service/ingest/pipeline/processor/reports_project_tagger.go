package processor

import (
	"context"
	"time"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const maxDropOnError = 128

// BundleReportProjectTagger - Build a project tagger processor for InSpec reports
func BundleReportProjectTagger(authzClient authz.ProjectsServiceClient) message.CompliancePipe {
	return func(in <-chan message.Compliance) <-chan message.Compliance {
		return reportProjectTagger(in, authzClient)
	}
}

// This processor is bundling all the messages that are currently in the 'in' channel. The bundling of messages
// decreases the number of times the authz-service is called for project rules. The way it works is when a message
// comes in, we make a call to the authz-service for the rules. We use these rules for all the messages that are
// currently in the queue. The 'bundleSize' is the number of messages that can use the current project rules from authz.
// When the bundleSize zero or less we need to refetch the project rules.
func reportProjectTagger(in <-chan message.Compliance, authzClient authz.ProjectsServiceClient) <-chan message.Compliance {
	if authzClient == nil {
		logrus.Error("no authz client found for project tagging; skipping project tagging")
		return in
	}
	out := make(chan message.Compliance, 100)
	go func() {
		nextNumToDrop := 1
		bundleSize := 0
		var projectRulesCollection map[string]*authz.ProjectRules
		for msg := range in {
			if msg.Ctx.Err() != nil {
				msg.FinishProcessingCompliance(msg.Ctx.Err())
				continue
			}
			if bundleSize <= 0 {
				nextBundleSize := len(in)
				logrus.WithFields(logrus.Fields{
					"message_id": msg.Report.ReportUuid,
					"bundleSize": nextBundleSize,
				}).Debug("BundleProjectTagging - Update Project rules")
				var err error
				projectRulesCollection, err = getProjectRulesFromAuthz(authzClient)
				if err != nil {
					msg.FinishProcessingCompliance(err)
					dropComplianceMessages(in, err, nextNumToDrop-1)
					nextNumToDrop *= 2
					if nextNumToDrop > maxDropOnError {
						nextNumToDrop = maxDropOnError
					}
					continue
				}
				bundleSize = nextBundleSize
				nextNumToDrop = 1
			} else {
				// Skip
				bundleSize--
			}

			projectTags := findMatchingProjects(msg.InspecReport, projectRulesCollection)

			msg.InspecReport.Projects = projectTags
			msg.InspecSummary.Projects = projectTags

			message.Propagate(out, &msg)
		}
		close(out)
	}()

	return out
}

func dropComplianceMessages(in <-chan message.Compliance, err error, numToDrop int) {
	var numDropped int
	err = errors.Wrap(err, "bulk dropping message")
	for numDropped = 0; numDropped < numToDrop; numDropped++ {
		select {
		case m := <-in:
			m.FinishProcessingCompliance(err)
		default:
			break
		}
	}
	logrus.Warnf("Dropped %d chef run messages", numDropped)
}

func getProjectRulesFromAuthz(authzClient authz.ProjectsServiceClient) (map[string]*authz.ProjectRules, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	projectsCollection, err := authzClient.ListRulesForAllProjects(ctx, &authz.ListRulesForAllProjectsReq{})
	if err != nil {
		logrus.WithError(err).Error("Could not fetch project rules from authz")
		return nil, errors.Wrap(err, "Could not fetch project rules from authz")
	}

	return projectsCollection.ProjectRules, nil
}
func findMatchingProjects(report *relaxting.ESInSpecReport, projects map[string]*authz.ProjectRules) []string {
	matchingProjects := make([]string, 0)

	for projectName, projectRules := range projects {
		if reportMatchesRules(report, projectRules.Rules) {
			matchingProjects = append(matchingProjects, projectName)
		}
	}

	return matchingProjects
}

// Only one rule has to match for the entire project to match (ORed together).
func reportMatchesRules(report *relaxting.ESInSpecReport, rules []*authz.ProjectRule) bool {
	for _, rule := range rules {
		if rule.Type == authz.ProjectRuleTypes_NODE && reportMatchesAllConditions(report, rule.Conditions) {
			return true
		}
	}

	return false
}

// All the conditions must be true for a rule to be true (ANDed together).
// If there are no conditions then the rule is false
func reportMatchesAllConditions(report *relaxting.ESInSpecReport, conditions []*authz.Condition) bool {
	if len(conditions) == 0 {
		return false
	}

	for _, condition := range conditions {
		switch condition.Attribute {
		case authz.ProjectRuleConditionAttributes_ENVIRONMENT:
			if !stringutils.SliceContains(condition.Values, report.Environment) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_ROLE:
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
		case authz.ProjectRuleConditionAttributes_CHEF_SERVER:
			if !stringutils.SliceContains(condition.Values, report.SourceFQDN) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
			if !stringutils.SliceContains(condition.Values, report.OrganizationName) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP:
			if !stringutils.SliceContains(condition.Values, report.PolicyGroup) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME:
			if !stringutils.SliceContains(condition.Values, report.PolicyName) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_TAG:
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
