package processor

import (
	chef "github.com/chef/automate/api/external/ingest/request"
	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/lib/stringutils"
	log "github.com/sirupsen/logrus"
)

// BuildActionProjectTagger - Build a project tagger for Chef Actions
func BuildActionProjectTagger(authzClient iam_v2.ProjectsClient) message.ChefActionPipe {
	return func(in <-chan message.ChefAction) <-chan message.ChefAction {
		return actionBundleProjectTagger(in, authzClient)
	}
}

// This processor is bundling all the messages that are currently in the 'in' channel. The bundling
// of messages decreases the number of times the authz-service is called for project rules. The way
// it works is when a message comes in, we make a call to the authz-service for the rules. We use
// these rules for all the messages that are currently in the queue. The 'bundleSize' is the number
// of messages that can use the current project rules from authz.
func actionBundleProjectTagger(in <-chan message.ChefAction,
	authzClient iam_v2.ProjectsClient) <-chan message.ChefAction {
	out := make(chan message.ChefAction, 100)
	go func() {
		bundleSize := 0
		var projectRulesCollection map[string]*iam_v2.ProjectRules
		for msg := range in {
			if bundleSize <= 0 {
				bundleSize = len(in)
				log.WithFields(log.Fields{
					"message_id": msg.ID,
					"bundleSize": bundleSize,
				}).Debug("BundleProjectTagging - Update Project rules")
				projectRulesCollection = getProjectRulesFromAuthz(msg.Ctx, authzClient)
			} else {
				// Skip
				bundleSize--
			}

			msg.InternalChefAction.Projects = findMatchingProjectsForAction(msg.Action, projectRulesCollection)

			out <- msg
		}
		close(out)
	}()

	return out
}

func findMatchingProjectsForAction(action *chef.Action, projects map[string]*iam_v2.ProjectRules) []string {
	matchingProjects := make([]string, 0)

	for projectName, project := range projects {
		if actionMatchesRules(action, project.Rules) {
			matchingProjects = append(matchingProjects, projectName)
		}
	}

	return matchingProjects
}

// Only one rule has to be true for the project to match (ORed together).
func actionMatchesRules(action *chef.Action, rules []*iam_v2.ProjectRule) bool {
	for _, rule := range rules {
		if rule.Type == iam_v2.ProjectRuleTypes_EVENT && actionMatchesAllConditions(action, rule.Conditions) {
			return true
		}
	}

	return false
}

// All the conditions must be true for a rule to be true (ANDed together).
// If there are no conditions then the rule is false
func actionMatchesAllConditions(action *chef.Action, conditions []*iam_v2.Condition) bool {
	if len(conditions) == 0 {
		return false
	}

	for _, condition := range conditions {
		switch condition.Attribute {
		case iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER:
			if !stringutils.SliceContains(condition.Values, action.RemoteHostname) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
			if !stringutils.SliceContains(condition.Values, action.OrganizationName) {
				return false
			}
		default:
			return false
		}
	}

	return true
}
