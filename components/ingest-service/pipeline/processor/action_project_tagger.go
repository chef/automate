package processor

import (
	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
)

// BuildActionProjectTagger - Build a project tagger for Chef Actions
func BuildActionProjectTagger(authzClient authz.ProjectsServiceClient) message.ChefActionPipe {
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
	authzClient authz.ProjectsServiceClient) <-chan message.ChefAction {
	out := make(chan message.ChefAction, 100)
	go func() {
		nextNumToDrop := 1
		bundleSize := 0
		var projectRulesCollection map[string]*authz.ProjectRules
		for msg := range in {
			if msg.Ctx.Err() != nil {
				msg.FinishProcessing(msg.Ctx.Err())
				continue
			}
			if bundleSize <= 0 {
				nextBundleSize := len(in)
				log.WithFields(log.Fields{
					"message_id": msg.ID,
					"bundleSize": nextBundleSize,
				}).Debug("BundleProjectTagging - Update Project rules")
				var err error
				projectRulesCollection, err = getProjectRulesFromAuthz(authzClient)
				if err != nil {
					msg.FinishProcessing(err)
					dropChefActionMessages(in, err, nextNumToDrop-1)
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

			msg.InternalChefAction.Projects = findMatchingProjectsForAction(msg.Action, projectRulesCollection)

			message.PropagateChefAction(out, &msg)
		}
		close(out)
	}()

	return out
}

func dropChefActionMessages(in <-chan message.ChefAction, err error, numToDrop int) {
	var numDropped int
	err = errors.Wrap(err, "bulk dropping message")
	for numDropped = 0; numDropped < numToDrop; numDropped++ {
		select {
		case m := <-in:
			m.FinishProcessing(err)
		default:
			break
		}
	}
	logrus.Warnf("Dropped %d messages", numDropped)
}

func findMatchingProjectsForAction(action *chef.Action, projects map[string]*authz.ProjectRules) []string {
	matchingProjects := make([]string, 0)

	for projectName, project := range projects {
		if actionMatchesRules(action, project.Rules) {
			matchingProjects = append(matchingProjects, projectName)
		}
	}

	return matchingProjects
}

// Only one rule has to be true for the project to match (ORed together).
func actionMatchesRules(action *chef.Action, rules []*authz.ProjectRule) bool {
	for _, rule := range rules {
		if rule.Type == authz.ProjectRuleTypes_EVENT && actionMatchesAllConditions(action, rule.Conditions) {
			return true
		}
	}

	return false
}

// All the conditions must be true for a rule to be true (ANDed together).
// If there are no conditions then the rule is false
func actionMatchesAllConditions(action *chef.Action, conditions []*authz.Condition) bool {
	if len(conditions) == 0 {
		return false
	}

	for _, condition := range conditions {
		switch condition.Attribute {
		case authz.ProjectRuleConditionAttributes_CHEF_SERVER:
			if !stringutils.SliceContains(condition.Values, action.ServiceHostname) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
			if !stringutils.SliceContains(condition.Values, action.OrganizationName) {
				return false
			}
		default:
			return false
		}
	}

	return true
}
