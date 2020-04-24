package processor

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/pipeline/message"
	"github.com/chef/automate/lib/stringutils"
)

const maxDropOnError = 128

// BuildRunProjectTagger - Build a project tagger for CCRs
func BuildRunProjectTagger(authzClient iam_v2.ProjectsClient) message.ChefRunPipe {
	return func(in <-chan message.ChefRun) <-chan message.ChefRun {
		return runBundleProjectTagger(in, authzClient)
	}
}

// This processor is bundling all the messages that are currently in the 'in' channel. The bundling
// of messages decreases the number of times the authz-service is called for project rules. The way
// it works is when a message comes in, we make a call to the authz-service for the rules. We use
// these rules for all the messages that are currently in the queue. The 'bundleSize' is the number
// of messages that can use the current project rules from authz.
func runBundleProjectTagger(in <-chan message.ChefRun,
	authzClient iam_v2.ProjectsClient) <-chan message.ChefRun {
	out := make(chan message.ChefRun, 100)
	go func() {
		nextNumToDrop := 1
		bundleSize := 0
		var projectRulesCollection map[string]*iam_v2.ProjectRules
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
					dropChefRunMessages(in, err, nextNumToDrop-1)
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

			msg.Node.Projects = findMatchingProjects(msg.Node, projectRulesCollection)

			out <- msg
		}
		close(out)
	}()

	return out
}

func dropChefRunMessages(in <-chan message.ChefRun, err error, numToDrop int) {
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
	logrus.Warnf("Dropped %d chef run messages", numDropped)
}

func findMatchingProjects(node backend.Node, projects map[string]*iam_v2.ProjectRules) []string {
	matchingProjects := make([]string, 0)

	for projectName, project := range projects {
		if nodeMatchesRules(node, project.Rules) {
			matchingProjects = append(matchingProjects, projectName)
		}
	}

	return matchingProjects
}

func getProjectRulesFromAuthz(authzClient iam_v2.ProjectsClient) (map[string]*iam_v2.ProjectRules, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	projectsCollection, err := authzClient.ListRulesForAllProjects(ctx, &iam_v2.ListRulesForAllProjectsReq{})
	if err != nil {
		log.WithError(err).Error("Could not fetch project rules from authz")
		return nil, errors.Wrap(err, "Could not fetch project rules from authz")
	}

	return projectsCollection.ProjectRules, nil
}

// Only one rule has to be true for the project to match (ORed together).
func nodeMatchesRules(node backend.Node, rules []*iam_v2.ProjectRule) bool {
	for _, rule := range rules {
		if rule.Type == iam_v2.ProjectRuleTypes_NODE && nodeMatchesAllConditions(node, rule.Conditions) {
			return true
		}
	}

	return false
}

// All the conditions must be true for a rule to be true (ANDed together).
// If there are no conditions then the rule is false
func nodeMatchesAllConditions(node backend.Node, conditions []*iam_v2.Condition) bool {
	if len(conditions) == 0 {
		return false
	}

	for _, condition := range conditions {
		switch condition.Attribute {
		case iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER:
			if !stringutils.SliceContains(condition.Values, node.SourceFqdn) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
			if !stringutils.SliceContains(condition.Values, node.OrganizationName) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT:
			if !stringutils.SliceContains(condition.Values, node.Environment) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP:
			if !stringutils.SliceContains(condition.Values, node.PolicyGroup) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME:
			if !stringutils.SliceContains(condition.Values, node.PolicyName) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE:
			foundMatch := false
			for _, projectRole := range condition.Values {
				if stringutils.SliceContains(node.Roles, projectRole) {
					foundMatch = true
					break
				}
			}
			if !foundMatch {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_TAG:
			foundMatch := false
			for _, projectRole := range condition.Values {
				if stringutils.SliceContains(node.ChefTags, projectRole) {
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
