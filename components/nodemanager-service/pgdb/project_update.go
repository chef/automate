package pgdb

import (
	"context"

	"encoding/json"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/stringutils"
	uuid "github.com/chef/automate/lib/uuid4"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

const selectNodesProjectData = `
SELECT
  n.id,
  n.projects_data
FROM nodes n
`

type NodeProjectData struct {
	ID                   string `db:"id"`
	Data                 string `db:"projects_data"`
	ProjectDataKeyValues []*nodes.ProjectsData
}

// JobCancel - cancel the a currently running project update
func (db *DB) JobCancel(ctx context.Context, projectID string) error {
	log.Debugf("Node manager project update JobCancel %s", projectID)
	if db.ProjectUpdate.ID == projectID {
		db.ProjectUpdate.running = false
	}
	return nil
}

// UpdateProjectTags - start a project update
func (db *DB) UpdateProjectTags(ctx context.Context,
	projectRules map[string]*iam_v2.ProjectRules) ([]string, error) {
	jobUUID, _ := uuid.NewV4()
	log.Debugf("Running node manager UpdateProjectTags %v ID: %s", projectRules, jobUUID.String())

	// setup for new project update run
	db.ProjectUpdate.jobStatusError = nil
	db.ProjectUpdate.running = true
	db.ProjectUpdate.jobStatus = project_update_lib.JobStatus{
		Completed:             false,
		PercentageComplete:    0,
		EstimatedEndTimeInSec: 0,
	}
	db.ProjectUpdate.ID = jobUUID.String()
	go db.updateNodes(projectRules)

	return []string{jobUUID.String()}, nil
}

// JobStatus - get the job status of the project update
func (db *DB) JobStatus(ctx context.Context, jobID string) (project_update_lib.JobStatus, error) {
	if db.ProjectUpdate.ID == jobID {
		if db.ProjectUpdate.jobStatusError != nil {
			return project_update_lib.JobStatus{}, db.ProjectUpdate.jobStatusError
		}
		return db.ProjectUpdate.jobStatus, nil
	}

	// If the jobID does not match the currently running job then it must have completed before.
	return project_update_lib.JobStatus{
		Completed:             true,
		PercentageComplete:    1,
		EstimatedEndTimeInSec: 0,
	}, nil
}

func (db *DB) updateNodes(projectRules map[string]*iam_v2.ProjectRules) {
	nodes, err := db.getAllNodes()
	if err != nil {
		db.projectUpdateFail(err)
		return
	}

	numberOfNodes := float32(len(nodes))
	for index, node := range nodes {
		if !db.ProjectUpdate.running {
			db.projectUpdateComplete()
			return
		}
		matchingProjectIDs := getMatchingProjectIDs(node, projectRules)

		err = db.updateNodeProjectIDs(node, matchingProjectIDs)
		if err != nil {
			db.projectUpdateFail(err)
			return
		}

		db.updateProjectStatus(float32((index + 1)) / numberOfNodes)
	}

	db.projectUpdateComplete()
}

func (db *DB) updateProjectStatus(percentageComplete float32) {
	db.ProjectUpdate.jobStatus = project_update_lib.JobStatus{
		Completed:             false,
		PercentageComplete:    percentageComplete,
		EstimatedEndTimeInSec: 0,
	}
}

func (db *DB) projectUpdateComplete() {
	db.ProjectUpdate.jobStatus = project_update_lib.JobStatus{
		Completed:             true,
		PercentageComplete:    1.0,
		EstimatedEndTimeInSec: 0,
	}
	db.ProjectUpdate.running = false
}

func (db *DB) projectUpdateFail(err error) {
	db.ProjectUpdate.jobStatusError = err
	db.ProjectUpdate.jobStatus = project_update_lib.JobStatus{
		Completed:             true,
		PercentageComplete:    1.0,
		EstimatedEndTimeInSec: 0,
	}
	db.ProjectUpdate.running = false
}

// updateNodeProjectIDs - update the nodes project IDs
func (db *DB) updateNodeProjectIDs(node *NodeProjectData, matchingProjectIDs []string) error {
	return Transact(db, func(tx *DBTrans) error {
		err := tx.updateNodeProjects(node.ID, matchingProjectIDs)
		if err != nil {
			return errors.Wrap(err, "updateNodeProjectIDs unable to add projects to node")
		}

		return nil
	})
}

func (db *DB) getAllNodes() ([]*NodeProjectData, error) {
	var nodesDaos []*NodeProjectData
	_, err := db.Select(&nodesDaos, selectNodesProjectData)

	for _, node := range nodesDaos {
		dbProjectsData := []*nodes.ProjectsData{}
		err := json.Unmarshal([]byte(node.Data), &dbProjectsData)
		if err != nil {
			return nodesDaos, errors.Wrap(err, "getAllNodes unable to unmarshal projects data")
		}

		node.ProjectDataKeyValues = dbProjectsData
	}
	return nodesDaos, err
}

func getMatchingProjectIDs(node *NodeProjectData, projectRules map[string]*iam_v2.ProjectRules) []string {
	matchingProjects := make([]string, 0)

	for projectName, project := range projectRules {
		if nodeMatchesRules(node, project.Rules) {
			matchingProjects = append(matchingProjects, projectName)
		}
	}

	return matchingProjects
}

// Only one rule has to be true for the project to match (ORed together).
func nodeMatchesRules(node *NodeProjectData, rules []*iam_v2.ProjectRule) bool {
	for _, rule := range rules {
		if rule.Type == iam_v2.ProjectRuleTypes_NODE && nodeMatchesAllConditions(node, rule.Conditions) {
			return true
		}
	}

	return false
}

// All the conditions must be true for a rule to be true (ANDed together).
// If there are no conditions then the rule is false
func nodeMatchesAllConditions(node *NodeProjectData, conditions []*iam_v2.Condition) bool {
	if len(conditions) == 0 {
		return false
	}

	for _, condition := range conditions {
		switch condition.Attribute {
		case iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER:
			values := node.getValues("chef_server")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
			values := node.getValues("organization_name")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_ENVIRONMENT:
			values := node.getValues("environment")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP:
			values := node.getValues("policy_group")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_POLICY_NAME:
			values := node.getValues("policy_name")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE:
			foundMatch := false
			values := node.getValues("roles")
			if len(values) == 0 {
				return false
			}
			for _, projectRole := range condition.Values {
				if stringutils.SliceContains(values, projectRole) {
					foundMatch = true
					break
				}
			}
			if !foundMatch {
				return false
			}
		case iam_v2.ProjectRuleConditionAttributes_CHEF_TAG:
			foundMatch := false
			values := node.getValues("chef_tags")
			if len(values) == 0 {
				return false
			}
			for _, projectRole := range condition.Values {
				if stringutils.SliceContains(values, projectRole) {
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

func (node *NodeProjectData) getValues(valuesType string) []string {

	for _, keyValues := range node.ProjectDataKeyValues {
		if keyValues.Key == valuesType {
			return keyValues.Values
		}
	}

	return []string{}
}
