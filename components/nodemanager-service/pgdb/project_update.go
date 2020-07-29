package pgdb

import (
	"context"
	"io"

	"encoding/json"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/stringutils"
	"github.com/go-gorp/gorp"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type NodeProjectData struct {
	ID                   string `db:"id"`
	Data                 string `db:"projects_data"`
	ProjectDataKeyValues []*nodes.ProjectsData
}

// JobCancel - cancel the a currently running project update
func (db *DB) JobCancel(ctx context.Context, jobID string) error {
	return errors.New("Unimplemented")
}

// UpdateProjectTags - start a project update
func (db *DB) UpdateProjectTags(ctx context.Context,
	projectRules map[string]*authz.ProjectRules) ([]string, error) {
	return nil, errors.New("Unimplemented")
}

// JobStatus - get the job status of the project update
func (db *DB) JobStatus(ctx context.Context, jobID string) (project_update_lib.JobStatus, error) {
	return project_update_lib.JobStatus{}, errors.New("Unimplemented")
}

func (db *DB) updateNodes(nodes []*NodeProjectData, projectRules map[string]*authz.ProjectRules) error {
	allProjectIDs := collectProjectIDs(projectRules)
	err := Transact(db, func(tx *DBTrans) error {
		err := tx.ensureProjects(allProjectIDs)
		if err != nil {
			return err
		}
		return nil
	})
	if err != nil {
		return err
	}
	nodeUpdates := make([]nodeUpdate, len(nodes))
	for i, node := range nodes {
		matchingProjectIDs := getMatchingProjectIDs(node, projectRules)
		nodeUpdates[i] = nodeUpdate{
			nodeID:     node.ID,
			projectIDs: matchingProjectIDs,
		}
	}

	return db.updateNodeProjectIDs(nodeUpdates)
}

// updateNodeProjectIDs - update the nodes project IDs
func (db *DB) updateNodeProjectIDs(nodeUpdates []nodeUpdate) error {
	return Transact(db, func(tx *DBTrans) error {
		err := tx.bulkUpdateNodeProjects(nodeUpdates)
		if err != nil {
			return errors.Wrap(err, "updateNodeProjectIDs unable to add projects to node")
		}

		return nil
	})
}

func getMatchingProjectIDs(node *NodeProjectData, projectRules map[string]*authz.ProjectRules) []string {
	matchingProjects := make([]string, 0)

	for projectName, project := range projectRules {
		if nodeMatchesRules(node, project.Rules) {
			matchingProjects = append(matchingProjects, projectName)
		}
	}

	return matchingProjects
}

func collectProjectIDs(projectRules map[string]*authz.ProjectRules) []string {
	projects := make([]string, 0, len(projectRules))

	for projectName := range projectRules {
		projects = append(projects, projectName)
	}

	return projects
}

// Only one rule has to be true for the project to match (ORed together).
func nodeMatchesRules(node *NodeProjectData, rules []*authz.ProjectRule) bool {
	for _, rule := range rules {
		if rule.Type == authz.ProjectRuleTypes_NODE && nodeMatchesAllConditions(node, rule.Conditions) {
			return true
		}
	}

	return false
}

// All the conditions must be true for a rule to be true (ANDed together).
// If there are no conditions then the rule is false
func nodeMatchesAllConditions(node *NodeProjectData, conditions []*authz.Condition) bool {
	if len(conditions) == 0 {
		return false
	}

	for _, condition := range conditions {
		switch condition.Attribute {
		case authz.ProjectRuleConditionAttributes_CHEF_SERVER:
			values := node.getValues("chef_server")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION:
			values := node.getValues("organization_name")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_ENVIRONMENT:
			values := node.getValues("environment")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP:
			values := node.getValues("policy_group")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME:
			values := node.getValues("policy_name")

			if len(values) == 0 || !stringutils.SliceContains(condition.Values, values[0]) {
				return false
			}
		case authz.ProjectRuleConditionAttributes_CHEF_ROLE:
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
		case authz.ProjectRuleConditionAttributes_CHEF_TAG:
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

const selectNodesProjectDataRange = `
SELECT
  n.id,
  n.projects_data
FROM nodes n
WHERE
  n.id > $1 AND
  n.id < $2
ORDER BY id asc
LIMIT $3;
`

const countNodesProjectData = `
SELECT
	count(*)
FROM nodes
`

const defaultLimit = 1000

type nodeDataIterator struct {
	rangeStart string
	rangeEnd   string
	eof        bool
	db         *gorp.DbMap
}

func (iter *nodeDataIterator) Next() ([]*NodeProjectData, error) {
	var nodesDaos []*NodeProjectData
	if iter.eof {
		return nil, io.EOF
	}

	logrus.WithFields(logrus.Fields{
		"start": iter.rangeStart,
		"end":   iter.rangeEnd,
	}).Debug("Processing nodes for project update")
	_, err := iter.db.Select(&nodesDaos, selectNodesProjectDataRange, iter.rangeStart, iter.rangeEnd, defaultLimit)

	if len(nodesDaos) == 0 {
		iter.eof = true
		return nil, io.EOF
	}

	for _, node := range nodesDaos {
		dbProjectsData := []*nodes.ProjectsData{}
		err := json.Unmarshal([]byte(node.Data), &dbProjectsData)
		if err != nil {
			return nodesDaos, errors.Wrap(err, "getNodes unable to unmarshal projects data")
		}

		node.ProjectDataKeyValues = dbProjectsData
	}

	iter.rangeStart = nodesDaos[len(nodesDaos)-1].ID

	return nodesDaos, err
}

var uuidChars = []byte{
	'0', '1', '2', '3',
	'4', '5', '6', '7',
	'8', '9', 'a', 'b',
	'c', 'd', 'e', 'f',
}

func uuidPrefixRanges16() []string {
	prefixes := make([]string, 16)
	for i := 0; i < 16; i++ {
		prefix := make([]byte, 1)
		prefix[0] = uuidChars[i]
		prefixes[i] = string(uuidChars[i])
	}
	return prefixes
}

func (db *DB) ListProjectUpdateTasks(ctx context.Context) ([]project_update_lib.SerializedProjectUpdateTask, error) {

	var count int
	row := db.Db.QueryRow(countNodesProjectData)
	if err := row.Scan(&count); err != nil {
		return nil, err
	}

	if count <= 50000 {
		tasks := make([]project_update_lib.SerializedProjectUpdateTask, 1)
		tasks[0] = project_update_lib.SerializedProjectUpdateTask{
			Priority: 1 << 62,
			Params: map[string]string{
				"s": "0",
				"e": "g",
			},
		}
		return tasks, nil
	}

	prefixes := uuidPrefixRanges16()
	logrus.Debugf("Found %d nodes, splitting into %d chunks", count, len(prefixes))
	tasks := make([]project_update_lib.SerializedProjectUpdateTask, len(prefixes))
	for i := 0; i < len(prefixes); i++ {
		start := prefixes[i]
		end := ""
		if i+1 < len(prefixes) {
			end = prefixes[i+1]
		} else {
			end = "g"
		}

		tasks[i] = project_update_lib.SerializedProjectUpdateTask{
			Priority: 1 << 62,
			Params: map[string]string{
				"s": start,
				"e": end,
			},
		}
	}
	return tasks, nil

}

func (db *DB) RunProjectUpdateTask(ctx context.Context, projectUpdateID string,
	params map[string]string, projectTaggingRules map[string]*authz.ProjectRules) (
	project_update_lib.SerializedProjectUpdateTaskID,
	project_update_lib.SerializedProjectUpdateTaskStatus,
	error) {

	start := params["s"]
	end := params["e"]

	logrus.WithFields(logrus.Fields{
		"start": start,
		"end":   end,
	}).Info("Running project update task for nodemanager")

	if start == "" || end == "" {
		return "", project_update_lib.NoStatus, errors.New("Invalid params")
	}

	iter := nodeDataIterator{
		rangeStart: start,
		rangeEnd:   end,
		db:         db.DbMap,
	}

	for {
		nodes, err := iter.Next()
		if err != nil {
			if err == io.EOF {
				logrus.WithFields(logrus.Fields{
					"start": start,
					"end":   end,
				}).Info("project update task complete for nodemanager")
				return "", project_update_lib.SerializedProjectUpdateTaskStatus{
					State:              project_update_lib.SerializedProjectUpdateTaskSuccess,
					PercentageComplete: 1.0,
				}, nil

			}
			return "", project_update_lib.NoStatus, err
		}

		if err := db.updateNodes(nodes, projectTaggingRules); err != nil {
			return "", project_update_lib.NoStatus, err
		}
	}

}

func (db *DB) MonitorProjectUpdateTask(ctx context.Context,
	projectUpdateID string, id project_update_lib.SerializedProjectUpdateTaskID) (
	project_update_lib.SerializedProjectUpdateTaskStatus, error) {

	return project_update_lib.NoStatus, errors.New("Unimplemented")

}

func (db *DB) CancelProjectUpdateTask(ctx context.Context, projectUpdateID string,
	id project_update_lib.SerializedProjectUpdateTaskID) error {
	return errors.New("Unimplemented")
}
