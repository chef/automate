package pgdb

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"

	"sort"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/lib/stringutils"
)

const selectSecretIdsFromNodes = `
SELECT secret_id
FROM nodes_secrets
WHERE node_id=$1;
`

const selectNodes = `
SELECT
  n.connection_error,
  n.id,
  n.last_contact,
  n.last_job,
  n.manager,
  n.name,
  n.platform,
  n.platform_version,
  n.report_id,
  n.source_state,
  n.status,
  n.target_config,
  n.last_scan,
  n.last_run,
  n.projects_data,
  COALESCE(('[' || string_agg('{"key":"' || t.key || '"' || ',"value": "' || t.value || '"}', ',') || ']'), '[]') :: JSON AS tags,
  COALESCE(array_to_json(array_remove(array_agg(DISTINCT m.manager_id), NULL)), '[]') AS manager_ids,
  COALESCE(array_to_json(array_remove(array_agg(p.project_id), NULL)), '[]') AS projects,
  COUNT(*) OVER () AS total_count
FROM nodes n
  LEFT JOIN nodes_tags nt ON n.id = nt.node_id
  LEFT JOIN tags t ON t.id = nt.tag_id
  LEFT JOIN node_managers_nodes m on n.id = m.node_id
  LEFT JOIN nodes_projects np on np.node_id = n.id
  LEFT JOIN projects p on np.project_id = p.id
%s
GROUP BY n.id
%s
ORDER BY %s %s
LIMIT $1
OFFSET $2;
`

const getNodeCounts = `
SELECT
	COUNT(case n.status when 'unreachable' then 1 end) AS unreachable_count,
	COUNT(case n.status when 'reachable' then 1 end) AS reachable_count,
	COUNT(case n.status when 'unknown' then 1 end) AS unknown_count
FROM nodes n
`

const deleteNodesWithQuery = `
DELETE FROM nodes
USING nodes n
  LEFT JOIN nodes_tags nt ON n.id = nt.node_id
  LEFT JOIN tags t ON t.id = nt.tag_id
  LEFT JOIN node_managers_nodes m on n.id = m.node_id
%s
%s
AND nodes.id = n.id
RETURNING nodes.name
`

const deleteNodesById = `
DELETE FROM nodes
WHERE nodes.id = ANY($1)
RETURNING nodes.name;
`

const deleteNodeTags = `
DELETE FROM tags
WHERE id IN (SELECT tag_id
             FROM nodes_tags
             WHERE node_id = $1);
`

const deleteNodesSecretsByNodeId = `
DELETE
FROM nodes_secrets
WHERE node_id = $1
      AND secret_id = $2;

`

const updateNodeById = `
UPDATE nodes
SET name = $2, target_config = $3
WHERE id = $1;
`

const sqlNodeState = `
SELECT source_state FROM nodes WHERE id=$1;
`

const sqlUpdateNodeDetectFailed = `
UPDATE nodes
SET status = $1, last_job = $2, last_connection_attempt = $4
WHERE id=$3;
`
const sqlUpdateNodeDetectCompleted = `
UPDATE nodes
SET platform = $1, platform_version = $2, status = $3, last_contact = $4, last_connection_attempt = $4, last_job = $5
WHERE id=$6;
`
const sqlUpdateNodeExecCompleted = `
UPDATE nodes
SET status = $1, last_contact =$2, last_connection_attempt = $2, last_job = $3
WHERE id=$4;
`
const sqlUpdateNodeDetectCompletedWithState = `
UPDATE nodes
SET platform = $1, platform_version = $2, status = $3, last_contact = $4, last_connection_attempt = $4, last_job = $5,  source_state = $6
WHERE id=$7;
`
const sqlUpdateNodeExecCompletedWithState = `
UPDATE nodes
SET status = $1, last_contact =$2, last_connection_attempt = $2, last_job = $3, source_state = $4
WHERE id=$5;
`
const sqlUpdateNodeConnectionError = `
UPDATE nodes
SET connection_error = $2
WHERE id = $1;
`

// $1 and $2 not working for ORDER BY so ended up using %s + Sprintf, but this requires solid input validation to avoid SQL injection
// Is there a better way to do it?

var nodesSortFields = map[string]string{
	"name":             "LOWER(n.name)",
	"platform":         "LOWER(n.platform)",
	"platform_version": "LOWER(n.platform_version)",
	"status":           "LOWER(n.status)",
	"manager":          "LOWER(n.manager)",
	"last_contact":     "n.last_contact",
}

type dbNode struct {
	ConnectionError string           `db:"connection_error"`
	ID              string           `db:"id"`
	LastContact     time.Time        `db:"last_contact"`
	LastJob         string           `db:"last_job"`
	Manager         string           `db:"manager"`
	ManagerIDs      json.RawMessage  `db:"manager_ids"`
	Name            string           `db:"name"`
	Platform        string           `db:"platform"`
	PlatformVersion string           `db:"platform_version"`
	Projects        json.RawMessage  `db:"projects"`
	ReportID        string           `db:"report_id"`
	State           string           `db:"source_state"`
	Status          string           `db:"status"`
	Tags            json.RawMessage  `db:"tags"`
	TargetConfig    *json.RawMessage `db:"target_config"`
	TotalCount      int64            `db:"total_count"`
	LastScan        json.RawMessage  `db:"last_scan"`
	LastRun         json.RawMessage  `db:"last_run"`
	ProjectsData    json.RawMessage  `db:"projects_data"`
}

type nodeCounts struct {
	UnreachableCount int64 `db:"unreachable_count"`
	ReachableCount   int64 `db:"reachable_count"`
	UnknownCount     int64 `db:"unknown_count"`
}

func toDBNode(inNode *nodes.Node) (node, error) {
	var newNode node
	if inNode.TargetConfig == nil {
		inNode.TargetConfig = &nodes.TargetConfig{}
	}
	backend := inNode.TargetConfig.Backend
	port := inNode.TargetConfig.Port
	if (backend == "winrm" || backend == "ssh") && (port < 1 || port > 65535) {
		return newNode, &utils.InvalidError{Msg: "Invalid node. Port for node must be within range: 1-65535"}
	}
	newNode.ID = inNode.Id
	newNode.Name = inNode.Name
	newNode.Platform = inNode.Platform
	newNode.PlatformVersion = inNode.PlatformVersion
	newNode.Manager = inNode.Manager
	jsonTC, err := json.Marshal(inNode.TargetConfig)
	if err != nil {
		return newNode, errors.Wrap(err, "toDBNode error marshalling target_config")
	}
	newNode.TargetConfig = jsonTC
	jsonTags, err := json.Marshal(inNode.Tags)
	if err != nil {
		return newNode, errors.Wrap(err, "toDBNode error marshalling node tags")
	}
	newNode.Tags = jsonTags
	newNode.Secrets = inNode.TargetConfig.Secrets
	return newNode, nil
}

func (db *DB) fromDBNode(inNode *dbNode) (*nodes.Node, error) {
	newNode := nodes.Node{}
	newNode.ConnectionError = inNode.ConnectionError
	newNode.Id = inNode.ID
	newNode.LastContact, _ = ptypes.TimestampProto(inNode.LastContact)
	newNode.Manager = inNode.Manager
	newNode.Name = inNode.Name
	newNode.Platform = inNode.Platform
	newNode.PlatformVersion = inNode.PlatformVersion
	newNode.State = inNode.State
	newNode.Status = inNode.Status

	var manager_ids []string
	err := json.Unmarshal(inNode.ManagerIDs, &manager_ids)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBNode unable to unmarshal manager_ids")
	}
	newNode.ManagerIds = manager_ids

	var tags []*common.Kv
	err = json.Unmarshal(inNode.Tags, &tags)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBNode unable to unmarshal tags")
	}
	newNode.Tags = tags

	var projects []string
	err = json.Unmarshal(inNode.Projects, &projects)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBNode unable to unmarshal projects")
	}
	newNode.Projects = projects

	if inNode.LastScan != nil {
		dbScanData := lastContactData{}
		err = json.Unmarshal(inNode.LastScan, &dbScanData)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBNode unable to unmarshal scan data")
		}
		newNode.ScanData, err = translateToProtoStruct(dbScanData)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBNode unable to translate scan data")
		}
	}

	if inNode.LastRun != nil {
		dbRunData := lastContactData{}
		err = json.Unmarshal(inNode.LastRun, &dbRunData)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBNode unable to unmarshal run data")
		}
		newNode.RunData, err = translateToProtoStruct(dbRunData)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBNode unable to translate scan data")
		}
	}

	if inNode.ProjectsData != nil {
		dbProjectsData := []*nodes.ProjectsData{}
		err = json.Unmarshal(inNode.ProjectsData, &dbProjectsData)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBNode unable to unmarshal projects data")
		}
		newNode.ProjectsData = dbProjectsData
	}

	t := inNode.LastContact.Round(1 * time.Second)
	newNode.LastContact, err = ptypes.TimestampProto(t)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBNode error translating last_contact to timestamp")
	}

	if inNode.LastJob != "" {
		jobStatus := "completed"
		if inNode.Status == "unreachable" {
			jobStatus = "failed"
		}
		newNode.LastJob = &nodes.ResultsRow{
			NodeId:   inNode.ID,
			ReportId: inNode.ReportID,
			JobId:    inNode.LastJob,
			Status:   jobStatus,
		}
	}

	return &newNode, nil
}

func translateToProtoStruct(nodeData lastContactData) (*nodes.LastContactData, error) {
	lastContactData := &nodes.LastContactData{
		Id:                nodeData.ID,
		Status:            translateStatusToEnum(nodeData.Status),
		PenultimateStatus: translateStatusToEnum(nodeData.PenultimateStatus),
	}
	if len(nodeData.EndTime) > 0 {
		time, err := time.Parse(time.RFC3339, nodeData.EndTime)
		if err != nil {
			return &nodes.LastContactData{}, errors.Wrap(err, "unable to parse end time")
		}
		timestamp, err := ptypes.TimestampProto(time)
		if err != nil {
			return &nodes.LastContactData{}, errors.Wrap(err, "unable to parse end time")
		}
		lastContactData.EndTime = timestamp
	}
	return lastContactData, nil
}

func translateStatusToEnum(status string) nodes.LastContactData_Status {
	switch status {
	case "PASSED":
		return nodes.LastContactData_PASSED
	case "FAILED":
		return nodes.LastContactData_FAILED
	case "SKIPPED":
		return nodes.LastContactData_SKIPPED
	}
	return nodes.LastContactData_UNKNOWN
}

func (db *DB) fromDBNodeWithTargetConfig(inNode *dbNode) (*nodes.Node, error) {
	newNode, err := db.fromDBNode(inNode)
	if err != nil {
		return nil, err
	}

	tc := nodes.TargetConfig{}
	if inNode.TargetConfig != nil {
		err = json.Unmarshal(*inNode.TargetConfig, &tc)
		if err != nil {
			return nil, errors.Wrap(err, "fromDBNodeWithTargetConfig unable to unmarshal target config")
		}
		newNode.TargetConfig = &tc
	}
	return newNode, nil
}

func (db *DB) BulkAddNodes(inNodes []*nodes.Node) (nodeIDs []string, err error) {
	logrus.Debugf("BulkAddNodes adding nodes: %+v", inNodes)
	for _, inNode := range inNodes {
		if inNode.Name == "" && inNode.NamePrefix == "" {
			return nodeIDs, &utils.InvalidError{Msg: "Invalid node, a node 'name' or 'prefix-name' must be supplied"}
		}

		node, err := toDBNode(inNode)
		if err != nil {
			return nodeIDs, errors.Wrap(err, "BulkAddNodes unable to translate node to db struct")
		}
		for _, host := range inNode.GetTargetConfig().GetHosts() {
			err = Transact(db, func(tx *DBTrans) error {
				node.ID = createUUID()
				if inNode.Name == "" {
					node.Name = fmt.Sprintf("%s-%s", inNode.NamePrefix, host)
				}
				inNode.TargetConfig.Host = host
				jsonTC, err := json.Marshal(inNode.TargetConfig)
				if err != nil {
					return errors.Wrap(err, "BulkAddNodes error marshalling target_config")
				}
				node.TargetConfig = jsonTC
				err = tx.Insert(&node)
				if err != nil {
					return errors.Wrap(err, "BulkAddNodes unable to insert node")
				}

				tags, err := tx.addTags(inNode.Tags)
				if err != nil {
					return errors.Wrap(err, "BulkAddNodes unable to add tags")
				}

				err = tx.tagNode(node.ID, tags)
				if err != nil {
					return errors.Wrap(err, "BulkAddNodes unable to tag node")
				}

				err = tx.nodeSecret(node.ID, node.Secrets)
				if err != nil {
					return errors.Wrap(err, "BulkAddNodes unable to insert node_secret")
				}

				err = tx.updateNodeProjects(node.ID, inNode.Projects)
				if err != nil {
					return errors.Wrap(err, "BulkAddNodes unable to add projects to node")
				}

				nodeIDs = append(nodeIDs, node.ID)
				return nil
			})
			if err != nil {
				return nodeIDs, err
			}
		}
	}

	return nodeIDs, err
}

func (db *DB) AddNode(inNode *nodes.Node) (string, error) {
	if inNode.Name == "" {
		return "", &utils.InvalidError{Msg: "Invalid node, 'name' is a required parameter"}
	}

	node, err := toDBNode(inNode)
	if err != nil {
		return "", errors.Wrap(err, "AddNode unable to translate node to db struct")
	}

	node.ID = createUUID()

	err = Transact(db, func(tx *DBTrans) error {

		err = tx.Insert(&node)
		if err != nil {
			return errors.Wrap(err, "AddNode unable to insert node")
		}

		tags, err := tx.addTags(inNode.Tags)
		if err != nil {
			return errors.Wrap(err, "AddNode unable to add tags")
		}

		err = tx.tagNode(node.ID, tags)
		if err != nil {
			return errors.Wrap(err, "AddNode unable to tag node")
		}

		err = tx.nodeSecret(node.ID, node.Secrets)
		if err != nil {
			return errors.Wrap(err, "AddNode unable to insert node_secret")
		}

		err = tx.updateNodeProjects(node.ID, inNode.Projects)
		if err != nil {
			return errors.Wrap(err, "AddNode unable to add projects to node")
		}

		return nil
	})

	return node.ID, err
}

type TotalCount struct {
	Total, Unreachable, Reachable, Unknown int32
}

func (db *DB) GetNodes(sortField string, insortOrder nodes.Query_OrderType, pageNr int32, perPage int32, filters []*common.Filter) ([]*nodes.Node, *TotalCount, error) {
	var sortOrder string
	sortField = valueOrDefaultStr(sortField, "name")
	if insortOrder == 1 {
		sortOrder = "desc"
	} else {
		sortOrder = "asc"
	}
	pageNr = valueOrDefaultInt(pageNr, 1) - 1
	perPage = valueOrDefaultInt(perPage, 100)

	if nodesSortFields[sortField] == "" {
		return nil, nil, &utils.InvalidError{Msg: fmt.Sprintf("Invalid sort field, valid ones are: %v", getMapKeys(nodesSortFields))}
	}
	if !stringutils.SliceContains(validOrderFields, sortOrder) {
		return nil, nil, &utils.InvalidError{Msg: fmt.Sprintf("Invalid order, valid ones are: %v", validOrderFields)}
	}

	filters = handleNodeTagFilters(filters)

	err := validateNodeFilters(filters)
	if err != nil {
		return nil, nil, errors.Wrapf(err, "GetNodes error validating node filters")
	}

	whereFilter, havingFilter, err := buildWhereHavingFilter(filters, "n", nodesFilterField)
	if err != nil {
		return nil, nil, errors.Wrap(err, "GetNodes error building where filter")
	}

	nodes := make([]*nodes.Node, 0)

	var nodesDaos []*dbNode
	query := fmt.Sprintf(selectNodes, whereFilter, havingFilter, nodesSortFields[sortField], sortOrder)
	logrus.Debugf("SQL: %s %d %d", query, perPage, pageNr*perPage)
	_, err = db.Select(&nodesDaos, query, perPage, pageNr*perPage)

	if err != nil {
		return nodes, nil, errors.Wrap(err, "GetNodes unable to get nodes")
	}

	// `total` should count items returned by query and the other
	// totals should be constant, so we fetch the unreachable, reachable,
	// and unknown counts in a separate, unfiltered query
	var nodeCounts []*nodeCounts
	// check if the filters contain a filter requesting only the nodes that have a manager
	// this is done so that we can get an accurate count of managed nodes in the ui for the scanner nodes
	countQuery := getNodeCounts
	if filtersContainsRequestForOnlyManagedNodes(filters) {
		countQuery = getNodeCounts + " WHERE manager != ''"
	}
	_, err = db.Select(&nodeCounts, countQuery)
	if err != nil {
		return nodes, nil, errors.Wrap(err, "GetNodes unable to count node totals")
	}

	var totalCount, unreachableTotal, reachableTotal, unknownTotal int32
	if len(nodesDaos) > 0 {
		totalCount = int32(nodesDaos[0].TotalCount)
	}
	if len(nodeCounts) > 0 {
		unreachableTotal = int32(nodeCounts[0].UnreachableCount)
		reachableTotal = int32(nodeCounts[0].ReachableCount)
		unknownTotal = int32(nodeCounts[0].UnknownCount)
	}

	for _, nodeDao := range nodesDaos {
		node, err := db.fromDBNode(nodeDao)
		if err != nil {
			return nodes, nil, errors.Wrap(err, "GetNodes unable to translate nodes from db struct")
		}
		nodes = append(nodes, node)
	}
	total := TotalCount{
		Total:       totalCount,
		Unreachable: unreachableTotal,
		Reachable:   reachableTotal,
		Unknown:     unknownTotal,
	}
	return nodes, &total, nil
}

func (db *DB) DeleteNodesById(ctx context.Context, ids []string) ([]string, error) {
	var names []string
	_, err := db.Select(&names, deleteNodesById, pq.Array(ids))
	if err != nil {
		return []string{}, errors.Wrap(err, "DeleteNodesById unable to delete nodes")
	}
	return names, nil
}

func (db *DB) DeleteNodesWithQuery(filters []*common.Filter) ([]string, error) {
	filters = handleNodeTagFilters(filters)

	err := validateNodeFilters(filters)
	if err != nil {
		return nil, errors.Wrapf(err, "DeleteNodesWithQuery error validating node filters")
	}

	whereFilter, havingFilter, err := buildWhereHavingFilter(filters, "n", nodesFilterField)
	if err != nil {
		return nil, errors.Wrap(err, "DeleteNodesWithQuery error building where filter")
	}

	var names []string

	_, err = db.Select(&names, fmt.Sprintf(deleteNodesWithQuery, whereFilter, havingFilter))
	if err != nil {
		return []string{}, errors.Wrap(err, "DeleteNodesWithQuery unable to delete nodes")
	}

	return names, nil
}

func handleNodeTagFilters(filters []*common.Filter) []*common.Filter {
	newFilters := make([]*common.Filter, len(filters))

	for i, filter := range filters {
		if _, found := nodesFilterField[filter.Key]; found {
			newFilters[i] = filters[i]
		} else {
			newFilters[i] = &common.Filter{
				Key:     fmt.Sprintf("tags:%s", filter.Key),
				Values:  filter.Values,
				Exclude: filter.Exclude,
			}
		}
	}

	return newFilters
}

var nodesFilterField = map[string]string{
	"account_id":                   "source_account_id", // azure: uuid, aws: 10 digit #
	"last_contact":                 "last_contact",
	"manager_id":                   "manager_id",
	"manager_type":                 "manager", // aws-ec2, aws-api, azure-api, automate
	"name":                         "name",
	"platform_name":                "platform",
	"platform_release":             "platform_version",
	"project":                      "project",
	"region":                       "source_region",
	"source_id":                    "source_id",
	"state":                        "source_state", // running, stopped, terminated
	"statechange_timerange":        "statechange_timestamp",
	"status":                       "status", // reachable, unreachable, unknown
	"last_run_timerange":           "last_run ->> 'EndTime'",
	"last_scan_timerange":          "last_scan ->> 'EndTime'",
	"last_run_status":              "last_run ->> 'Status'",
	"last_scan_status":             "last_scan ->> 'Status'",
	"last_run_penultimate_status":  "last_run ->> 'PenultimateStatus'",
	"last_scan_penultimate_status": "last_scan ->> 'PenultimateStatus'",
}

func validateNodeFilters(filters []*common.Filter) error {
	for _, filter := range filters {
		switch filter.Key {
		case "manager_type":
			for _, item := range filter.Values {
				if !isValidNodeManagerType(item) {
					return &utils.InvalidError{Msg: fmt.Sprintf("Invalid manager_type filter: %s. manager_type must be one of the following: 'aws-ec2', 'aws-api', 'azure-api', 'automate'", item)}
				}
			}
		case "status":
			for _, item := range filter.Values {
				if !isValidStatus(item) {
					return &utils.InvalidError{Msg: fmt.Sprintf("Invalid status filter: %s. status must be one of the following: 'reachable', 'unreachable', 'unknown'", item)}
				}
			}
		case "state":
			for _, item := range filter.Values {
				if !isValidState(item) {
					return &utils.InvalidError{Msg: fmt.Sprintf("Invalid state filter: %s. state must be one of the following: 'RUNNING', 'STOPPED', 'TERMINATED'", item)}
				}
			}
		case "statechange_timerange":
			for _, item := range filter.Values {
				_, err := time.Parse(time.RFC3339, item)
				if err != nil {
					return &utils.InvalidError{Msg: fmt.Sprintf("Invalid statechange_timerange filter: %s. statechange_timerange entered is not valid timestamp", item)}
				}
			}
		case "last_contact":
			for _, item := range filter.Values {
				_, err := time.Parse(time.RFC3339, item)
				if err != nil {
					return &utils.InvalidError{Msg: fmt.Sprintf("Invalid last_contact filter: %s. last_contact entered is not valid timestamp", item)}
				}
			}
		}
	}
	return nil
}

func isValidNodeManagerType(item string) bool {
	switch item {
	// empty string covers legacy nodes (nodes created before we started associating all manual nodes with "automate" as a manager and nodes brought over from a1)
	case "aws-ec2", "aws-api", "azure-api", "azure-vm", "automate", "":
		return true
	default:
		return false
	}
}

func isValidStatus(item string) bool {
	switch item {
	case "reachable", "unreachable", "unknown":
		return true
	default:
		return false
	}
}

func isValidState(item string) bool {
	if len(item) == 0 {
		return true // this covers the empty string case
	}
	switch item {
	case "STOPPED", "RUNNING", "TERMINATED":
		return true
	default:
		return false
	}
}

func filtersContainsRequestForOnlyManagedNodes(filters []*common.Filter) bool {
	for _, filter := range filters {
		if filter.Key == "manager_type" && len(filter.Values) == 1 && len(filter.Values[0]) == 0 && filter.Exclude {
			return true
		}
	}
	return false
}

func (db *DB) GetNode(ctx context.Context, id string) (*nodes.Node, error) {
	var node dbNode

	logrus.Debugf("Getting node %s", id)
	// Args are the where clause, order field, order direction
	query := fmt.Sprintf(selectNodes, "WHERE n.id = $3", "", "name", "desc")
	// Args are the query, limit, offset, node id
	err := db.SelectOne(&node, query, 1, 0, id)
	if err != nil {
		return nil, utils.ProcessSQLNotFound(err, id, "GetNode unable to select node")
	}

	n, err := db.fromDBNodeWithTargetConfig(&node)
	if err != nil {
		return nil, errors.Wrap(err, "GetNode unable to translate node from db struct")
	}
	sort.Strings(n.Projects)

	return n, nil
}

func (db *DB) GetNodeSecretIds(ctx context.Context, id string) ([]string, error) {
	secretIds := make([]string, 0)

	logrus.Debugf("Getting secrets for node %s", id)
	_, err := db.Select(&secretIds, selectSecretIdsFromNodes, id)
	if err != nil {
		return secretIds, errors.Wrap(err, "GetNodeSecretIds unable to select secrets from db for node")
	}
	return secretIds, nil
}

//UpdateNode takes an inNode model struct and uses it to update a Node in the db.
func (db *DB) UpdateNode(inNode *nodes.Node) error {
	if inNode == nil {
		return errors.New("UpdateNode unable to update node, no node provided")
	}

	node, err := toDBNode(inNode)
	if err != nil {
		return errors.Wrap(err, "UpdateNode unable to translate node to db struct")
	}

	err = Transact(db, func(tx *DBTrans) error {
		err = tx.processNodesTagsUpdate(inNode)
		if err != nil {
			return errors.Wrap(err, "UpdateNode unable to process node tags update")
		}

		err = tx.processNodesSecretsUpdate(inNode)
		if err != nil {
			return errors.Wrap(err, "UpdateNode unable to process node secret updates")
		}

		err = tx.updateNodeProjects(node.ID, inNode.Projects)
		if err != nil {
			return errors.Wrap(err, "UpdateNode unable to add projects to node")
		}

		_, err := tx.Exec(updateNodeById, node.ID, node.Name, node.TargetConfig)
		if err != nil {
			return errors.Wrap(err, "UpdateNode unable to update node")
		}
		return nil
	})

	return err
}

func (trans *DBTrans) processNodesSecretsUpdate(inNode *nodes.Node) (err error) {
	var secretIdsByNodeId []string

	_, err = trans.Select(&secretIdsByNodeId, selectSecretIdsFromNodes, inNode.Id)
	if err != nil {
		err = errors.Wrap(err, "processNodesSecretsUpdate unable to update node secrets")
		return
	}

	secretsToAdd := utils.DiffArray(inNode.TargetConfig.Secrets, secretIdsByNodeId)
	nodeSecretsToAdd := make([]interface{}, 0)
	for _, secret := range secretsToAdd {
		nodeSecretsToAdd = append(nodeSecretsToAdd, &NodeSecret{NodeID: inNode.Id, SecretID: secret})
	}

	secretsToDelete := utils.DiffArray(secretIdsByNodeId, inNode.TargetConfig.Secrets)
	nodeSecretsToDelete := make([]*NodeSecret, 0)
	for _, secret := range secretsToDelete {
		nodeSecretsToDelete = append(nodeSecretsToDelete, &NodeSecret{NodeID: inNode.Id, SecretID: secret})
	}

	if len(secretsToDelete) > 0 {
		for _, nodeSecretToDelete := range nodeSecretsToDelete {
			_, err = trans.Exec(deleteNodesSecretsByNodeId, inNode.Id, nodeSecretToDelete.SecretID)
			if err != nil {
				err = errors.Wrap(err, "processNodesSecretsUpdate unable to delete secret")
				return
			}
		}
	}

	if len(secretsToAdd) > 0 {
		err = trans.Insert(nodeSecretsToAdd...)
		if err != nil {
			err = errors.Wrap(err, "processNodesSecretsUpdate unable to insert secrets")
			return
		}
	}
	return
}

func (trans *DBTrans) processNodesTagsUpdate(inNode *nodes.Node) error {
	//get list of tags from current node
	_, err := trans.Exec(deleteNodeTags, inNode.Id)
	if err != nil {
		return errors.Wrap(err, "processNodesTagsUpdate unable to delete node tags")
	}

	tags, err := trans.addTags(inNode.Tags)
	if err != nil {
		return errors.Wrap(err, "processNodesTagsUpdate unable to add tags")
	}

	err = trans.tagNode(inNode.Id, tags)
	if err != nil {
		return errors.Wrap(err, "processNodesTagsUpdate unable to tag node")
	}
	return nil

}

func (db *DB) DeleteNode(id string) (int64, error) {
	var node node

	node.ID = id

	err := Transact(db, func(tx *DBTrans) error {
		_, err := tx.Exec(deleteNodeTags, id)
		if err != nil {
			return errors.Wrap(err, "DeleteNode unable to delete node tags")
		}

		_, err = tx.Delete(&node)
		if err != nil {
			return errors.Wrap(err, "DeleteNode unable to delete node")
		}

		return nil
	})

	return 0, err
}

func (trans *DBTrans) nodeSecret(nodeID string, secretIDs []string) error {
	links := make([]interface{}, 0)
	for _, secretID := range secretIDs {
		link := NodeSecret{
			NodeID:   nodeID,
			SecretID: secretID,
		}
		links = append(links, &link)
	}
	return trans.Insert(links...)
}

func (db *DB) GetNodeState(id string) (string, error) {
	var state string
	err := db.QueryRow(sqlNodeState, id).Scan(&state)
	if err != nil {
		return "", errors.Wrapf(err, "GetNodeState unable to query node id for state %s", id)
	}
	return state, nil
}

func (db *DB) QueryManualNodesFields(ctx context.Context, filters []*common.Filter, field string) ([]string, error) {
	var perPage int32 = 100
	var total int64 = 100
	nodes := make([]*nodes.Node, 0)
	results := make([]string, 0)
	for cnt := int32(1); int64(len(nodes)) < total; cnt++ {
		logrus.Debugf("getting nodes with page %d for total %d, per_page %d", cnt, total, perPage)
		pageNodes, totalCount, err := db.GetNodes("", 0, cnt, perPage, filters)
		if err != nil {
			logrus.Errorf("handleManuallyManagedNodes unable to get nodes with query: %+v  aborting job: %s", filters, err.Error())
			return nil, err
		}
		total = int64(totalCount.Total)
		nodes = append(nodes, pageNodes...)
	}
	for _, node := range nodes {
		if strings.HasPrefix(field, "tags:") {
			for _, kv := range node.Tags {
				if strings.TrimPrefix(field, "tags:") == kv.Key {
					results = append(results, kv.Value)
				}
			}
		}
		switch field {
		case "name":
			results = append(results, node.Name)
		case "tags":
			for _, kv := range node.Tags {
				results = append(results, kv.Key)
			}
		}
	}
	return utils.UniqueStringSlice(results), nil
}

// Update node with inspec detect job results. This includes platform name, platform release,
// last_contact, status and state
func (db *DB) UpdateNodeDetectInfo(ctx context.Context, nodeDetectInfo *nodes.NodeDetectJobInfo) (err error) {
	now := time.Now().UTC()
	var updated sql.Result
	time, err := ptypes.Timestamp(nodeDetectInfo.JobEndTime)
	if err != nil {
		logrus.Errorf("UpdateNodeDetectInfo unable to parse job end time")
		time = now
	}
	switch nodeDetectInfo.NodeStatus {
	case types.StatusCompleted:
		switch nodeDetectInfo.JobType {
		case "detect":
			if nodeStatus(nodeDetectInfo.NodeStatus) == "reachable" {
				updated, err = db.Exec(sqlUpdateNodeDetectCompletedWithState, nodeDetectInfo.PlatformName, nodeDetectInfo.PlatformRelease, nodeStatus(nodeDetectInfo.NodeStatus), time, nodeDetectInfo.JobId, manager.NodeState_RUNNING.String(), nodeDetectInfo.NodeId)
			} else {
				updated, err = db.Exec(sqlUpdateNodeDetectCompleted, nodeDetectInfo.PlatformName, nodeDetectInfo.PlatformRelease, nodeStatus(nodeDetectInfo.NodeStatus), time, nodeDetectInfo.JobId, nodeDetectInfo.NodeId)
			}
		case "exec":
			if nodeStatus(nodeDetectInfo.NodeStatus) == "reachable" {
				updated, err = db.Exec(sqlUpdateNodeExecCompletedWithState, nodeStatus(nodeDetectInfo.NodeStatus), time, nodeDetectInfo.JobId, manager.NodeState_RUNNING.String(), nodeDetectInfo.NodeId)
			} else {
				updated, err = db.Exec(sqlUpdateNodeExecCompleted, nodeStatus(nodeDetectInfo.NodeStatus), time, nodeDetectInfo.JobId, nodeDetectInfo.NodeId)
			}
		default:
			return errors.Errorf("UpdateNodeDetectInfo unknown node JobType %s", nodeDetectInfo.JobType)
		}
	case types.StatusFailed:
		updated, err = db.Exec(sqlUpdateNodeDetectFailed, nodeStatus(nodeDetectInfo.NodeStatus), nodeDetectInfo.JobId, nodeDetectInfo.NodeId, now)
	default:
		return errors.Errorf("UpdateNodeDetectInfo unknown NodeStatus %s", nodeDetectInfo.NodeStatus)
	}
	if err != nil {
		return errors.Wrapf(err, "UpdateNodeDetectInfo unable to update node %s", nodeDetectInfo.NodeId)
	}
	rows, err := updated.RowsAffected()
	if err != nil {
		return errors.Wrap(err, "UpdateNodeDetectInfo unable to read rows affected")
	}
	if rows == 1 {
		logrus.Debugf("updated node %s", nodeDetectInfo.NodeId)
	} else {
		logrus.Error("unexpected results. more than one node was updated. something isn't right")
	}
	return nil
}

func nodeStatus(jobStatus string) string {
	switch jobStatus {
	case types.StatusCompleted:
		return "reachable"
	case types.StatusFailed:
		return "unreachable"
	default:
		return "unknown"
	}
}

// update node with the connection error
func (db *DB) UpdateNodeConnectionErr(ctx context.Context, connErr *nodes.NodeError) (err error) {
	_, err = db.Exec(sqlUpdateNodeConnectionError, connErr.NodeId, connErr.ConnectionError)
	return err
}

func (tx *DBTrans) updateNodeProjects(nodeID string, projectIDs []string) error {
	_, err := tx.Exec(`
		DELETE FROM nodes_projects
		WHERE node_id = $1
	`, nodeID)
	if err != nil {
		return err
	}

	insertProject, err := tx.Prepare(`
		INSERT into projects (id, project_id)
		VALUES ($1, $2)
		ON CONFLICT DO NOTHING
	`)
	if err != nil {
		return err
	}

	for _, projectID := range projectIDs {
		_, err = insertProject.Exec(createUUID(), projectID)
		if err != nil {
			return err
		}
	}

	_, err = tx.Exec(`
		INSERT into nodes_projects (node_id, project_id)
		SELECT $1, id
		FROM projects
		WHERE project_id = ANY($2)
	`, nodeID, pq.Array(projectIDs))

	return err
}
