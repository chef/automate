package pgdb

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/golang/protobuf/ptypes"
	"github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/lib/stringutils"
)

const selectNodeManagers = `
SELECT
  nm.id,
  nm.name,
  nm.type,
	nm.credentials,
	nm.instance_credentials,
  nm.status_type,
	nm.status_message,
	nm.date_added,
  COUNT(*)
  OVER () AS total_count
FROM node_managers nm
%s
ORDER BY %s %s
LIMIT $1
OFFSET $2;
`

const selectNodeManager = `
SELECT
  nm.id,
  nm.name,
  nm.type,
	nm.credentials,
	nm.instance_credentials,
  nm.status_type,
	nm.status_message,
	nm.account_id,
	nm.date_added
FROM node_managers nm
WHERE nm.id = $1;
`

const deleteNodeManagersNodes = `
DELETE from nodes
WHERE id IN (
  SELECT node_id
  FROM node_managers_nodes nmn
  WHERE nmn.manager_id = $1
    AND NOT EXISTS (
      SELECT 1
      FROM node_managers_nodes nmn2
      WHERE nmn2.node_id = nmn.node_id
        AND nmn2.manager_id != nmn.manager_id
    )
)
RETURNING id
`

const updateNodeManagersNodes = `
UPDATE nodes
SET manager = 'automate'
WHERE id IN (
  SELECT node_id
  FROM node_managers_nodes nmn
  WHERE nmn.manager_id = $1
    AND NOT EXISTS (
      SELECT 1
      FROM node_managers_nodes nmn2
      WHERE nmn2.node_id = nmn.node_id
        AND nmn2.manager_id != nmn.manager_id
    )
)
`

const updateNodeManagersNodesState = `
UPDATE nodes
SET manager = 'automate', source_state = $2
WHERE id IN (
  SELECT node_id
  FROM node_managers_nodes nmn
  WHERE nmn.manager_id = $1
    AND NOT EXISTS (
      SELECT 1
      FROM node_managers_nodes nmn2
      WHERE nmn2.node_id = nmn.node_id
        AND nmn2.manager_id != nmn.manager_id
    )
)
`

const sqlUpdateManagerStatus = `
UPDATE node_managers
SET status_type = $1
WHERE node_managers.id = $2
`

const deleteNodeManagersNodesJoins = `
DELETE FROM node_managers_nodes
WHERE node_managers_nodes.manager_id = $1
`

const updateNodeManagersNodesNotInList = `
UPDATE nodes
SET manager = 'automate'
FROM node_managers_nodes
WHERE node_managers_nodes.manager_id = $1
  AND node_managers_nodes.node_id = nodes.id
  AND nodes.id != ALL($2)
`

const deleteNodeManagersNodesJoinsNotInList = `
DELETE FROM node_managers_nodes
WHERE node_managers_nodes.manager_id = $1
  AND node_managers_nodes.node_id != ALL($2)
`

var nodeManagerSortFields = map[string]string{
	"name":           "LOWER(nm.name)",
	"type":           "LOWER(nm.type)",
	"status":         "LOWER(nm.status_type)",
	"status_message": "LOWER(nm.status_message)",
	"date_added":     "nm.date_added",
}

type nodeManagerSummary struct {
	nodeManager
	TotalCount int64 `db:"total_count"`
}

func toDBNodeManager(inNodeManager *manager.NodeManager) (nodeManager, error) {
	newNodeManager := nodeManager{}
	newNodeManager.ID = inNodeManager.Id
	newNodeManager.Name = inNodeManager.Name
	newNodeManager.Type = inNodeManager.Type
	newNodeManager.Credential = inNodeManager.CredentialId
	instanceCreds, err := json.Marshal(inNodeManager.InstanceCredentials)
	if err != nil {
		return newNodeManager, errors.Wrap(err, "toDBNodeManager unable to marshal instance_credentials")
	}
	newNodeManager.InstanceCredentials = instanceCreds
	newNodeManager.AccountID = inNodeManager.AccountId
	newNodeManager.DateAdded = timeNowRef()
	newNodeManager.Status = inNodeManager.Status

	return newNodeManager, nil
}

func fromDBNodeManager(inNodeManager *nodeManager) (manager.NodeManager, error) {
	newNodeManager := manager.NodeManager{}
	newNodeManager.Id = inNodeManager.ID
	newNodeManager.Name = inNodeManager.Name
	newNodeManager.Type = inNodeManager.Type
	newNodeManager.CredentialId = inNodeManager.Credential
	var creds []*manager.CredentialsByTags
	err := json.Unmarshal(inNodeManager.InstanceCredentials, &creds)
	if err != nil {
		return newNodeManager, errors.Wrap(err, "fromDBNodeManager unable to unmarshal instance_credentials")
	}
	newNodeManager.InstanceCredentials = creds
	newNodeManager.AccountId = inNodeManager.AccountID
	newNodeManager.DateAdded, err = ptypes.TimestampProto(inNodeManager.DateAdded)
	if err != nil {
		return newNodeManager, errors.Wrap(err, "fromDBNodeManager error translating date_added to timestamp")
	}
	newNodeManager.Status = inNodeManager.Status

	return newNodeManager, nil
}

func fromDBNodeManagerSummary(inNodeManager *nodeManagerSummary) (manager.NodeManager, error) {
	newNodeManager := manager.NodeManager{}
	newNodeManager.Id = inNodeManager.ID
	newNodeManager.Name = inNodeManager.Name
	newNodeManager.Type = inNodeManager.Type
	newNodeManager.CredentialId = inNodeManager.Credential
	var creds []*manager.CredentialsByTags
	err := json.Unmarshal(inNodeManager.InstanceCredentials, &creds)
	if err != nil {
		return newNodeManager, errors.Wrap(err, "fromDBNodeManagerSummary unable to unmarshal instance_credentials")
	}
	newNodeManager.InstanceCredentials = creds
	newNodeManager.DateAdded, err = ptypes.TimestampProto(inNodeManager.DateAdded)
	if err != nil {
		return newNodeManager, errors.Wrap(err, "fromDBNodeManagerSummary error translating date_added to timestamp")
	}
	newNodeManager.Status = inNodeManager.Status

	return newNodeManager, nil
}

func (db *DB) AddNodeManager(inNodeManager *manager.NodeManager, acctID string) (string, error) {
	nodeManager, err := toDBNodeManager(inNodeManager)
	if err != nil {
		return "", err
	}

	if nodeManager.ID != mgrtypes.AutomateManagerID {
		nodeManager.ID = createUUID()
	}
	nodeManager.AccountID = acctID

	err = Transact(db, func(tx *DBTrans) error {
		logrus.Infof("nodemanager to insert, %+v", nodeManager.ID)
		if err = tx.Insert(&nodeManager); err != nil {
			return err
		}

		return nil
	})

	return nodeManager.ID, err
}

func (db *DB) UpdateNodeManager(inNodeManager *manager.NodeManager) (string, error) {
	nodeManager, err := toDBNodeManager(inNodeManager)
	if err != nil {
		return "", err
	}

	err = Transact(db, func(tx *DBTrans) error {
		logrus.Infof("nodemanager to update, %+v", nodeManager.ID)
		if _, err = tx.Update(&nodeManager); err != nil {
			return err
		}

		return nil
	})

	return nodeManager.ID, err
}

func (db *DB) GetNodeManager(id string) (*manager.NodeManager, error) {
	var nodeMan nodeManager
	var newNodeManager manager.NodeManager

	err := db.SelectOne(&nodeMan, selectNodeManager, id)
	if err != nil {
		return &newNodeManager, err
	}

	newNodeManager, err = fromDBNodeManager(&nodeMan)
	if err != nil {
		return &newNodeManager, err
	}
	return &newNodeManager, nil
}

func (db *DB) GetCredentialFromNodeManager(ctx context.Context, id string, secretsClient secrets.SecretsServiceClient) (*secrets.Secret, error) {
	var secret *secrets.Secret
	nodeMngr, err := db.GetNodeManager(id)
	if err != nil {
		logrus.Infof("Could not find node manager with id: %+s", id)
		return secret, err
	}

	if len(nodeMngr.CredentialId) == 0 {
		return nil, nil
	}

	secret, err = secretsClient.Read(ctx, &secrets.Id{Id: nodeMngr.CredentialId})
	if err != nil {
		logrus.Infof("Could not find secret with id: %+s", id)
		return secret, err
	}

	return secret, nil
}

func (db *DB) GetNodeManagers(sortField string, insortOrder manager.Query_OrderType,
	pageNr int32, perPage int32, filters []*common.Filter) ([]*manager.NodeManager, int64, error) {

	var nodeManagerDaos []nodeManagerSummary

	var sortOrder string
	sortField = valueOrDefaultStr(sortField, "name")
	if insortOrder == 1 {
		sortOrder = "desc"
	} else {
		sortOrder = "asc"
	}
	pageNr = valueOrDefaultInt(pageNr, 1) - 1
	perPage = valueOrDefaultInt(perPage, 100)

	if nodeManagerSortFields[sortField] == "" {
		return nil, 0, fmt.Errorf("Invalid sort field, valid ones are: %v", getMapKeys(nodeManagerSortFields))
	}
	if !stringutils.SliceContains(validOrderFields, sortOrder) {
		return nil, 0, fmt.Errorf("Invalid order, valid ones are: %v", validOrderFields)
	}
	err := validateNodeManagerFilters(filters)
	if err != nil {
		return nil, 0, errors.Wrapf(err, "GetNodeManagers error validating node manager filters")
	}
	whereFilter, _, err := buildWhereHavingFilter(filters, "nm", mgrFilterField)
	if err != nil {
		return nil, 0, errors.Wrap(err, "GetNodeManagers error building where filter")
	}
	_, err = db.Select(&nodeManagerDaos, fmt.Sprintf(selectNodeManagers, whereFilter, nodeManagerSortFields[sortField], sortOrder), perPage, pageNr*perPage)

	totalCount := int64(0)
	if len(nodeManagerDaos) > 0 {
		totalCount = nodeManagerDaos[0].TotalCount
	}

	nodeManagers := make([]*manager.NodeManager, 0)
	if err != nil {
		return nil, 0, err
	}

	for _, nodeManagerDao := range nodeManagerDaos {
		nodeManager, err := fromDBNodeManagerSummary(&nodeManagerDao)
		if err != nil {
			return nil, 0, err
		}
		nodeManagers = append(nodeManagers, &nodeManager)
	}

	return nodeManagers, totalCount, nil
}

var mgrFilterField = map[string]string{
	"manager_type": "type",
}

func validateNodeManagerFilters(filters []*common.Filter) error {
	for _, filter := range filters {
		switch filter.Key {
		case "manager_type":
			for _, item := range filter.Values {
				if !isValidManagerType(item) {
					return &utils.InvalidError{Msg: fmt.Sprintf("Invalid manager type filter: %s. manager_type must be one of the following: 'aws-ec2', 'aws-api', 'azure-api'", item)}
				}
			}
		}
	}
	return nil
}

func isValidManagerType(managerType string) bool {
	switch managerType {
	case "aws-ec2", "aws-api", "azure-api":
		return true
	default:
		return false
	}
}

func (db *DB) DeleteNodeManager(id string) error {
	err := deleteNodeManager(db, id, func(tx *DBTrans) error {
		_, err := tx.Exec(updateNodeManagersNodes, id)
		return err
	})

	return err
}

func (db *DB) DeleteNodeManagerWithNodes(id string) ([]string, error) {
	var ids []string

	err := deleteNodeManager(db, id, func(tx *DBTrans) error {
		_, err := tx.Select(&ids, deleteNodeManagersNodes, id)
		return err
	})

	return ids, err
}

func (db *DB) DeleteNodeManagerWithNodeStateUpdate(id string, state string) error {
	err := deleteNodeManager(db, id, func(tx *DBTrans) error {
		_, err := tx.Exec(updateNodeManagersNodesState, id, state)
		return err
	})
	return err
}

func deleteNodeManager(db *DB, id string, deleteFunc func(*DBTrans) error) error {
	var nodeMan nodeManager
	var err error

	// get mgr type by mgr id
	mgr, err := db.GetNodeManager(id)
	if err != nil {
		return errors.Wrapf(err, "deleteNodeManager unable to retrieve existing node mgr with id: %s", id)
	}
	// return err if user is trying to delete manual node manager
	if mgr.Type == "automate" {
		return &utils.InvalidError{Msg: "Invalid request. Unable to delete manual node manager"}
	}

	err = Transact(db, func(tx *DBTrans) error {
		err = deleteFunc(tx)
		if err != nil {
			return err
		}
		_, err = tx.Exec(deleteNodeManagersNodesJoins, id)
		if err != nil {
			return err
		}
		nodeMan.ID = id
		_, err = tx.Delete(&nodeMan)
		if err != nil {
			return err
		}
		return nil
	})

	return err
}

func (db *DB) RemoveStaleNodeAssociations(managerId string, nodeIds []string) error {
	var err error

	err = Transact(db, func(tx *DBTrans) error {
		_, err = tx.Exec(updateNodeManagersNodesNotInList, managerId, pq.Array(nodeIds))
		if err != nil {
			return err
		}
		_, err = tx.Exec(deleteNodeManagersNodesJoinsNotInList, managerId, pq.Array(nodeIds))
		if err != nil {
			return err
		}
		return nil
	})

	return err
}

func (db *DB) AssociateNodeIDsWithManagerID(nodeIDs []string, nodeManagerID string) error {
	for _, nodeID := range nodeIDs {
		_, err := db.Exec(sqlInsertNodeManagerNode, nodeManagerID, nodeID)
		if err != nil {
			return errors.Wrapf(err, "AssociateNodesWithManagerID unable to insert node_managers_nodes manager_id: %s node_id: %s", nodeManagerID, nodeID)
		}
	}
	return nil
}

func (db *DB) UpdateManagerStatus(mgrId string, status string) error {
	_, err := db.Exec(sqlUpdateManagerStatus, status, mgrId)
	return err
}
