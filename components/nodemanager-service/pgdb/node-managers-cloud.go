package pgdb

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
)

const sqlMgrIds = `
SELECT id
FROM node_managers
WHERE node_managers.type = $1;
`

const sqlUpsertInstanceSourceState = `
INSERT INTO nodes
 (id, name, source_id, source_state, source_region, source_account_id, target_config, statechange_timestamp, manager)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET source_state = $4, statechange_timestamp = $8
WHERE nodes.source_state != 'TERMINATED'
RETURNING id;
`

const sqlUpdateInstanceSourceStateAndStatus = `
UPDATE nodes
SET source_state = $4, status = $5, statechange_timestamp = $6, connection_error = $7
WHERE nodes.source_id = $1 AND nodes.source_region = $2 AND nodes.source_account_id = $3 AND nodes.source_state != 'TERMINATED'
RETURNING id;
`

const sqlInsertManagerNode = `
INSERT INTO nodes
  (id, manager, source_id, target_config, name, source_region, source_account_id)
VALUES ($1, $2, $3, $4, $5, $6, $7)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET manager = $2
RETURNING id;
`

const sqlInsertNodeManagerNode = `
INSERT INTO node_managers_nodes
	(manager_id, node_id)
VALUES ($1, $2)
ON CONFLICT
DO NOTHING;
`

func (db *DB) AddManagerSubscriptionsToDB(subs []*manager.ManagerNode, managerId string, tenantID string, credential string) []string {
	nodeIds := make([]string, 0)
	for _, item := range subs {
		uuid := uuid.Must(uuid.NewV4()).String()
		name := item.Name
		tc := nodes.TargetConfig{
			Backend:        "azure",
			SubscriptionId: item.Id,
		}
		jsonTc, err := json.Marshal(tc)
		if err != nil {
			logrus.Errorf("AddManagerSubscriptionsToDB unable to marshal target_config info for region %s", item.Name)
			continue
		}

		err = Transact(db, func(tx *DBTrans) error {
			uuid, err = tx.SelectStr(sqlInsertManagerNode, uuid, "azure-api", item.Id, jsonTc, name, "", tenantID)
			if err != nil {
				return errors.Wrapf(err, "AddManagerSubscriptionsToDB unable to insert subscription with id %s", item.Id)
			}
			// add credential to nodes_secrets table
			err = tx.nodeSecret(uuid, []string{credential})
			if err != nil {
				return errors.Wrap(err, "AddManagerSubscriptionsToDB unable to insert node_secret")
			}
			_, err = tx.Exec(sqlInsertNodeManagerNode, managerId, uuid)
			if err != nil {
				return errors.Wrapf(err, "AddManagerSubscriptionsToDB unable to insert node_managers_nodes manager_id: %s node_id: %s", managerId, uuid)
			}
			return nil
		})
		if err != nil {
			logrus.Errorf("AddManagerSubscriptionsToDB unable to insert subscription %s %v", item.Name, err)
			continue
		}
		nodeIds = append(nodeIds, uuid)
	}
	return nodeIds
}

func (db *DB) AddManagerNodeToDB(managerId string, managerAcctId string, credential string, acctAlias string) ([]string, error) {
	nodeIds := make([]string, 0)
	var name string
	uuid := uuid.Must(uuid.NewV4()).String()
	if len(acctAlias) > 0 {
		name = acctAlias
	} else {
		name = managerAcctId
	}
	tc := nodes.TargetConfig{
		Backend: "aws",
		Region:  "us-east-1",
	}
	jsonTc, err := json.Marshal(tc)
	if err != nil {
		logrus.Errorf("AddManagerNodeToDB unable to marshal target_config info for account %s", acctAlias)
		jsonTc = make([]byte, 0)
	}
	sourceID := managerAcctId

	err = Transact(db, func(tx *DBTrans) error {
		uuid, err = tx.SelectStr(sqlInsertManagerNode, uuid, "aws-api", sourceID, jsonTc, name, acctAlias, managerAcctId)
		if err != nil {
			return errors.Wrapf(err, "AddManagerNodeToDB unable to insert node for account %s", acctAlias)
		}
		// add credential to nodes_secrets table
		err = tx.nodeSecret(uuid, []string{credential})
		if err != nil {
			return errors.Wrap(err, "AddManagerNodeToDB unable to insert node_secret")
		}
		_, err = tx.Exec(sqlInsertNodeManagerNode, managerId, uuid)
		if err != nil {
			return errors.Wrapf(err, "AddManagerNodeToDB unable to insert node_managers_nodes manager_id: %s node_id: %s", managerId, uuid)
		}
		return nil
	})

	if err != nil {
		logrus.Errorf("AddManagerRegionsToDB unable to insert node for %s %v", acctAlias, err)
		return nodeIds, errors.Wrapf(err, "AddManagerRegionsToDB unable to insert node for %s", acctAlias)
	}
	nodeIds = append(nodeIds, uuid)

	return nodeIds, nil
}

// Adds gcp-api node that corresponds to a gcp node manager
func (db *DB) AddManagerGcpApi(managerId string, managerAcctId string, credential string) (*string, error) {
	uuid := uuid.Must(uuid.NewV4()).String()
	tc := nodes.TargetConfig{
		Backend:        "gcp",
		SubscriptionId: managerAcctId,
	}
	jsonTc, err := json.Marshal(tc)
	if err != nil {
		logrus.Errorf("AddManagerGcpApi unable to marshal target_config info for project %s", managerAcctId)
		jsonTc = make([]byte, 0)
	}

	err = Transact(db, func(tx *DBTrans) error {
		// We name the node same as the gcp project id(e.g. chef-compliance-prod-1) which is a unique and immutable id in Google cloud
		uuid, err = tx.SelectStr(sqlInsertManagerNode, uuid, "gcp-api", managerAcctId, jsonTc, managerAcctId, "", managerAcctId)
		if err != nil {
			return errors.Wrapf(err, "AddManagerGcpApi unable to insert project %s", managerAcctId)
		}
		// add credential to nodes_secrets table
		err = tx.nodeSecret(uuid, []string{credential})
		if err != nil {
			return errors.Wrap(err, "AddManagerGcpApi unable to insert node_secret")
		}
		_, err = tx.Exec(sqlInsertNodeManagerNode, managerId, uuid)
		if err != nil {
			return errors.Wrapf(err, "AddManagerGcpApi unable to insert node_managers_nodes manager_id: %s node_id: %s", managerId, uuid)
		}
		return nil
	})

	if err != nil {
		logrus.Errorf("AddManagerGcpApi unable to insert gcp-api node %s", uuid)
		return nil, err
	}
	return &uuid, nil
}

func (db *DB) AddManagerNodesToDB(nodesArr []*manager.ManagerNode, managerId string, managerAcctId string, instanceCredentials []*manager.CredentialsByTags, mgrType string) []string {
	nodeIds := make([]string, 0)
	for _, item := range nodesArr {
		uuid := uuid.Must(uuid.NewV4()).String()
		backend := "ssh"
		if item.Platform == "windows" {
			backend = "winrm"
		}
		if strings.HasPrefix(item.Ssm, "Online") {
			// this indicates we will run an "ssm" job (aws sys mgr send_command, azure run_command)
			backend = "ssm"
		}
		if item.Name == "" {
			if len(item.Host) > 0 {
				item.Name = item.Host
			} else {
				item.Name = item.PublicIp
			}
		}
		tc := nodes.TargetConfig{
			Backend: backend,
			Host:    item.PublicIp,
		}
		jsonTc, err := json.Marshal(tc)
		if err != nil {
			logrus.Errorf("AddManagerNodesToDB unable to marshal target_config info for source id %s", item.Id)
			jsonTc = make([]byte, 0)
		}

		credsForInstance := make([]string, 0)
		for _, credTagGroup := range instanceCredentials {
			for _, kv := range item.Tags {
				isMatch := utils.KvMatches(credTagGroup.TagKey, credTagGroup.TagValue, kv)
				if isMatch {
					credsForInstance = append(credsForInstance, credTagGroup.CredentialIds...)
				}
			}
		}

		commonTags := make([]*common.Kv, len(item.Tags))
		for i, tag := range item.Tags {
			commonTags[i] = &common.Kv{
				Key:   tag.Key,
				Value: tag.Value,
			}
		}

		err = Transact(db, func(tx *DBTrans) error {
			uuid, err = tx.SelectStr(sqlInsertManagerNode, uuid, mgrType, item.Id, jsonTc, item.Name, item.Region, managerAcctId)
			if err != nil {
				return errors.Wrapf(err, "AddManagerNodesToDB unable to insert node with source id %s", item.Id)
			}
			// add instancecredentials to nodes_secrets table
			err = tx.nodeSecret(uuid, credsForInstance)
			if err != nil {
				return errors.Wrap(err, "AddManagerNodesToDB unable to insert node_secret")
			}
			_, err = tx.Exec(deleteNodeTags, uuid)
			if err != nil {
				return errors.Wrap(err, "AddManagerNodesToDB unable to delete existing node tags")
			}
			// add tags to nodes_secrets table
			tags, err := tx.addTags(commonTags)
			if err != nil {
				return errors.Wrap(err, "AddManagerNodesToDB unable to add tags")
			}
			err = tx.tagNode(uuid, tags)
			if err != nil {
				return errors.Wrap(err, "AddManagerNodesToDB unable to tag node")
			}
			_, err = tx.Exec(sqlInsertNodeManagerNode, managerId, uuid)
			if err != nil {
				return errors.Wrapf(err, "AddManagerRegionsToDB unable to insert node_managers_nodes manager_id: %s node_id: %s", managerId, uuid)
			}
			return nil
		})
		if err != nil {
			logrus.Errorf("AddManagerNodesToDB unable to complete transaction to insert node and nodes secrets, %+v", err)
			continue
		}
		nodeIds = append(nodeIds, uuid)
	}
	return nodeIds
}

func (db *DB) GetAllManagersByType(mgrType string) ([]string, error) {
	var id string
	mgrIds := make([]string, 0)
	rows, err := db.Query(sqlMgrIds, mgrType)
	if err != nil {
		return mgrIds, errors.Wrap(err, "GetAllManagersByType unable to query for nodemgr ids")
	}
	defer rows.Close() // nolint: errcheck
	for rows.Next() {
		err := rows.Scan(&id)
		if err != nil {
			logrus.Error(err)
			continue
		}
		logrus.Debugf("adding mgr id %s to the list", id)
		mgrIds = append(mgrIds, id)
	}
	err = rows.Err()
	if err != nil {
		return mgrIds, errors.Wrap(err, "GetAllManagersByType rows error")
	}

	return mgrIds, nil
}

type InstanceState struct {
	ID     string
	Name   string
	State  string
	Region string
}

// UpdateInstanceSourceStateInDb updates source_state of instance passed in, and updates
// the status as unreachable if the source_state is anything other than running
// if any rows are inserted or updated returns true, otherwise false
func (db *DB) UpdateOrInsertInstanceSourceStateInDb(instance InstanceState, mgrID string, sourceAcctID string, mgrType string) (bool, error) {
	uuid := uuid.Must(uuid.NewV4()).String()
	instanceState := convertInstanceStateToManagerInstanceState(instance.State)
	nowTime := time.Now().UTC()
	tcByte, err := json.Marshal(nodes.TargetConfig{})
	if err != nil {
		return false, errors.Wrap(err, "UpdateInstanceSourceState unable to marshal target config")
	}
	var id string
	connectionErr := fmt.Sprintf("node state is %s", instanceState)
	name := instance.Name
	if len(name) == 0 {
		name = instance.ID
	}
	switch instanceState {
	case "TERMINATED", "STOPPED":
		// if the instance state we're getting is terminated or stopped, we only want to update the node if we already
		// have it in the system. it's silly to *add* nodes that are already terminated/stopped here.
		id, err = db.SelectStr(sqlUpdateInstanceSourceStateAndStatus, instance.ID, instance.Region, sourceAcctID, instanceState, "unreachable", nowTime, connectionErr)
	case "RUNNING":
		id, err = db.SelectStr(sqlUpsertInstanceSourceState, uuid, name, instance.ID, instanceState, instance.Region, sourceAcctID, tcByte, nowTime, mgrType)
	default:
		logrus.Errorf("unknown instance state reported %s", instanceState)
		return false, nil
	}
	if err != nil {
		return false, errors.Wrapf(err, "UpdateInstanceSourceState unable to update instance %s %s", instance.ID, name)
	}
	if len(id) == 0 {
		return false, nil
	}
	err = db.AssociateNodeIDsWithManagerID([]string{id}, mgrID)
	if err != nil {
		return false, errors.Wrapf(err, "UpdateInstanceSourceState unable to associate manager with node %s", name)
	}
	return true, nil
}

func convertInstanceStateToManagerInstanceState(state string) string {
	switch state {
	case "running":
		return manager.NodeState_RUNNING.String()
	case "terminated":
		return manager.NodeState_TERMINATED.String()
	default:
		return manager.NodeState_STOPPED.String()
	}
}
