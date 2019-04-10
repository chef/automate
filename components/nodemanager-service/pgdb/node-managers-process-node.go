package pgdb

import (
	"database/sql"
	"encoding/json"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
)

const sqlUpsertBySourceID = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, last_job, last_contact, source_id, source_region, source_account_id, target_config)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET source_state = $5, last_contact = $7, last_job = $6, name = $2
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpdateNodeLastContact = `
UPDATE nodes
SET source_state = $1, last_contact = $2, last_job = $3, name = $4, platform = $5, platform_version = $6
WHERE nodes.source_state != 'TERMINATED' AND id=$7;
`

const SqlInsertNode = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, last_job, last_contact, target_config)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8);
`

const SqlInsertNodeWithSourceInfo = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, last_job, last_contact, target_config, source_id, source_region, source_account_id)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);
`

func (db *DB) ProcessIncomingNode(node *manager.NodeMetadata) error {
	logrus.Debugf("processing node with uuid %s", node.Uuid)
	// if node.LastContact is less than 10 min ago, we can assume node to be in "running" state.
	// if it is more than 10 min ago, we don't know what the state is, so we set to empty string
	tenMinAgo := time.Now().UTC().Add(time.Minute * -10)
	timestamp, err := ptypes.Timestamp(node.LastContact)
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to parse node last contact")
	}
	nodeState := ""
	if timestamp.After(tenMinAgo) {
		nodeState = manager.NodeState_RUNNING.String()
	}
	// TODO: give the target config for nodes table a default value in db
	// so we don't have to do this.
	tcByte, err := json.Marshal(nodes.TargetConfig{})
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to marshal target config")
	}
	lastContact := ptypes.TimestampString(node.LastContact)
	// the incoming node may hit any of these cases:
	// 1) it is already registered in our db with same uuid as report: update the node entry
	// 2) it is already registered in our db with diff uuid, same source_id: update the node by source_id
	// 3) it is not in our db, we must add it
	// TODO (@vj): revisit this to address race condition issue; either decide on a code guard
	// against that case, or put it all in sql

	// here we address case #1
	rows := int64(0)
	if node.Uuid != "" {
		updated, err := updateNode(node.Uuid, node, lastContact, db, nodeState)
		if err != nil {
			return errors.Wrapf(err, "ProcessIncomingNode error updating node %s", node.Uuid)
		}
		rows, err = updated.RowsAffected()
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to read rows affected")
		}
	}
	if rows > 0 {
		logrus.Debugf("updated node %s with last_contact %s", node.Uuid, node.LastContact)
		return nil
	}
	if node.SourceId != "" {
		// here we address case #2
		err := upsertNodeBySourceID(node, lastContact, db, nodeState, tcByte)
		if err != nil {
			return errors.Wrapf(err, "ProcessIncomingNode unable to upsert node by source_id %s", node.SourceId)
		}
	} else {
		// here we address case #3
		err := addNodeToDB(node, lastContact, db, nodeState, tcByte)
		if err != nil {
			// TODO:
			// if we get here and there is a duplicate node id error, we probably attempted to send a report in
			// on a terminated node. we should check and log that msg if it is the case
			return errors.Wrapf(err, "ProcessIncomingNode unable to add node to db %s", node.Name)
		}
	}
	return nil
}

func updateNode(nodeUuid string, node *manager.NodeMetadata, lastContact string, db *DB, nodeState string) (sql.Result, error) {
	updated, err := db.Exec(sqlUpdateNodeLastContact, nodeState, lastContact, node.JobUuid, node.Name, node.PlatformName, node.PlatformRelease, nodeUuid)
	if err != nil {
		return nil, errors.Wrapf(err, "updateNode unable to update node %s", node.Uuid)
	}
	return updated, nil
}

func upsertNodeBySourceID(node *manager.NodeMetadata, lastContact string, db *DB, nodeState string, tc []byte) error {
	logrus.Debugf("doing an upsert on node with source_id %s", node.SourceId)
	nodeUuid := node.Uuid
	if node.Uuid == "" {
		nodeUuid = uuid.Must(uuid.NewV4()).String()
	}
	_, err := db.Exec(sqlUpsertBySourceID, nodeUuid, node.Name, node.PlatformName, node.PlatformRelease, nodeState, node.JobUuid, lastContact, node.SourceId, node.SourceRegion, node.SourceAccountId, tc)
	if err != nil {
		return errors.Wrapf(err, "upsertNodeBySourceID unable to add node %s", node.SourceId)
	}
	return nil
}

func addNodeToDB(node *manager.NodeMetadata, lastContact string, db *DB, nodeState string, tc []byte) error {
	var added sql.Result
	var err error
	nodeUuid := node.Uuid
	if node.Uuid == "" {
		nodeUuid = uuid.Must(uuid.NewV4()).String()
	}
	logrus.Debugf("adding node %s", nodeUuid)
	if len(node.SourceId) == 0 && len(node.SourceRegion) == 0 && len(node.SourceAccountId) == 0 {
		added, err = db.Exec(SqlInsertNode, nodeUuid, node.Name, node.PlatformName, node.PlatformRelease, nodeState, node.JobUuid, lastContact, tc)
	} else {
		added, err = db.Exec(SqlInsertNodeWithSourceInfo, nodeUuid, node.Name, node.PlatformName, node.PlatformRelease, nodeState, node.JobUuid, lastContact, tc, node.SourceId, node.SourceRegion, node.SourceAccountId)
	}
	if err != nil {
		return errors.Wrapf(err, "addNodeToDB unable to create node %s", node.Uuid)
	}
	rows, err := added.RowsAffected()
	if err != nil {
		return errors.Wrap(err, "addNodeToDB unable to read rows affected")
	}
	if rows == 1 {
		logrus.Debugf("created node %s", nodeUuid)
	}
	return nil
}
