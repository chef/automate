package pgdb

import (
	"encoding/json"
	"fmt"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
)

const sqlUpsertByIDRunData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_region, source_account_id, last_run, projects_data)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
ON CONFLICT (id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_run = $9, projects_data = $10
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpsertByIDScanData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_region, source_account_id, last_job, last_scan, projects_data)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
ON CONFLICT (id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_job = $9, last_scan = $10, projects_data = $11
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpsertBySourceIDRunData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_id, source_region, source_account_id, last_run, projects_data)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_run = $10, projects_data = $11
WHERE nodes.source_state != 'TERMINATED' RETURNING id;
`

const sqlUpsertBySourceIDScanData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_id, source_region, source_account_id, last_job, last_scan, projects_data)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_job = $10, last_scan = $11, projects_data = $12
WHERE nodes.source_state != 'TERMINATED' RETURNING id;
`

const sqlGetCurrentRunStatus = `
SELECT coalesce(last_run ->> 'Status', '') AS status
FROM nodes
WHERE id = $1 OR source_id = $2 AND source_region = $3 AND source_account_id = $4;
`

const sqlGetCurrentScanStatus = `
SELECT coalesce(last_scan ->> 'Status', '') AS status
FROM nodes
WHERE id = $1 OR source_id = $2 AND source_region = $3 AND source_account_id = $4;
`

type lastContactData struct {
	ID                string
	Status            string
	PenultimateStatus string
	EndTime           string
}

func (db *DB) ProcessIncomingNode(node *manager.NodeMetadata) error {
	logrus.Infof("processing node with uuid %s", node.Uuid)
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

	lastContact := ptypes.TimestampString(node.LastContact)

	if len(node.GetUuid()) == 0 {
		id, err := uuid.NewV4()
		if err != nil {
			return err
		}
		node.Uuid = id.String()
	}

	// the incoming node may hit any of these cases:
	// 1) it is already registered in our db with same uuid as incoming report: update the node entry
	// 2) it is already registered in our db with diff uuid, same source_id: update the node by source_id
	// 3) it is not in our db, we must add it

	lastContactInfo, err := db.handleIncomingLastContactData(node)
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to parse node last contact data")
	}
	lastContactDataByte, err := json.Marshal(lastContactInfo)
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to marshal last contact data")
	}
	projectsDataByte, err := json.Marshal(node.GetProjects())
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to marshal projects data")
	}

	err = Transact(db, func(tx *DBTrans) error {
		if len(node.GetSourceId()) == 0 || len(node.GetSourceAccountId()) == 0 || len(node.GetSourceRegion()) == 0 {
			if node.GetScanData() != nil {
				_, err = db.Exec(sqlUpsertByIDScanData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceRegion(), node.GetSourceAccountId(),
					node.GetJobUuid(), lastContactDataByte, projectsDataByte)
			} else if node.GetRunData() != nil {
				_, err = db.Exec(sqlUpsertByIDRunData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceRegion(), node.GetSourceAccountId(),
					lastContactDataByte, projectsDataByte)
			}
		} else {
			var id string
			if node.GetScanData() != nil {
				id, err = db.SelectStr(sqlUpsertBySourceIDScanData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceId(), node.GetSourceRegion(),
					node.GetSourceAccountId(), node.GetJobUuid(), lastContactDataByte, projectsDataByte)
			} else if node.GetRunData() != nil {
				id, err = db.SelectStr(sqlUpsertBySourceIDRunData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceId(), node.GetSourceRegion(),
					node.GetSourceAccountId(), lastContactDataByte, projectsDataByte)
			}
			if len(id) > 0 {
				node.Uuid = id
			}
		}
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to process node")
		}
		tags, err := tx.addTags(node.GetTags())
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to add tags")
		}
		err = tx.tagNode(node.GetUuid(), tags)
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to tag node")
		}
		return tx.updateNodeProjects(node.GetUuid(), node.GetProjects())
	})

	return err
}

func (db *DB) handleIncomingLastContactData(node *manager.NodeMetadata) (lastContactData, error) {
	var err error
	var lastContactInfo lastContactData

	if node.GetScanData() == nil && node.GetRunData() == nil {
		msg := "invalid request: scan_data or run_data must be provided"
		logrus.Errorf(msg)
		return lastContactInfo, fmt.Errorf(msg)
	}

	if node.GetScanData() != nil {
		lastContactInfo, err = translateToDBStruct(node.GetScanData())
		if err != nil {
			return lastContactInfo, errors.Wrap(err, "ProcessIncomingNode unable to translate struct to db struct")
		}
		lastContactInfo.PenultimateStatus, err = db.getCurrentScanStatus(node)
		if err != nil {
			return lastContactInfo, errors.Wrap(err, "ProcessIncomingNode unable to get current scan status")
		}
	} else if node.GetRunData() != nil {
		lastContactInfo, err = translateToDBStruct(node.GetRunData())
		if err != nil {
			return lastContactInfo, errors.Wrap(err, "ProcessIncomingNode unable to translate struct to db struct")
		}
		lastContactInfo.PenultimateStatus, err = db.getCurrentRunStatus(node)
		if err != nil {
			return lastContactInfo, errors.Wrap(err, "ProcessIncomingNode unable to get current scan status")
		}
	}
	return lastContactInfo, nil
}

func translateToDBStruct(nodeData *nodes.LastContactData) (lastContactData, error) {
	lastContactData := lastContactData{
		ID:                nodeData.GetId(),
		Status:            nodeData.GetStatus().String(),
		PenultimateStatus: nodeData.GetPenultimateStatus().String(),
	}
	if nodeData.GetEndTime() != nil {
		time := ptypes.TimestampString(nodeData.GetEndTime())
		lastContactData.EndTime = time
	}
	return lastContactData, nil
}

func (db *DB) getCurrentScanStatus(node *manager.NodeMetadata) (string, error) {
	status, err := db.SelectStr(sqlGetCurrentScanStatus, node.GetUuid(),
		node.GetSourceId(), node.GetSourceRegion(), node.GetSourceAccountId())
	if err != nil {
		return "", errors.Wrap(err, "unable to read status")
	}
	return status, nil
}

func (db *DB) getCurrentRunStatus(node *manager.NodeMetadata) (string, error) {
	status, err := db.SelectStr(sqlGetCurrentRunStatus, node.GetUuid(),
		node.GetSourceId(), node.GetSourceRegion(), node.GetSourceAccountId())
	if err != nil {
		return "", errors.Wrap(err, "unable to read status")
	}
	return status, nil
}
