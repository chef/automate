package pgdb

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
)

const sqlUpsertByIDRunData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state,
		last_contact, source_id, source_region, source_account_id, last_run, projects_data, manager)
VALUES ($1, $2, $3, $4, $5, $6, NULLIF($7,''), NULLIF($8,''), NULLIF($9,''), $10, $11, $12)
ON CONFLICT (id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5,
	last_contact = $6, source_id = NULLIF($7,''), source_region = NULLIF($8,''), source_account_id = NULLIF($9,''), last_run = $10, projects_data = $11
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpsertByIDScanData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state,
		last_contact, source_id, source_region, source_account_id, last_job, last_scan, projects_data, manager)
VALUES ($1, $2, $3, $4, $5, $6, NULLIF($7,''), NULLIF($8,''), NULLIF($9,''), $10, $11, $12, $13)
ON CONFLICT (id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5,
	last_contact = $6, source_id = NULLIF($7,''), source_region = NULLIF($8,''), source_account_id = NULLIF($9,''), last_job = $10, last_scan = $11, projects_data = $12
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpsertBySourceIDRunData = `
SELECT upsert_by_source_id_run_data($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12);`

const sqlUpsertBySourceIDScanData = `
SELECT upsert_by_source_id_scan_data($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13);`

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
	logrus.Debugf("processing node %s with uuid %s", node.Name, node.Uuid)
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

	// note: we shouldn't be able to ever get here without a node id, because this function is called from the
	// ingestion process, and the ingestion process will reject a report with no node uuid
	if len(node.GetUuid()) == 0 {
		return fmt.Errorf("no uuid included in message. aborting.")
	}

	// warn if manager is empty; it should not be
	if len(node.GetManagerType()) == 0 {
		logrus.Warnf("manager value is empty for node %s", node.GetName())
	}

	// the incoming node may hit any of these cases:
	// 1) it is already registered in our db with same uuid as incoming report: update the node entry
	// 2) it is already registered in our db with diff uuid, same source_id, region, acct id: update the node by source_id
	// 3) it is not in our db, we must add it

	lastContactInfo, err := db.handleIncomingLastContactData(node)
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to parse node last contact data")
	}
	lastContactDataByte, err := json.Marshal(lastContactInfo)
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to marshal last contact data")
	}
	projectsDataByte, err := json.Marshal(node.GetProjectsData())
	if err != nil {
		return errors.Wrap(err, "ProcessIncomingNode unable to marshal projects data")
	}

	err = Transact(db, func(tx *DBTrans) error {
		logrus.Debugf("processing node %s with cloud info %s %s %s", node.GetName(), node.GetSourceId(), node.GetSourceAccountId(), node.GetSourceRegion())
		nodeDetails := nodeDetails{
			nodeState:           nodeState,
			lastContact:         lastContact,
			mgrType:             node.GetManagerType(),
			lastContactDataByte: lastContactDataByte,
			projectsDataByte:    projectsDataByte,
		}

		if !hasCloudInformation(node) {
			if node.GetManagerType() == "azure-api" && node.SourceRegion == "" {
				node.SourceRegion = "azure-api-region"
			}
			err = tx.upsertByID(node, nodeDetails)
		} else {
			node.Uuid, err = tx.upsertByCloudDetails(node, nodeDetails)
			if err != nil {
				return errors.Wrap(err, "ProcessIncomingNode unable to upsert with cloudDetails")
			}
		}
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to process node")
		}
		if node.ManagerId != "" {
			_, err = tx.Exec(sqlInsertNodeManagerNode, node.ManagerId, node.Uuid)
			if err != nil {
				return errors.Wrap(err, "ProcessIncomingNode unable to create manager-node association")
			}
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

type nodeDetails struct {
	nodeState           string
	lastContact         string
	mgrType             string
	lastContactDataByte []byte
	projectsDataByte    []byte
}

func (tx *DBTrans) upsertByID(node *manager.NodeMetadata, details nodeDetails) error {
	var err error
	if node.GetScanData() != nil {
		_, err = tx.Exec(sqlUpsertByIDScanData, node.GetUuid(),
			node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
			details.nodeState, details.lastContact, node.GetSourceId(), node.GetSourceRegion(), node.GetSourceAccountId(),
			node.GetJobUuid(), details.lastContactDataByte, details.projectsDataByte, details.mgrType)
	} else if node.GetRunData() != nil {
		_, err = tx.Exec(sqlUpsertByIDRunData, node.GetUuid(),
			node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
			details.nodeState, details.lastContact, node.GetSourceId(), node.GetSourceRegion(), node.GetSourceAccountId(),
			details.lastContactDataByte, details.projectsDataByte, details.mgrType)
	}
	return err
}

func (tx *DBTrans) upsertByCloudDetails(node *manager.NodeMetadata, details nodeDetails) (string, error) {
	var id string
	var err error
	if node.GetScanData() != nil {
		id, err = tx.SelectStr(sqlUpsertBySourceIDScanData, node.GetUuid(),
			node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
			details.nodeState, details.lastContact, node.GetSourceId(), node.GetSourceRegion(),
			node.GetSourceAccountId(), node.GetJobUuid(), details.lastContactDataByte, details.projectsDataByte, details.mgrType)
	} else if node.GetRunData() != nil {
		id, err = tx.SelectStr(sqlUpsertBySourceIDRunData, node.GetUuid(),
			node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
			details.nodeState, details.lastContact, node.GetSourceId(), node.GetSourceRegion(),
			node.GetSourceAccountId(), details.lastContactDataByte, details.projectsDataByte, details.mgrType)
	}
	if len(id) > 0 {
		logrus.Debugf("found match for node %s with details: %s %s %s. Node was updated.", node.GetName(), node.GetSourceId(), node.GetSourceRegion(), node.GetSourceAccountId())
		return id, err
	}
	return node.Uuid, err
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

func hasCloudInformation(node *manager.NodeMetadata) bool {
	return len(node.GetSourceId()) != 0 && len(node.GetSourceAccountId()) != 0 && len(node.GetSourceRegion()) != 0
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
