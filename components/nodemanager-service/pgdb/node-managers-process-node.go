package pgdb

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"strconv"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
)

const sqlUpsertByIDRunData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_region, source_account_id, last_run)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
ON CONFLICT (id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_run = $9
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpsertByIDScanData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_region, source_account_id, last_job, last_scan)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
ON CONFLICT (id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_job = $9, last_scan = $10
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpsertBySourceIDRunData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_id, source_region, source_account_id, last_run)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_run = $10
WHERE nodes.source_state != 'TERMINATED' RETURNING id;
`

const sqlUpsertBySourceIDScanData = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_id, source_region, source_account_id, last_job, last_scan)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_job = $10, last_scan = $11
WHERE nodes.source_state != 'TERMINATED' RETURNING id;
`

const sqlGetCurrentRunStatus = `
SELECT last_run -> 'status' AS status
FROM nodes
WHERE id = $1 OR source_id = $2 AND source_region = $3 AND source_account_id = $4;
`

const sqlGetCurrentScanStatus = `
SELECT last_scan -> 'status' AS status
FROM nodes
WHERE id = $1 OR source_id = $2 AND source_region = $3 AND source_account_id = $4;
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

	lastContactDataByte := []byte{}
	if node.GetScanData() != nil {
		node.GetScanData().PenultimateStatus, err = db.getCurrentScanStatus(node)
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to get current scan status")
		}
		lastContactDataByte, err = json.Marshal(node.GetScanData())
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to parse node scan data")
		}
	} else if node.GetRunData() != nil {
		node.GetRunData().PenultimateStatus, err = db.getCurrentRunStatus(node)
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to get current scan status")
		}
		lastContactDataByte, err = json.Marshal(node.GetRunData())
		if err != nil {
			return errors.Wrap(err, "ProcessIncomingNode unable to parse node run data")
		}
	} else {
		msg := "ProcessIncomingNode: invalid request: scan_data or run_data must be provided"
		logrus.Errorf(msg)
		return fmt.Errorf(msg)
	}

	err = Transact(db, func(tx *DBTrans) error {
		if len(node.GetSourceId()) == 0 || len(node.GetSourceAccountId()) == 0 || len(node.GetSourceRegion()) == 0 {
			if node.GetScanData() != nil {
				_, err = db.Exec(sqlUpsertByIDScanData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceRegion(), node.GetSourceAccountId(),
					node.GetJobUuid(), lastContactDataByte)
			} else if node.GetRunData() != nil {
				_, err = db.Exec(sqlUpsertByIDRunData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceRegion(), node.GetSourceAccountId(),
					lastContactDataByte)
			}
		} else {
			var id string
			if node.GetScanData() != nil {
				id, err = db.SelectStr(sqlUpsertBySourceIDScanData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceId(), node.GetSourceRegion(),
					node.GetSourceAccountId(), node.GetJobUuid(), lastContactDataByte)
			} else if node.GetRunData() != nil {
				id, err = db.SelectStr(sqlUpsertBySourceIDRunData, node.GetUuid(),
					node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
					nodeState, lastContact, node.GetSourceId(), node.GetSourceRegion(),
					node.GetSourceAccountId(), lastContactDataByte)
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
		return tx.tagNode(node.GetUuid(), tags)
	})

	return err
}

func (db *DB) getCurrentScanStatus(node *manager.NodeMetadata) (string, error) {
	var status sql.NullString
	err := db.QueryRow(sqlGetCurrentScanStatus, node.GetUuid(),
		node.GetSourceId(), node.GetSourceRegion(), node.GetSourceAccountId()).Scan(&status)
	switch {
	case err == sql.ErrNoRows:
		return "", nil
	case err != nil:
		return "", errors.Wrap(err, "unable to query for current status")
	}
	if !status.Valid {
		return "", nil
	}
	statusString, err := strconv.Unquote(status.String)
	if err != nil {
		return "", errors.Wrap(err, "unable to read status")
	}
	return statusString, nil
}

func (db *DB) getCurrentRunStatus(node *manager.NodeMetadata) (string, error) {
	var status sql.NullString
	err := db.QueryRow(sqlGetCurrentRunStatus, node.GetUuid(),
		node.GetSourceId(), node.GetSourceRegion(), node.GetSourceAccountId()).Scan(&status)
	switch {
	case err == sql.ErrNoRows:
		return "", nil
	case err != nil:
		return "", errors.Wrap(err, "unable to query for current status")

	}
	if !status.Valid {
		return "", nil
	}
	statusString, err := strconv.Unquote(status.String)
	if err != nil {
		return "", errors.Wrap(err, "unable to read status")
	}
	return statusString, nil
}
