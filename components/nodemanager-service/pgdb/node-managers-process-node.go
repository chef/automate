package pgdb

import (
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
)

const sqlUpsertByID = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_region, source_account_id, last_job)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
ON CONFLICT (id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_job = $9
WHERE nodes.source_state != 'TERMINATED';
`

const sqlUpsertBySourceID = `
INSERT INTO nodes
	(id, name, platform, platform_version, source_state, 
		last_contact, source_id, source_region, source_account_id, last_job)
VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
ON CONFLICT (source_id, source_region, source_account_id)
DO UPDATE
SET name = $2, platform = $3, platform_version = $4, source_state = $5, 
	last_contact = $6, last_job = $10
WHERE nodes.source_state != 'TERMINATED';
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

	if len(node.GetSourceId()) == 0 || len(node.GetSourceAccountId()) == 0 || len(node.GetSourceRegion()) == 0 {
		_, err = db.Exec(sqlUpsertByID, node.GetUuid(),
			node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
			nodeState, lastContact, node.GetSourceRegion(), node.GetSourceAccountId(),
			node.GetJobUuid())
	} else {
		_, err = db.Exec(sqlUpsertBySourceID, node.GetUuid(),
			node.GetName(), node.GetPlatformName(), node.GetPlatformRelease(),
			nodeState, lastContact, node.GetSourceId(), node.GetSourceRegion(),
			node.GetSourceAccountId(), node.GetJobUuid())
	}
	return err
}
