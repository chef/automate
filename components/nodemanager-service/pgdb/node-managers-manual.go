package pgdb

import (
	"time"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

const sqlGetManualDueNodes = `
SELECT id
FROM nodes
WHERE nodes.manager = 'automate' AND nodes.source_state != 'STOPPED' AND nodes.source_state != 'TERMINATED' AND nodes.last_contact <= $1 AND nodes.last_connection_attempt <= $1;
`

const sqlChangeNodeStateAndStatus = `
UPDATE nodes
SET source_state = $2, status = $3, statechange_timestamp = $4
WHERE id = $1 AND nodes.source_state != 'TERMINATED';
`

const sqlChangeNodeState = `
UPDATE nodes
SET source_state = $2, statechange_timestamp = $3
WHERE id = $1 AND nodes.source_state != 'TERMINATED';
`

func (db *DB) GetManualNodesDue(timeAgo time.Time) ([]string, error) {
	var id string
	nodeIds := make([]string, 0)
	rows, err := db.Query(sqlGetManualDueNodes, timeAgo)
	if err != nil {
		return nodeIds, errors.Wrap(err, "GetManualNodesDue unable to query for nodes")
	}
	defer rows.Close() // nolint: errcheck
	for rows.Next() {
		err := rows.Scan(&id)
		if err != nil {
			logrus.Error(err)
			continue
		}
		logrus.Debugf("adding node id %s to the list", id)
		nodeIds = append(nodeIds, id)
	}
	err = rows.Err()
	if err != nil {
		return nodeIds, errors.Wrap(err, "GetManualNodesDue rows error")
	}

	return nodeIds, nil
}

func (db *DB) ChangeNodeState(nodeState *manager.NodeState) error {
	nowTime := time.Now().UTC()
	if nodeState.State == manager.NodeState_RUNNING {
		_, err := db.Exec(sqlChangeNodeState, nodeState.Id, nodeState.State.String(), nowTime)
		if err != nil {
			return errors.Wrapf(err, "ChangeNodeState unable to change node state for node%s", nodeState.Id)
		}
		return nil
	}
	_, err := db.Exec(sqlChangeNodeStateAndStatus, nodeState.Id, nodeState.State.String(), "unreachable", nowTime)
	if err != nil {
		return errors.Wrapf(err, "ChangeNodeState unable to change node state for node%s", nodeState.Id)
	}
	return nil
}
