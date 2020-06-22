package pgdb

import (
	"github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/compliance/common"
)

const sqlFindTagID = `
SELECT id
FROM tags
WHERE key = $1 AND value = $2;
`

// Insert into node_tags excluding (node_id,tag_id) pairs
// that already exist
const sqlInsertNodeTag = `
INSERT INTO nodes_tags(node_id, tag_id) 
	(
		SELECT $1, tid 
		FROM 
			unnest($2::TEXT[]) tid 
		LEFT JOIN nodes_tags ON 
			nodes_tags.tag_id=tid AND 
			node_id=$1 
		WHERE nodes_tags.node_id IS NULL
	);
`

func (trans *DBTrans) addTags(tags []*common.Kv) ([]string, error) {
	tagIDs := make([]string, 0, len(tags))
	tagArr := make([]interface{}, 0, len(tags))

	for _, keyValue := range tags {
		// check if tag exists
		id, err := trans.tagExists(keyValue)
		if err != nil {
			return tagIDs, errors.Wrap(err, "addTags unable to check for tag existence in db")
		}

		if len(id) == 0 {
			// create tag and add to tag array if not exists
			tag := tag{
				ID:    createUUID(),
				Key:   keyValue.Key,
				Value: keyValue.Value,
			}
			tagArr = append(tagArr, &tag)
			id = tag.ID
		}
		// add id of tag to tagIDs
		tagIDs = append(tagIDs, id)
	}

	err := trans.Insert(tagArr...)
	if err != nil {
		return tagIDs, errors.Wrap(err, "addTags unable to add tags in db")
	}

	return tagIDs, nil
}

func (trans *DBTrans) tagExists(tag *common.Kv) (string, error) {
	var id string
	rows, err := trans.Query(sqlFindTagID, tag.GetKey(), tag.GetValue())
	if err != nil {
		return id, errors.Wrap(err, "tagExists unable to query for tag")
	}
	defer rows.Close() // nolint: errcheck
	for rows.Next() {
		err := rows.Scan(&id)
		if err != nil {
			logrus.Error(err)
			continue
		}
	}
	return id, rows.Err()
}

func (trans *DBTrans) tagNode(nodeID string, tagIDs []string) error {
	_, err := trans.Exec(sqlInsertNodeTag, nodeID, pq.Array(tagIDs))
	return err
}
