package pgdb

import (
	"encoding/json"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/api/common"
)

const sqlFindTagID = `
SELECT id
FROM tags
WHERE key = $1 AND value = $2;
`

const sqlFindNodeTag = `
SELECT exists(select 1 from nodes_tags where node_id = $1 AND tag_id = $2);
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

func (trans *DBTrans) nodeTagExists(nodeID string, tagID string) (bool, error) {
	var exists bool
	rows, err := trans.Query(sqlFindNodeTag, nodeID, tagID)
	if err != nil {
		return false, err
	}
	defer rows.Close() // nolint: errcheck
	for rows.Next() {
		err := rows.Scan(&exists)
		if err != nil {
			logrus.Error(err)
			continue
		}
	}
	return exists, rows.Err()
}

func (trans *DBTrans) tagNode(nodeID string, tagIDs []string) error {
	links := make([]interface{}, 0, len(tagIDs))

	for _, tagID := range tagIDs {
		// check if node_tag association already exists
		exists, err := trans.nodeTagExists(nodeID, tagID)
		if err != nil {
			return err
		}
		if !exists {
			link := NodeTag{
				NodeID: nodeID,
				TagID:  tagID,
			}
			links = append(links, &link)
		}
	}
	return trans.Insert(links...)
}

// FindKeyValue finds a Tag object in the array based on key match
func FindKeyValue(tags []*common.Kv, key string) *common.Kv {
	for _, tag := range tags {
		if tag.Key == key {
			return tag
		}
	}
	return &common.Kv{}
}

// RemoveKeyValue removes an item from the array base on key match
func RemoveKeyValue(tags []*common.Kv, key string) []*common.Kv {
	for i, tag := range tags {
		if tag.Key == key {
			tags[i] = tags[len(tags)-1]
			return tags[:len(tags)-1]
		}
	}
	return tags
}

// KeyValueToRawMap helps convert an array of KeyValues in a Map and convert it to json
func KeyValueToRawMap(arr []*common.Kv) (json.RawMessage, error) {
	zaMap := make(map[string]string, 0)
	for _, kv := range arr {
		zaMap[kv.Key] = kv.Value
	}
	jsonMap, err := json.Marshal(zaMap)
	if err != nil {
		return jsonMap, errors.Wrap(err, "keyValueToRawMap unable to marshal map")
	}
	return jsonMap, nil
}

// RawMapToKeyValue helps convert an array of KeyValues in a Map and convert it to json
func RawMapToKeyValue(rawJSON json.RawMessage) ([]*common.Kv, error) {
	var zaMap map[string]string
	err := json.Unmarshal(rawJSON, &zaMap)
	if err != nil {
		return nil, errors.Wrap(err, "rawMapToKeyValue unable to unmarshal map")
	}

	zaArray := make([]*common.Kv, 0, len(zaMap))
	for k, v := range zaMap {
		zaArray = append(zaArray, &common.Kv{Key: k, Value: v})
	}
	return zaArray, nil
}
