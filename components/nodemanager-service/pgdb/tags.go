package pgdb

import (
	"encoding/json"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/pkg/errors"
)

const sqlInsertNodeTag = `
INSERT INTO nodes_tags
(node_id, tag_id)
VALUES ($1, $2)
ON CONFLICT (node_id, tag_id)
DO NOTHING;
`

const sqlInsertTag = `
INSERT INTO tags
(id, key, value)
VALUES ($1, $2, $3)
ON CONFLICT (key, value)
DO UPDATE SET value=$3 RETURNING id;
`

// Note: ^^ this is silly. apparently you can't do an ON CONFLICT
// DO NOTHING and still return the id, and we need to know the id b/c we still
// need it to do the nodes_tags association. So we do a silly update so we can get
// the id back from postgres :/ (vj)

func (trans *DBTrans) addTags(tags []*common.Kv) ([]string, error) {
	tagIDs := make([]string, 0, len(tags))

	for _, keyValue := range tags {
		id := createUUID()
		dbID, err := trans.SelectStr(sqlInsertTag, id, keyValue.Key, keyValue.Value)
		if err != nil {
			return tagIDs, errors.Wrap(err, "addTags unable to add tag")
		}
		if len(dbID) > 0 {
			id = dbID
		}
		tagIDs = append(tagIDs, id)
	}

	return tagIDs, nil
}

func (trans *DBTrans) tagNode(nodeID string, tagIDs []string) error {
	for _, tagID := range tagIDs {
		_, err := trans.Exec(sqlInsertNodeTag, nodeID, tagID)
		if err != nil {
			return errors.Wrap(err, "tagNode unable to add insert node tag")
		}
	}
	return nil
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
	var zaArray []*common.Kv
	err := json.Unmarshal(rawJSON, &zaMap)
	if err != nil {
		return zaArray, errors.Wrap(err, "rawMapToKeyValue unable to unmarshal map")
	}
	for k, v := range zaMap {
		zaArray = append(zaArray, &common.Kv{Key: k, Value: v})
	}
	return zaArray, nil
}
