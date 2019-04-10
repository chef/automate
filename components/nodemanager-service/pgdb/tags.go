package pgdb

import (
	"encoding/json"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/pkg/errors"
)

func (trans *DBTrans) addTags(tags []*common.Kv) ([]string, error) {
	tagIDs := make([]string, 0, len(tags))
	tagArr := make([]interface{}, 0, len(tags))

	for _, keyValue := range tags {
		tag := tag{
			ID:    createUUID(),
			Key:   keyValue.Key,
			Value: keyValue.Value,
		}
		tagArr = append(tagArr, &tag)
		tagIDs = append(tagIDs, tag.ID)
	}

	err := trans.Insert(tagArr...)
	if err != nil {
		return tagIDs, errors.Wrap(err, "addTags unable to add tags in db")
	}

	return tagIDs, nil
}

func (trans *DBTrans) tagNode(nodeID string, tagIDs []string) error {
	links := make([]interface{}, 0, len(tagIDs))

	for _, tagID := range tagIDs {
		link := NodeTag{
			NodeID: nodeID,
			TagID:  tagID,
		}
		links = append(links, &link)
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
