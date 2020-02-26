package dao

import (
	"encoding/json"

	"github.com/pkg/errors"

	"github.com/chef/automate/api/external/common/query"
)

type secretTag struct {
	ID    string `db:"id"`
	Key   string `db:"key"`
	Value string `db:"value"`
}

func (trans *DBTrans) addSecretTags(tags []*query.Kv) ([]string, error) {
	tagIDs := make([]string, 0)
	tagArr := make([]interface{}, 0)
	for _, keyValue := range tags {
		tag := secretTag{
			ID:    createUUID(),
			Key:   keyValue.Key,
			Value: keyValue.Value,
		}
		tagArr = append(tagArr, &tag)
		tagIDs = append(tagIDs, tag.ID)
	}

	err := trans.Insert(tagArr...)
	if err != nil {
		return tagIDs, errors.Wrap(err, "addSecretTags unable to add tags to db")
	}

	return tagIDs, nil
}

func (trans *DBTrans) tagSecret(secretID string, tagIDs []string) error {
	links := make([]interface{}, 0)

	for _, tagID := range tagIDs {
		link := SecretTag{
			SecretID: secretID,
			TagID:    tagID,
		}
		links = append(links, &link)
	}
	return trans.Insert(links...)
}

// KeyValueToRawMap helps convert an array of KeyValues in a Map and convert it to json
func KeyValueToRawMap(arr []*query.Kv) (json.RawMessage, error) {
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
func RawMapToKeyValue(rawJSON json.RawMessage) ([]*query.Kv, error) {
	var zaMap map[string]string

	err := json.Unmarshal(rawJSON, &zaMap)
	if err != nil {
		return nil, errors.Wrap(err, "rawMapToKeyValue unable to unmarshal map")
	}

	zaArray := make([]*query.Kv, 0, len(zaMap))
	for k, v := range zaMap {
		zaArray = append(zaArray, &query.Kv{Key: k, Value: v})
	}
	return zaArray, nil
}
