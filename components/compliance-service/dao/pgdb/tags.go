package pgdb

import (
	"github.com/pkg/errors"

	"github.com/chef/automate/components/compliance-service/api/common"
)

type tag struct {
	ID    string `db:"id"`
	Key   string `db:"key"`
	Value string `db:"value"`
}

func (trans *DBTrans) addTags(tags []*common.Kv) ([]string, error) {
	tagIDs := make([]string, 0)
	tagArr := make([]interface{}, 0)

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

func (trans *DBTrans) tagJob(jobID string, tagIDs []string) error {
	links := make([]interface{}, 0)

	for _, tagID := range tagIDs {
		link := JobTag{
			JobID: jobID,
			TagID: tagID,
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
