package pgdb

import (
	"encoding/json"
	"testing"

	"github.com/chef/automate/components/compliance-service/api/common"

	"github.com/stretchr/testify/assert"
)

func TestFindKeyValueReturnsEmptyKeyValuePair(t *testing.T) {
	var tags []*common.Kv

	result := FindKeyValue(tags, "foo")

	assert.Equal(t, &common.Kv{}, result)
}

func TestFindKeyValeuReturnsEmptykeyValuePairWhenNoMatch(t *testing.T) {
	secret1 := common.Kv{Key: "key1", Value: "value1"}
	secret2 := common.Kv{Key: "key2", Value: "value2"}
	tags := []*common.Kv{&secret1, &secret2}

	result := FindKeyValue(tags, "foo")

	assert.Equal(t, &common.Kv{}, result)
}

func TestFindKeyValueReturnsValueWhenFound(t *testing.T) {
	secret1 := common.Kv{Key: "key1", Value: "value1"}
	secret2 := common.Kv{Key: "key2", Value: "value2"}
	tags := []*common.Kv{&secret1, &secret2}

	result := FindKeyValue(tags, "key2")

	assert.Equal(t, &secret2, result)
}

func TestRemoveKeyValueReturnsEmptyArrayGivenEmptyArray(t *testing.T) {
	var tags []*common.Kv

	result := RemoveKeyValue(tags, "key2")

	assert.ElementsMatch(t, tags, result)
}

func TestRemoveKeyValueReturnsSameArrayWhenKeyNotFound(t *testing.T) {
	secret1 := common.Kv{Key: "key1", Value: "value1"}
	secret2 := common.Kv{Key: "key2", Value: "value2"}
	tags := []*common.Kv{&secret1, &secret2}

	result := RemoveKeyValue(tags, "foo")

	assert.ElementsMatch(t, tags, result)
}

func TestRemoveKeyValueReturnsArrayWithKeyRemoved(t *testing.T) {
	secret1 := common.Kv{Key: "key1", Value: "value1"}
	secret2 := common.Kv{Key: "key2", Value: "value2"}
	tags := []*common.Kv{&secret1, &secret2}

	result := RemoveKeyValue(tags, "key1")

	assert.Equal(t, []*common.Kv{&secret2}, result)
}

func TestKeyValueToRawMapReturnsEmptyMessageWhenArrayEmpty(t *testing.T) {
	var tags []*common.Kv
	var bytes []byte
	var err error
	var j json.RawMessage

	if j, err = KeyValueToRawMap(tags); err != nil {
		t.FailNow()
	}

	bytes, _ = j.MarshalJSON()

	assert.Equal(t, "{}", string(bytes))
}

func TestKeyValueToRawMapReturnsJsonMessage(t *testing.T) {
	secret1 := common.Kv{Key: "key1", Value: "value1"}
	secret2 := common.Kv{Key: "key2", Value: "value2"}
	tags := []*common.Kv{&secret1, &secret2}

	var bytes []byte
	var err error
	var j json.RawMessage

	if j, err = KeyValueToRawMap(tags); err != nil {
		t.FailNow()
	}

	bytes, _ = j.MarshalJSON()

	assert.Equal(t, "{\"key1\":\"value1\",\"key2\":\"value2\"}", string(bytes))
}

func TestRawMapToKeyValueReturnsEmptyTagsWhenJsonEmpty(t *testing.T) {
	var tags []*common.Kv
	j := json.RawMessage(`{}`)

	result, err := RawMapToKeyValue(j)

	if assert.Nil(t, err) {
		assert.ElementsMatch(t, tags, result)
	}
}

func TestRawMapToKeyValueReturnsKeyValues(t *testing.T) {
	secret1 := common.Kv{Key: "key1", Value: "value1"}
	secret2 := common.Kv{Key: "key2", Value: "value2"}
	tags := []*common.Kv{&secret1, &secret2}

	j := json.RawMessage(`{"key1":"value1","key2":"value2"}`)

	result, err := RawMapToKeyValue(j)

	if assert.Nil(t, err) {
		assert.ElementsMatch(t, tags, result)
	}
}
