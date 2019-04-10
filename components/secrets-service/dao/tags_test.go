package dao

import (
	"encoding/json"
	"testing"

	"github.com/chef/automate/api/external/secrets"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestKeyValueToRawMapReturnsEmptyMessageWhenArrayEmpty(t *testing.T) {
	var tags []*secrets.Kv
	var bytes []byte
	var err error
	var j json.RawMessage

	j, err = KeyValueToRawMap(tags)
	require.NoError(t, err)

	bytes, _ = j.MarshalJSON()

	assert.Equal(t, "{}", string(bytes))
}

func TestKeyValueToRawMapReturnsJsonMessage(t *testing.T) {
	secret1 := secrets.Kv{Key: "key1", Value: "value1"}
	secret2 := secrets.Kv{Key: "key2", Value: "value2"}
	tags := []*secrets.Kv{&secret1, &secret2}

	var bytes []byte
	var err error
	var j json.RawMessage

	j, err = KeyValueToRawMap(tags)
	require.NoError(t, err)

	bytes, _ = j.MarshalJSON()

	assert.Equal(t, "{\"key1\":\"value1\",\"key2\":\"value2\"}", string(bytes))
}

func TestRawMapToKeyValueReturnsEmptyTagsWhenJsonEmpty(t *testing.T) {
	var tags []*secrets.Kv
	j := json.RawMessage(`{}`)

	result, err := RawMapToKeyValue(j)

	if assert.Nil(t, err) {
		assert.ElementsMatch(t, tags, result)
	}
}

func TestRawMapToKeyValueReturnsKeyValues(t *testing.T) {
	secret1 := secrets.Kv{Key: "key1", Value: "value1"}
	secret2 := secrets.Kv{Key: "key2", Value: "value2"}
	tags := []*secrets.Kv{&secret1, &secret2}

	j := json.RawMessage(`{"key1":"value1","key2":"value2"}`)

	result, err := RawMapToKeyValue(j)

	if assert.Nil(t, err) {
		assert.ElementsMatch(t, tags, result)
	}
}
