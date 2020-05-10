package pgdb

import (
	"testing"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/stretchr/testify/assert"
)

func TestFormatTagsRemovesTroublesomeCharsFromTags(t *testing.T) {
	kv := &common.Kv{
		Key:   "test.thing-yz.us-east",
		Value: "test-bla.bla.x-y",
	}
	res := formatTag(kv)
	assert.Equal(t, kv, &res)

	kv = &common.Kv{
		Key:   "test.thing-yz.us-east",
		Value: "Dot.Comma,Big;\"Trouble",
	}
	res = formatTag(kv)
	assert.Equal(t, &common.Kv{Key: "test.thing-yz.us-east", Value: "Dot.Comma,Big;Trouble"}, &res)
}
