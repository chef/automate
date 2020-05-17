package pgdb

import (
	"testing"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/stretchr/testify/assert"
)

func TestValidateTagsReturnsFalseForBadString(t *testing.T) {
	kv := &common.Kv{
		Key:   "test.thing-yz.us-east",
		Value: "test-bla.bla.x-y",
	}
	assert.Equal(t, true, validateTag(kv))

	kv = &common.Kv{
		Key:   "test.thing-yz.us-east",
		Value: "Dot.Comma,Big;\"Trouble",
	}
	assert.Equal(t, false, validateTag(kv))

	kv = &common.Kv{
		Key:   "test.thing-yz.us-east",
		Value: "Dot.Comma,Big;'Trouble",
	}
	assert.Equal(t, true, validateTag(kv))
}
