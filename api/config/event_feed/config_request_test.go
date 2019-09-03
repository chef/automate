package event_feed

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestDeprecations(t *testing.T) {
	c := NewConfigRequest()
	c.V1.Sys.Service.PurgeEventFeedAfterDays = w.Int32(7)
	err := c.Validate()
	require.Error(t, err)
	require.Contains(t, err.Error(), "'event_feed_service.v1.sys.service.purge_event_feed_after_days' has been deprecated")
}
