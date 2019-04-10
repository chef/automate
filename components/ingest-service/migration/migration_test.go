package migration

import (
	"testing"

	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/stretchr/testify/assert"
)

func TestRenameFromInsightsIndexName(t *testing.T) {
	assert.Equal(t, a2CurrentNodeStateIndex, mappings.NodeState.Index)
}
