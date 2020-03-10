package mappings

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDateScan(t *testing.T) {
	now := time.Now().UTC()

	indexName := ComplianceRepDate.IndexTimeseriesFmt(now)

	indexDay, err := ComplianceRepDate.IndexTimeseriesScanDate(indexName)
	require.NoError(t, err)

	assert.Equal(t, time.UTC, indexDay.Location())
	assert.Equal(t, now.Year(), indexDay.Year())
	assert.Equal(t, now.Month(), indexDay.Month())
	assert.Equal(t, now.Day(), indexDay.Day())
}
