package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/olivere/elastic/v7"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/require"
)

func TestIngestionPipelineControlIndex(t *testing.T) {
	index := "comp-1-control*"
	suit := NewGlobalSuite()
	require.NotNil(t, suit)

	input := "../ingest/examples/compliance-success-tiny-report.json"
	err := suit.ingestReport(input, func(r *compliance.Report) {
		r.NodeUuid = newUUID()
		r.ReportUuid = newUUID()
	})
	require.NoError(t, err)

	logrus.Info("Parsing...")
	time.Sleep(time.Second * 60)

	// Get the indexes starting with `comp-1-control`
	ifExists := suit.indexExists(index)
	require.True(t, ifExists)

	// Check document's count
	query := elastic.NewMatchAllQuery()
	searchResult, err := suit.elasticClient.Search().
		Query(query).
		Index(index).
		Do(context.Background())

	require.NoError(t, err)
	require.NotNil(t, searchResult)
	require.Greater(t, searchResult.TotalHits(), 0, "Search result total hits shouldn't be 0")
}
