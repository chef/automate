package integration_test

import (
	"context"
	"encoding/json"
	"io/ioutil"
	"os"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/migrations"
	"github.com/olivere/elastic/v7"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/require"
)

func TestMarkDayLatestToFalse(t *testing.T) {
	// index := "comp-1*"
	suit := NewGlobalSuite()
	require.NotNil(t, suit)
	nodes, err := migrations.GetNodesDayLatestTrue(suit.elasticClient, context.Background())
	require.NoError(t, err)
	require.NotEmpty(t, nodes)
	require.NotNil(t, nodes)
}

func TestIngestionPipelineControlIndex(t *testing.T) {
	index := "comp-1-control*"
	suit := NewGlobalSuite()
	require.NotNil(t, suit)

	input := "../ingest/examples/compliance-success-tiny-report.json"
	mapIndex := "../ingest/examples/mapping-comp-1-control-2018.10.25.json"
	err := suit.ingestReport(input, func(r *compliance.Report) {
		r.NodeUuid = newUUID()
		r.ReportUuid = newUUID()
	})
	require.NoError(t, err)

	logrus.Info("Parsing...")
	time.Sleep(time.Second * 120)

	// Get the indexes starting with `comp-1-control`
	ifExists := suit.indexExists(index)
	require.True(t, ifExists)

	// Check document's count
	query := elastic.NewMatchAllQuery()
	searchResult, err := suit.elasticClient.Search().
		Index(index).
		Query(query).
		Pretty(true).
		Do(context.Background())

	require.NoError(t, err)
	require.NotNil(t, searchResult)
	require.Greater(t, searchResult.TotalHits(), int64(0), "Total hits cannot be 0 or less")

	// Check mapping
	mappings, err := suit.elasticClient.GetMapping().
		Index(index).
		Pretty(true).
		Do(context.Background())
	require.NoError(t, err)
	require.NotNil(t, mappings)
	require.NotEmpty(t, mappings, "mappings cannot be empty")

	file, err := os.Open(mapIndex)
	require.NoError(t, err)

	byteValue, err := ioutil.ReadAll(file)
	require.NoError(t, err)

	var result map[string]interface{}
	err = json.Unmarshal(byteValue, &result)
	require.NoError(t, err)

	require.Equal(t, result, mappings, "result and mapping cannot be different")
}
