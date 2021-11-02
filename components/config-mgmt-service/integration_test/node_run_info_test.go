package integration_test

import (
	"context"
	"encoding/json"

	//reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"

	"testing"
	"time"

	iBackend "github.com/chef/automate/components/ingest-service/backend"

	"gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNodeRunInfoLoadSixteenDaysOfData(t *testing.T) {
	//clear out the docs
	suite.DeleteAllDocuments()

	now := time.Now().UTC().Truncate(60 * time.Second)
	nodes := []iBackend.Node{}
	runs := []iBackend.Run{}

	seventeenDaysAgo := now.Add(time.Duration(-24*17) * time.Hour)
	//put in 2 nodes and runs every day for the past 16 days starting with yesterday
	for i := 1; i <= 16; i++ {
		dayOfReport := seventeenDaysAgo.Add(time.Duration(24*i) * time.Hour)

		uid := newUUID()
		nodes = append(nodes,
			iBackend.Node{
				Exists: true,
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: uid,
				},
			})

		runs = append(runs,
			newIngestRun(newUUID(), "success",
				dayOfReport.Add(-1*time.Minute).Format(time.RFC3339),
				dayOfReport.Format(time.RFC3339)),
		)

		uid = newUUID()
		nodes = append(nodes,
			iBackend.Node{
				Exists: true,
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
				},
			})

		runs = append(runs,
			newIngestRun(newUUID(), "success",
				dayOfReport.Add(-1*time.Minute).Format(time.RFC3339),
				dayOfReport.Format(time.RFC3339)),
		)
	}

	suite.IngestNodes(nodes)
	suite.IngestRuns(runs)
	suite.IngestNodeRunDateInfo(runs)

	rurunRuns := []iBackend.Run{}

	//rerun one of the two run nodes for each day on days 3, 4 and 5 and rerun on the same days, just a minute later
	//we do two nodes a day above so
	//day 3 runs occupy index 4 and 5
	//day 4 runs occupy index 6 and 7
	//day 5 runs occupy index 8 and 9
	for i := 4; i < 10; i += 2 {
		dayOfReport := runs[i].EndTime.Add(time.Duration(1) * time.Minute)
		rurunRuns = append(rurunRuns,
			newIngestRun(runs[i].EntityUuid, "success",
				dayOfReport.Add(-1*time.Minute).Format(time.RFC3339),
				dayOfReport.Format(time.RFC3339)),
		)
	}

	suite.IngestRuns(rurunRuns)
	suite.IngestNodeRunDateInfo(rurunRuns)

	rerunSecondReportFromSixteenDaysAgoToday := []iBackend.Run{}

	//rerun the second report from 16 days ago.. rerun it today
	rerunSecondReportFromSixteenDaysAgoToday = append(rerunSecondReportFromSixteenDaysAgoToday,
		newIngestRun(runs[1].EntityUuid, "success",
			now.Add(-1*time.Minute).Format(time.RFC3339),
			now.Format(time.RFC3339)),
	)

	suite.IngestRuns(rerunSecondReportFromSixteenDaysAgoToday)
	suite.IngestNodeRunDateInfo(rerunSecondReportFromSixteenDaysAgoToday)

}

func TestNodeRunInfoDateTime(t *testing.T) {

	nodeIds := []string{newUUID(), newUUID()}

	now := time.Now().UTC().Truncate(60 * time.Second)
	then := now.Add(time.Duration(-1) * time.Minute)
	nodes := []iBackend.Node{
		{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeIds[0],
			},
		},
		{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeIds[1],
			},
		},
	}

	runs := []iBackend.Run{
		newIngestRun(nodeIds[0], "success",
			then.Add(-1*time.Minute).Format(time.RFC3339),
			then.Format(time.RFC3339)),

		newIngestRun(nodeIds[1], "success",
			then.Add(-1*time.Minute).Format(time.RFC3339),
			then.Format(time.RFC3339)),

		newIngestRun(nodeIds[1], "success",
			now.Add(-1*time.Minute).Format(time.RFC3339),
			now.Format(time.RFC3339)),
	}

	suite.IngestNodes(nodes)
	suite.IngestRuns(runs)
	suite.IngestNodeRunDateInfo(runs)

	//defer suite.DeleteAllDocuments()

	successCases := []struct {
		description      string
		expectedNodeId   string
		expectedFirstRun time.Time
		expectedLastRun  time.Time
	}{
		{
			description:      "node_run_info_test.go => node run once has same first_run and last_run",
			expectedNodeId:   nodeIds[0],
			expectedFirstRun: then,
			expectedLastRun:  then,
		},
		{
			description:      "node_run_info_test.go => node run twice has different first_run and last_run",
			expectedNodeId:   nodeIds[1],
			expectedFirstRun: then,
			expectedLastRun:  now,
		},
	}

	esClient := suite.client
	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			boolQuery := elastic.NewBoolQuery().Must(elastic.NewTermQuery("node_uuid", test.expectedNodeId))
			searchSource := elastic.NewSearchSource().Query(boolQuery)

			//give it time to refresh in es
			time.Sleep(1 * time.Second)

			searchResult, err := esClient.
				Search(mappings.IndexNameNodeRunInfo).
				SearchSource(searchSource).
				FilterPath(
					"took",
					"hits.total",
					"hits.hits._id",
					"hits.hits._source",
				).
				Do(context.Background())

			require.NoError(t, err)

			if searchResult.TotalHits() > 0 && searchResult.Hits.TotalHits > 0 {
				for _, hit := range searchResult.Hits.Hits {
					var item iBackend.NodeRunDateInfo

					if hit.Source != nil {
						err := json.Unmarshal(*hit.Source, &item)
						require.NoError(t, err)
						assert.Equal(t, test.expectedNodeId, item.NodeID)
						assert.Equal(t, test.expectedFirstRun, item.FirstRun)
						assert.Equal(t, test.expectedLastRun, item.LastRun)
					}
				}
			}
		})
	}
}
