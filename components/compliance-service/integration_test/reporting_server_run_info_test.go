package integration_test

import (
	"context"
	"encoding/json"
	"log"
	"net"
	"testing"
	"time"

	"gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestReportingServerRunInfo(t *testing.T) {
	backend := &relaxting.ES2Backend{ESUrl: elasticsearchUrl}
	server := reportingServer.New(backend)

	lis := bufconn.Listen(1024 * 1024)
	s := grpc.NewServer()
	reporting.RegisterReportingServiceServer(s, server)

	go func() {
		if err := s.Serve(lis); err != nil {
			log.Fatalf("Server exited with error: %v", err)
		}
	}()

	dialer := func(string, time.Duration) (net.Conn, error) { return lis.Dial() }

	conn, err := grpc.DialContext(context.Background(), "bufnet", grpc.WithDialer(dialer), grpc.WithInsecure())
	defer conn.Close()
	require.NoError(t, err)

	nodeIds := []string{newUUID(), newUUID()}

	now := time.Now().UTC().Truncate(60 * time.Second)
	then := now.Add(time.Duration(-1) * time.Minute)
	reports := []*relaxting.ESInSpecReport{
		{
			NodeID:  nodeIds[0],
			EndTime: then, // for this node, since it was only run once, last_run will match first_run
		},
		{
			NodeID:  nodeIds[1],
			EndTime: then,
		},
		{
			NodeID:  nodeIds[1], //put this one in again and see that last_run for this run_info no longer matches first_run
			EndTime: now,
		},
	}

	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	_, err = suite.InsertComplianceRunInfos(reports)
	require.NoError(t, err)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, len(reports))

	successCases := []struct {
		description      string
		expectedNodeId   string
		expectedFirstRun time.Time
		expectedLastRun  time.Time
	}{
		{
			description:      "reporting_server_run_info_test.go => node run once has same first_run and last_run",
			expectedNodeId:   nodeIds[0],
			expectedFirstRun: then,
			expectedLastRun:  then,
		},
		{
			description:      "reporting_server_run_info_test.go => node run twice has different first_run and last_run",
			expectedNodeId:   nodeIds[1],
			expectedFirstRun: then,
			expectedLastRun:  now,
		},
	}

	esClient, err := backend.ES2Client()
	require.NoError(t, err)

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			boolQuery := elastic.NewBoolQuery().Must(elastic.NewTermQuery("node_uuid", test.expectedNodeId))
			searchResult, err := esClient.
				Search(mappings.IndexNameComplianceRunInfo).
				SearchSource(elastic.NewSearchSource().Query(boolQuery)).
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
					var item relaxting.ESComplianceRunInfo
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

func TestReportingServerRunInfoLoadSixteenDaysOfData(t *testing.T) {
	//clear out the docs
	defer suite.DeleteAllDocuments()

	backend := &relaxting.ES2Backend{ESUrl: elasticsearchUrl}
	server := reportingServer.New(backend)

	lis := bufconn.Listen(1024 * 1024)
	s := grpc.NewServer()
	reporting.RegisterReportingServiceServer(s, server)

	go func() {
		if err := s.Serve(lis); err != nil {
			log.Fatalf("Server exited with error: %v", err)
		}
	}()

	dialer := func(string, time.Duration) (net.Conn, error) { return lis.Dial() }

	conn, err := grpc.DialContext(context.Background(), "bufnet", grpc.WithDialer(dialer), grpc.WithInsecure())
	defer conn.Close()
	require.NoError(t, err)

	now := time.Now().UTC().Truncate(60 * time.Second)
	reports := []*relaxting.ESInSpecReport{}
	summaries := []*relaxting.ESInSpecSummary{}

	seventeenDaysAgo := now.Add(time.Duration(-24*17) * time.Hour)
	//put in 2 reports and summaries every day for the past 16 days starting with yesterday
	for i := 1; i <= 16; i++ {
		dayOfReport := seventeenDaysAgo.Add(time.Duration(24*i) * time.Hour)
		reports = append(reports,
			&relaxting.ESInSpecReport{
				NodeID:  newUUID(),
				EndTime: dayOfReport,
			})
		reports = append(reports,
			&relaxting.ESInSpecReport{
				NodeID:  newUUID(),
				EndTime: dayOfReport,
			})

		summaries = append(summaries,
			&relaxting.ESInSpecSummary{
				NodeID:  newUUID(),
				EndTime: dayOfReport,
			})
		summaries = append(summaries,
			&relaxting.ESInSpecSummary{
				NodeID:  newUUID(),
				EndTime: dayOfReport,
			})
	}

	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	_, err = suite.InsertInspecSummaries(summaries)
	require.NoError(t, err)

	_, err = suite.InsertComplianceRunInfos(reports)
	require.NoError(t, err)

	rerunReports := []*relaxting.ESInSpecReport{}
	rerunSummaries := []*relaxting.ESInSpecSummary{}

	//rerun one of the two run nodes for each day on days 3, 4 and 5 and rerun on the same days, just a minute later
	//we do two nodes a day above so
	//day 3 runs occupy index 4 and 5
	//day 4 runs occupy index 6 and 7
	//day 5 runs occupy index 8 and 9
	for i := 4; i < 10; i += 2 {
		dayOfReport := reports[i].EndTime.Add(time.Duration(1) * time.Minute)
		rerunReports = append(rerunReports,
			&relaxting.ESInSpecReport{
				NodeID:  reports[i].NodeID,
				EndTime: dayOfReport,
			})

		rerunSummaries = append(rerunSummaries,
			&relaxting.ESInSpecSummary{
				NodeID:  reports[i].NodeID,
				EndTime: dayOfReport,
			})
	}
	reportIds, err = suite.InsertInspecReports(rerunReports)
	require.NoError(t, err)

	summaryIds, err := suite.InsertInspecSummaries(rerunSummaries)
	require.NoError(t, err)

	runInfoIds, err := suite.InsertComplianceRunInfos(rerunReports)
	require.NoError(t, err)

	require.Len(t, reportIds, len(rerunReports))
	require.Len(t, summaryIds, len(rerunSummaries))
	require.Len(t, runInfoIds, len(rerunReports))

	rerunSecondReportFromSixteenDaysAgoToday := []*relaxting.ESInSpecReport{}
	rerunSummariesFromSixteenDaysAgoToday := []*relaxting.ESInSpecSummary{}

	//rerun the second report from 16 days ago.. rerun it today
	rerunSecondReportFromSixteenDaysAgoToday = append(rerunSecondReportFromSixteenDaysAgoToday,
		&relaxting.ESInSpecReport{
			NodeID:  reports[1].NodeID,
			EndTime: now,
		})

	rerunSummariesFromSixteenDaysAgoToday = append(rerunSummariesFromSixteenDaysAgoToday,
		&relaxting.ESInSpecSummary{
			NodeID:  reports[1].NodeID,
			EndTime: now,
		})

	reportIds, err = suite.InsertInspecReports(rerunSecondReportFromSixteenDaysAgoToday)
	require.NoError(t, err)

	summaryIds, err = suite.InsertInspecSummaries(rerunSummariesFromSixteenDaysAgoToday)
	require.NoError(t, err)

	runInfoIds, err = suite.InsertComplianceRunInfos(rerunSecondReportFromSixteenDaysAgoToday)
	require.NoError(t, err)

	require.Len(t, reportIds, len(rerunSecondReportFromSixteenDaysAgoToday))
	require.Len(t, summaryIds, len(rerunSummariesFromSixteenDaysAgoToday))
	require.Len(t, runInfoIds, len(rerunSecondReportFromSixteenDaysAgoToday))
}
