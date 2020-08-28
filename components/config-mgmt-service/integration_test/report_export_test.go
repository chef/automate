package integration_test

import (
	"context"
	"fmt"
	"io"
	"net"
	"testing"
	"time"

	"github.com/buger/jsonparser"
	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	api "github.com/chef/automate/api/interservice/cfgmgmt/service"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/config-mgmt-service/backend/elastic"
	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/components/config-mgmt-service/grpcserver"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/timestamp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"
)

func TestReportExportProjectFilters(t *testing.T) {
	mockConn := reportExportSetup(t)
	defer mockConn.Close()
	client := api.NewCfgMgmtServiceClient(mockConn)

	cases := []struct {
		description     string
		runsProjects    []string
		allowedProjects []string
		runIsReturned   bool
	}{
		{
			description:     "Run is returned when the one run project matches the one allowed project",
			runsProjects:    []string{"one"},
			allowedProjects: []string{"one"},
			runIsReturned:   true,
		},
		{
			description:     "No run is returned when the one run project does not match the one allowed project",
			runsProjects:    []string{"one"},
			allowedProjects: []string{"two"},
			runIsReturned:   false,
		},
		{
			description:     "Run is returned when the one run project matches one of the two allowed projects",
			runsProjects:    []string{"one"},
			allowedProjects: []string{"one", "two"},
			runIsReturned:   true,
		},
		{
			description:     "Run is returned when both of the two run projects match both of the allowed projects",
			runsProjects:    []string{"one", "two"},
			allowedProjects: []string{"one", "two"},
			runIsReturned:   true,
		},
		{
			description:     "Run is returned when the run does not have an assigned project and the allowed project is unassigned",
			runsProjects:    []string{},
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			runIsReturned:   true,
		},
		{
			description:     "Run is returned when the run does not have an assigned project and no allowed project are provided",
			runsProjects:    []string{},
			allowedProjects: []string{},
			runIsReturned:   true,
		},
		{
			description:     "No run is returned when the run does not have an assigned project and one allowed project",
			runsProjects:    []string{},
			allowedProjects: []string{"one"},
			runIsReturned:   false,
		},
		{
			description:     "No run is returned when the run has one project and the allowed project is unassigned",
			runsProjects:    []string{"one"},
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			runIsReturned:   false,
		},
		{
			description:     "Run is returned when the run has a matching project with one of the allowed project with unassigned",
			runsProjects:    []string{"one"},
			allowedProjects: []string{authzConstants.UnassignedProjectID, "one"},
			runIsReturned:   true,
		},
		{
			description:     "Run is returned when the run has one project and all projects are allowed",
			runsProjects:    []string{"one"},
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			runIsReturned:   true,
		},
		{
			description:     "Run is returned when the run does not have a project and all projects are allowed",
			runsProjects:    []string{},
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			runIsReturned:   true,
		},
		{
			description:     "Run is returned when the run has a matching project with one of several allowed projects",
			runsProjects:    []string{"project7"},
			allowedProjects: []string{"project3", "project9", "project7", "project6"},
			runIsReturned:   true,
		},
		{
			description:     "No run is returned when the one run's project does not match several allowed projects",
			runsProjects:    []string{"project12"},
			allowedProjects: []string{"project3", "project9", "project7", "project6"},
			runIsReturned:   false,
		},
		{
			description:     "Run is returned when one of several run's projects matches the one project allowed",
			runsProjects:    []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project3"},
			runIsReturned:   true,
		},
		{
			description:     "No run is returned when none of the several run's projects match the one allowed project",
			runsProjects:    []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project12"},
			runIsReturned:   false,
		},
		{
			description:     "Run is returned when one of the several run's projects matches one of the several allowed projects",
			runsProjects:    []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project1", "project2", "project7", "project12"},
			runIsReturned:   true,
		},
		{
			description:     "No run is returned when none of the several run's projects matches none of the several allowed projects",
			runsProjects:    []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project1", "project2", "project11", "project12"},
			runIsReturned:   false,
		},
		{
			description:     "Run is returned when several of the run's projects match all of the several allowed projects",
			runsProjects:    []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project3", "project9", "project7", "project6"},
			runIsReturned:   true,
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {

			// associated run's node
			node := iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
				},
				Projects: test.runsProjects,
				Exists:   true,
			}

			run := iBackend.Run{
				NodeInfo:  node.NodeInfo,
				RunID:     "run_id",
				StartTime: time.Now().Add(-time.Minute),
				EndTime:   time.Now(),
			}

			// Ingest run and associated node
			suite.IngestNodes([]iBackend.Node{node})
			suite.IngestRuns([]iBackend.Run{run})
			defer suite.DeleteAllDocuments()
			ctx := auth_context.NewOutgoingContext(contextWithProjects(test.allowedProjects))
			response, err := client.ReportExport(ctx, &request.ReportExport{
				OutputType: "json",
				NodeId:     node.EntityUuid,
			})
			assert.NoError(t, err)
			require.NotNil(t, response)

			data := make([]byte, 0)
			for {
				tdata, err := response.Recv()
				if err != nil && err == io.EOF {
					data = append(data, tdata.GetContent()...)
					break
				}

				require.NoError(t, err)
				data = append(data, tdata.GetContent()...)
			}

			actualNumberOfRuns := 0
			jsonparser.ArrayEach(data, func(run []byte, _ jsonparser.ValueType, _ int, err error) {
				require.NoError(t, err)
				actualNumberOfRuns++
			})

			if test.runIsReturned {
				assert.Equal(t, 1, actualNumberOfRuns,
					"One run should have been returned not %d runs", actualNumberOfRuns)
			} else {
				assert.Equal(t, 0, actualNumberOfRuns,
					"No runs should have been returned not %d runs", actualNumberOfRuns)
			}
		})
	}
}

func createTimestamp(t *testing.T, date time.Time) *timestamp.Timestamp {
	timestamp, err := ptypes.TimestampProto(time.Now())
	require.NoError(t, err)

	return timestamp
}
func TestReportExportFilters(t *testing.T) {
	mockConn := reportExportSetup(t)
	defer mockConn.Close()
	client := api.NewCfgMgmtServiceClient(mockConn)

	cases := []struct {
		description    string
		runs           []iBackend.Run
		reportExport   *request.ReportExport
		expectedRunIDs []string
	}{
		{
			description: "One run added and returned with no filters",
			runs: []iBackend.Run{
				{
					RunID:     "run_id",
					StartTime: time.Now().Add(-time.Minute),
					EndTime:   time.Now(),
				},
			},
			reportExport:   &request.ReportExport{},
			expectedRunIDs: []string{"run_id"},
		},
		{
			description: "Two runs added and two returned with no filters",
			runs: []iBackend.Run{
				{
					RunID:     "run_1",
					StartTime: time.Now().Add(-time.Minute),
					EndTime:   time.Now(),
				},
				{
					RunID:     "run_2",
					StartTime: time.Now().Add(-time.Minute),
					EndTime:   time.Now(),
				},
			},
			reportExport:   &request.ReportExport{},
			expectedRunIDs: []string{"run_1", "run_2"},
		},
		{
			description: "One run added and returned with date filters including run",
			runs: []iBackend.Run{
				{
					RunID:     "run_id",
					StartTime: time.Now().Add(-time.Minute),
					EndTime:   time.Now(),
				},
			},
			reportExport: &request.ReportExport{
				Start: createTimestamp(t, time.Now().Add(-time.Hour)),
				End:   createTimestamp(t, time.Now().Add(time.Hour)),
			},
			expectedRunIDs: []string{"run_id"},
		},
		{
			description: "One run added and zero returned with date filters excluding run",
			runs: []iBackend.Run{
				{
					RunID:     "run_id",
					StartTime: time.Now().Add(time.Hour * -12),
					EndTime:   time.Now().Add(time.Hour * -11),
				},
			},
			reportExport: &request.ReportExport{
				Start: createTimestamp(t, time.Now().Add(-time.Hour)),
				End:   createTimestamp(t, time.Now().Add(time.Hour)),
			},
			expectedRunIDs: []string{},
		},
		{
			description: "Two runs added and one returned with date filters including one run",
			runs: []iBackend.Run{
				{
					RunID:     "run_1",
					StartTime: time.Now().Add(time.Minute * -1),
					EndTime:   time.Now(),
				},
				{
					RunID:     "run_2",
					StartTime: time.Now().Add(time.Hour * -48),
					EndTime:   time.Now().Add(time.Hour * -47),
				},
			},
			reportExport: &request.ReportExport{
				Start: createTimestamp(t, time.Now().Add(-time.Hour)),
				End:   createTimestamp(t, time.Now().Add(time.Hour)),
			},
			expectedRunIDs: []string{"run_1"},
		},
		{
			description: "One run added and one returned with status filter including run",
			runs: []iBackend.Run{
				{
					RunID:     "run_id",
					StartTime: time.Now().Add(-time.Minute),
					EndTime:   time.Now(),
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
			},
			reportExport: &request.ReportExport{
				Filter: []string{"status:failure"},
			},
			expectedRunIDs: []string{"run_id"},
		},
		{
			description: "Two runs added and one returned with status filter including only one run",
			runs: []iBackend.Run{
				{
					RunID:     "run_1",
					StartTime: time.Now().Add(-time.Minute),
					EndTime:   time.Now(),
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					RunID:     "run_2",
					StartTime: time.Now().Add(-time.Minute),
					EndTime:   time.Now(),
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
			},
			reportExport: &request.ReportExport{
				Filter: []string{"status:success"},
			},
			expectedRunIDs: []string{"run_2"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("filter: %s", test.description), func(t *testing.T) {

			// associated run's node
			node := iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: newUUID(),
				},
				Exists: true,
			}

			for index := range test.runs {
				test.runs[index].NodeInfo.EntityUuid = node.NodeInfo.EntityUuid
			}

			test.reportExport.OutputType = "json"
			test.reportExport.NodeId = node.EntityUuid

			// Ingest runs and associated node
			suite.IngestNodes([]iBackend.Node{node})
			suite.IngestRuns(test.runs)
			defer suite.DeleteAllDocuments()

			response, err := client.ReportExport(context.Background(), test.reportExport)
			assert.NoError(t, err)
			require.NotNil(t, response)

			data := make([]byte, 0)
			for {
				tdata, err := response.Recv()
				if err != nil && err == io.EOF {
					data = append(data, tdata.GetContent()...)
					break
				}

				require.NoError(t, err)
				data = append(data, tdata.GetContent()...)
			}

			actualRunIDs := make([]string, 0)
			jsonparser.ArrayEach(data, func(run []byte, _ jsonparser.ValueType, _ int, err error) {
				require.NoError(t, err)
				runID, err := jsonparser.GetString(run, "run_id")
				require.NoError(t, err)
				actualRunIDs = append(actualRunIDs, runID)
			})

			assert.ElementsMatch(t, test.expectedRunIDs, actualRunIDs)
		})
	}
}

func reportExportSetup(t *testing.T) *grpc.ClientConn {
	esClient := elastic.New(elasticsearchUrl)
	c := config.New(esClient)
	server := grpcserver.NewCfgMgmtServer(c)

	lis := bufconn.Listen(1024 * 1024)
	s := grpc.NewServer()
	api.RegisterCfgMgmtServiceServer(s, server)

	go func() {
		if err := s.Serve(lis); err != nil {
			t.Fatalf("Server exited with error: %v", err)
		}
	}()

	dialer := func(string, time.Duration) (net.Conn, error) { return lis.Dial() }

	conn, err := grpc.DialContext(context.Background(), "bufnet", grpc.WithDialer(dialer), grpc.WithInsecure())
	require.NoError(t, err)

	return conn
}
