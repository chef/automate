package integration_test

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"log"
	"net"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/grpc/auth_context"
)

func TestReportingServerExport(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})

	/*
	   Create a struct suitable for setting up a streaming server connection in a test environment.

	   Most of this connection setup code was copied from two examples:
	   * https://stackoverflow.com/questions/42102496/testing-a-grpc-service/52080545#52080545
	   * https://gitlab.cncf.ci/grpc/grpc-go/blob/c7e2c00ed1df1690ec11b9984b3fa8da1bfafbdd/test/end2end_test.go#L4451
	*/

	// I'm not sure the significance of this value, this was copy-pasted from an example.
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

	client := reporting.NewReportingServiceClient(conn)

	nodeIds := []string{newUUID(), newUUID(), newUUID(), newUUID(), newUUID(), newUUID()}

	reports := []*relaxting.ESInSpecReport{
		{
			NodeID:   nodeIds[0],
			Projects: []string{},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[1],
			Projects: []string{"project1"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[2],
			Projects: []string{"project1", "project2"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[3],
			Projects: []string{"project2"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[4],
			Projects: []string{"project2", "project3"},
			EndTime:  time.Now(),
		},
		{
			NodeID:   nodeIds[5],
			Projects: []string{"project3"},
			EndTime:  time.Now(),
		},
	}

	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, len(reports))

	successCases := []struct {
		description     string
		allowedProjects []string
		expectedIds     []string
	}{
		{
			description:     "reporting_server_export_test.go => Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     reportIds,
		},
		{
			description:     "reporting_server_export_test.go => Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     reportIds[1:3],
		},
		{
			description:     "reporting_server_export_test.go => Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     reportIds[1:5],
		},
		{
			description:     "reporting_server_export_test.go => Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "reporting_server_export_test.go => Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:3],
		},
		{
			description:     "reporting_server_export_test.go => Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:5],
		},
		{
			description:     "reporting_server_export_test.go => Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
		{
			description:     "reporting_server_export_test.go => Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := auth_context.NewOutgoingContext(contextWithProjects(test.allowedProjects))
			response, err := client.Export(ctx, &reporting.Query{Type: "json"})

			assert.NoError(t, err)
			require.NotNil(t, response)

			data := make([]byte, 0)

			//the gateway wraps the response in [] because it's a json array
			//since we are not using the gateway in this test, we need to do that wrapping
			//here's the '[' (open wrapper)
			data = append([]byte("["), data...)
			for {
				tdata, err := response.Recv()
				if err != nil && err == io.EOF {
					data = append(data, tdata.GetContent()...)
					break
				}

				require.NoError(t, err)
				data = append(data, tdata.GetContent()...)
			}
			//and here's the ']' (close wrapper)
			data = append(data, []byte("]")...)

			actualIds := make([]string, 0)
			dec := json.NewDecoder(bytes.NewReader(data))
			for dec.More() {
				var reports []reporting.Report
				err := dec.Decode(&reports)
				require.NoError(t, err)

				for _, report := range reports {
					actualIds = append(actualIds, report.Id)
				}

			}

			assert.ElementsMatch(t, test.expectedIds, actualIds)
		})
	}
}
