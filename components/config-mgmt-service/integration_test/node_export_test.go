package integration_test

import (
	// "bytes"
	"context"
	// "encoding/json"
	"fmt"
	"github.com/buger/jsonparser"
	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	api "github.com/chef/automate/api/interservice/cfgmgmt/service"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/backend/elastic"
	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/components/config-mgmt-service/grpcserver"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"
	"io"
	"net"
	"testing"
	"time"
)

func TestNodeExportProjectFilters(t *testing.T) {
	esClient := elastic.New(elasticsearchUrl)
	c := config.New(esClient)
	server := grpcserver.NewCfgMgmtServer(c)

	lis := bufconn.Listen(1024 * 1024)
	s := grpc.NewServer()
	api.RegisterCfgMgmtServer(s, server)

	go func() {
		if err := s.Serve(lis); err != nil {
			t.Fatalf("Server exited with error: %v", err)
		}
	}()

	dialer := func(string, time.Duration) (net.Conn, error) { return lis.Dial() }

	conn, err := grpc.DialContext(context.Background(), "bufnet", grpc.WithDialer(dialer), grpc.WithInsecure())
	defer conn.Close()
	require.NoError(t, err)

	client := api.NewCfgMgmtClient(conn)

	cases := []struct {
		description       string
		nodes             []iBackend.Node
		allowedProjects   []string
		expectedNodeNames []string
		filter            []string
	}{
		{
			description:       "No nodes with a project tag",
			nodes:             []iBackend.Node{},
			allowedProjects:   []string{"one"},
			expectedNodeNames: []string{},
		},
		{
			description: "One node with a project matching requested projects",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"one"},
				},
			},
			allowedProjects:   []string{"one"},
			expectedNodeNames: []string{"1"},
		},
		{
			description: "Two nodes matching on the same project tag",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			allowedProjects:   []string{"one"},
			expectedNodeNames: []string{"1", "2"},
		},
		{
			description: "Two nodes matching with two project tags",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"one"},
				},
			},
			allowedProjects:   []string{"one", "two"},
			expectedNodeNames: []string{"1", "2"},
		},
		{
			description: "Two nodes, one matching",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			allowedProjects:   []string{"one"},
			expectedNodeNames: []string{"2"},
		},
		{
			description: "Matching all",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
					},
				},
			},
			allowedProjects:   []string{authzConstants.AllProjectsExternalID},
			expectedNodeNames: []string{"1", "2", "3"},
		},
		{
			description: "Match one unassigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			allowedProjects:   []string{authzConstants.UnassignedProjectID},
			expectedNodeNames: []string{"1"},
		},
		{
			description: "No unassigned; no matches",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			allowedProjects:   []string{authzConstants.UnassignedProjectID},
			expectedNodeNames: []string{},
		},
		{
			description: "Match one unassigned and one assigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two"},
				},
			},
			allowedProjects:   []string{authzConstants.UnassignedProjectID, "two"},
			expectedNodeNames: []string{"1", "2"},
		},
		{
			description: "Match all projects with status filter",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Status:   "failure",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Status:   "success",
					},
					Projects: []string{"two"},
				},
			},
			allowedProjects:   []string{authzConstants.AllProjectsExternalID},
			filter:            []string{"status:success"},
			expectedNodeNames: []string{"2"},
		},
		{
			description: "Match 'one' project with status filter",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Status:   "failure",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Status:   "success",
					},
					Projects: []string{"two"},
				},
			},
			allowedProjects:   []string{"one"},
			filter:            []string{"status:success"},
			expectedNodeNames: []string{},
		},
		{
			description: "Match unassigned projects with status filter",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Status:   "success",
					},
					Projects: []string{},
				},
			},
			allowedProjects:   []string{authzConstants.UnassignedProjectID},
			filter:            []string{"status:success"},
			expectedNodeNames: []string{"2"},
		},
		{
			description: "Node with one project matching one of several requested projects allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"project9"},
				},
			},
			allowedProjects:   []string{"project3", "project9", "project7", "project6"},
			expectedNodeNames: []string{"2"},
		},
		{
			description: "Two nodes with one project matching different ones of several requested projects",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project9"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"project3"},
				},
			},
			allowedProjects:   []string{"project3", "project9", "project7", "project6"},
			expectedNodeNames: []string{"1", "2"},
		},
		{
			description: "One node with one project not matching any of several requested projects allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project10"},
				},
			},
			allowedProjects:   []string{"project3", "project9", "project7", "project6"},
			expectedNodeNames: []string{},
		},
		{
			description: "Two nodes with neither project matching any of several requested projects allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project9"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"project10"},
				},
			},
			allowedProjects:   []string{"project3", "project4", "project7", "project6"},
			expectedNodeNames: []string{},
		},
		{
			description: "One node with several projects where one matches a single requested project allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project3", "project4", "project7", "project6"},
				},
			},
			allowedProjects:   []string{"project3"},
			expectedNodeNames: []string{"1"},
		},
		{
			description: "Two nodes with several projects where one matches a single requested project allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project3", "project4", "project7", "project6"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"project12", "project10", "project11", "project3"},
				},
			},
			allowedProjects:   []string{"project3"},
			expectedNodeNames: []string{"1", "2"},
		},
		{
			description: "Two nodes with several projects where only one node's project matches a single requested project allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project3", "project4", "project7", "project6"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"project12", "project10", "project11", "project13"},
				},
			},
			allowedProjects:   []string{"project3"},
			expectedNodeNames: []string{"1"},
		},
		{
			description: "One node with several projects where one matches one of several requested project allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project3", "project4", "project7", "project6"},
				},
			},
			allowedProjects:   []string{"project3", "project10", "project12", "project13"},
			expectedNodeNames: []string{"1"},
		},
		{
			description: "Two nodes with several projects where one matches one of several requested project allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project3", "project4", "project7", "project6"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"project13", "project14", "project17", "project16"},
				},
			},
			allowedProjects:   []string{"project3", "project10", "project12", "project13"},
			expectedNodeNames: []string{"1", "2"},
		},
		{
			description: "One node with several projects where none matches several requested project allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project3", "project4", "project7", "project6"},
				},
			},
			allowedProjects:   []string{"project14", "project10", "project12", "project13"},
			expectedNodeNames: []string{},
		},
		{
			description: "One node with several projects where two matches two of several requested project allowed",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"project3", "project10", "project7", "project6"},
				},
			},
			allowedProjects:   []string{"project3", "project10", "project12", "project13"},
			expectedNodeNames: []string{"1"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			// Adding required node data
			for index := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add nodes with projects
			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()
			ctx := auth_context.NewOutgoingContext(contextWithProjects(test.allowedProjects))
			response, err := client.NodeExport(ctx, &request.NodeExport{
				OutputType: "json",
				Filter:     test.filter,
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

			actualNodeNames := make([]string, 0)
			jsonparser.ArrayEach(data, func(node []byte, _ jsonparser.ValueType, _ int, err error) {
				require.NoError(t, err)
				nodeName, err := jsonparser.GetString(node, "name")
				require.NoError(t, err)
				actualNodeNames = append(actualNodeNames, nodeName)
			})

			assert.ElementsMatch(t, test.expectedNodeNames, actualNodeNames)
		})
	}
}

func TestNodeExportLoopBug(t *testing.T) {
	esClient := elastic.New(elasticsearchUrl)
	c := config.New(esClient)
	server := grpcserver.NewCfgMgmtServer(c)

	lis := bufconn.Listen(1024 * 1024)
	s := grpc.NewServer()
	api.RegisterCfgMgmtServer(s, server)

	go func() {
		if err := s.Serve(lis); err != nil {
			t.Fatalf("Server exited with error: %v", err)
		}
	}()

	dialer := func(string, time.Duration) (net.Conn, error) { return lis.Dial() }

	conn, err := grpc.DialContext(context.Background(), "bufnet", grpc.WithDialer(dialer), grpc.WithInsecure())
	defer conn.Close()
	require.NoError(t, err)

	client := api.NewCfgMgmtClient(conn)

	cases := []struct {
		description string
		node        iBackend.Node
		sorting     *request.Sorting
	}{
		{
			description: "node names with upper case character",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					NodeName: "Insights.A",
				},
			},
			sorting: &request.Sorting{
				Field: backend.Name,
			},
		},
		{
			description: "organization names with upper case character",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					OrganizationName: "Rangers",
				},
			},
			sorting: &request.Sorting{
				Field: backend.Organization,
			},
		},
		{
			description: "platform names with upper case character",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					Platform: "Ubuntu",
				},
			},
			sorting: &request.Sorting{
				Field: backend.Platform,
			},
		},
		{
			description: "environment names with upper case character",
			node: iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					Environment: "Dev_Backend",
				},
			},
			sorting: &request.Sorting{
				Field: backend.Environment,
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("multiple nodes with the same: %s", test.description), func(t *testing.T) {
			numberOfNodes := 5
			nodes := make([]iBackend.Node, numberOfNodes)
			// Adding required node data
			for index := range nodes {
				nodes[index].Exists = true
				nodes[index].NodeInfo = test.node.NodeInfo
				nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add nodes with projects
			suite.IngestNodes(nodes)
			defer suite.DeleteAllDocuments()

			response, err := client.NodeExport(context.Background(), &request.NodeExport{
				OutputType: "json",
				Sorting:    test.sorting,
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

			actualNumberOfNodes := 0
			jsonparser.ArrayEach(data, func(node []byte, _ jsonparser.ValueType, _ int, err error) {
				require.NoError(t, err)
				actualNumberOfNodes++
			})

			assert.Equal(t, numberOfNodes, actualNumberOfNodes)
		})
	}
}
