package integration_test

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	apiReq "github.com/chef/automate/api/external/cfgmgmt/request"
	external_response "github.com/chef/automate/api/external/cfgmgmt/response"
	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	iBackend "github.com/chef/automate/components/ingest-service/backend"

	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	gp "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNodesReturnsEmptyList(t *testing.T) {
	// rpc GetNodes (request.Nodes) returns (google.protobuf.ListValue)
	ctx := context.Background()
	req := request.Nodes{}

	expected := new(gp.ListValue)
	res, err := cfgmgmt.GetNodes(ctx, &req)

	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestNodesWithFilterReturnsListWithOneNode(t *testing.T) {
	nodes := twoNodeArray()

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.Nodes{}

	// Filtering by status:success
	req.Filter = []string{"status:success"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one node", req.Filter),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[0]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Filtering by status:missing
	req.Filter = []string{"status:missing"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one node", req.Filter),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[1]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestNodesWithPagination(t *testing.T) {
	nodes := twoNodeArray()

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.Nodes{
		Pagination: &request.Pagination{
			Size: 1,
		},
	}

	// Showing page number one (by default)
	t.Run(fmt.Sprintf("with pagination '%v' should return only one node/page", req.Pagination),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[0]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Showing page number two
	req.Pagination.Page = 2

	t.Run(fmt.Sprintf("with pagination '%v' should return only one node/page", req.Pagination),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[1]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestNodesWithSorting(t *testing.T) {
	nodes := twoNodeArray()

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.Nodes{
		Sorting: &request.Sorting{Field: "name"},
	}

	// Sorting (by default asc) by name
	t.Run(fmt.Sprintf("with sorting '%v' should sort the nodes ASC (default)", req.Sorting),
		func(t *testing.T) {
			expected := expectedNodeListFromNodeArray(nodes)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Sorting DESC by name
	req.Sorting.Order = request.Order_desc

	t.Run(fmt.Sprintf("with sorting '%v' should sort the nodes DESC", req.Sorting),
		func(t *testing.T) {
			// Reverse order
			expectedNodeArray := []iBackend.Node{nodes[1], nodes[0]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestGetNodesRegexWithExactSameField(t *testing.T) {

	cases := []struct {
		description string
		nodes       []iBackend.Node
		request     request.Nodes
		expected    []string
	}{
		{
			description: "Matching two with regex one with exact",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a1-prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"name:a2-*", "name:a1-prod"},
			},
			expected: []string{"a2-dev", "a2-prod", "a1-prod"},
		},
		{
			description: "Two regex filters on same type",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a1-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a1-dev",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"name:a2-*", "name:a1-*"},
			},
			expected: []string{"a2-dev", "a2-prod", "a1-prod", "a1-dev"},
		},
		{
			description: "Overlapping regex filters on same type",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a1-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a1-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b1-dev",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"name:a2-*", "name:a*"},
			},
			expected: []string{"a2-dev", "a2-prod", "a1-prod", "a1-dev"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Regex with exact filter: %s", test.description), func(t *testing.T) {

			// Adding required node data
			for index := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add node with project
			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()

			// call GetNodes
			res, err := cfgmgmt.GetNodes(context.Background(), &test.request)
			assert.NoError(t, err)

			names := getFieldValues(res, "name")

			// Test what nodes are returned.
			assert.ElementsMatch(t, test.expected, names)
		})
	}
}

func TestGetNodesWildcardCaseInsensitive(t *testing.T) {

	cases := []struct {
		description string
		nodes       []iBackend.Node
		request     request.Nodes
		expected    []string
	}{
		// Node name
		{
			description: "Node name: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"name:a2-*"},
			},
			expected: []string{"a2-dev", "A2-Dev", "A2-prod", "a2-Prod"},
		},
		{
			description: "Node name: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"name:A2-*"},
			},
			expected: []string{"a2-dev", "A2-Dev", "A2-prod", "a2-Prod"},
		},

		// organization
		{
			description: "Org: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "a",
						OrganizationName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "b",
						OrganizationName: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "c",
						OrganizationName: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "d",
						OrganizationName: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "e",
						OrganizationName: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"organization:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "Org: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "a",
						OrganizationName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "b",
						OrganizationName: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "c",
						OrganizationName: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "d",
						OrganizationName: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:         "e",
						OrganizationName: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"organization:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// environment
		{
			description: "environment: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "a",
						Environment: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "b",
						Environment: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "c",
						Environment: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "d",
						Environment: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "e",
						Environment: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"environment:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "environment: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "a",
						Environment: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "b",
						Environment: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "c",
						Environment: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "d",
						Environment: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "e",
						Environment: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"environment:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// platform
		{
			description: "Platform: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Platform: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Platform: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Platform: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Platform: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Platform: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"platform:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "platform: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Platform: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Platform: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Platform: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Platform: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Platform: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"platform:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// resource_names
		{
			description: "resource_names: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "a",
						ResourceNames: []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "b",
						ResourceNames: []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "c",
						ResourceNames: []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "d",
						ResourceNames: []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "e",
						ResourceNames: []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"resource_names:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "resource_names: term lowercase with array values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "a",
						ResourceNames: []string{"a2-dev", "a3-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "b",
						ResourceNames: []string{"b2-prod", "A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "c",
						ResourceNames: []string{"A2-prod", "A1-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "d",
						ResourceNames: []string{"c-9", "a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "e",
						ResourceNames: []string{"c-9", "a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"resource_names:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "resource_names: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "a",
						ResourceNames: []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "b",
						ResourceNames: []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "c",
						ResourceNames: []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "d",
						ResourceNames: []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "e",
						ResourceNames: []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"resource_names:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// error_message
		{
			description: "error_message: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
					},
					ErrorMessage: "a2-dev",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
					},
					ErrorMessage: "A2-Dev",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
					},
					ErrorMessage: "A2-prod",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
					},
					ErrorMessage: "a2-Prod",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
					},
					ErrorMessage: "a3-Prod",
				},
			},
			request: request.Nodes{
				Filter: []string{"error:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "error_message: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
					},
					ErrorMessage: "a2-dev",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
					},
					ErrorMessage: "A2-Dev",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
					},
					ErrorMessage: "A2-prod",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
					},
					ErrorMessage: "a2-Prod",
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
					},
					ErrorMessage: "a3-Prod",
				},
			},
			request: request.Nodes{
				Filter: []string{"error:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// recipes
		{
			description: "recipe: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Recipes:  []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Recipes:  []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Recipes:  []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Recipes:  []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Recipes:  []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"recipe:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "recipe: term lowercase with array values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Recipes:  []string{"a2-dev", "a3-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Recipes:  []string{"b2-prod", "A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Recipes:  []string{"A2-prod", "A1-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Recipes:  []string{"c-9", "a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Recipes:  []string{"c-9", "a4-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"recipe:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "recipe: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Recipes:  []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Recipes:  []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Recipes:  []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Recipes:  []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Recipes:  []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"recipe:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// chef_tags
		{
			description: "ChefTags: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						ChefTags: []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						ChefTags: []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						ChefTags: []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						ChefTags: []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						ChefTags: []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"chef_tags:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "ChefTags: term lowercase with array values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						ChefTags: []string{"a2-dev", "a3-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						ChefTags: []string{"b2-prod", "A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						ChefTags: []string{"A2-prod", "A1-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						ChefTags: []string{"c-9", "a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						ChefTags: []string{"c-9", "a4-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"chef_tags:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "ChefTags: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						ChefTags: []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						ChefTags: []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						ChefTags: []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						ChefTags: []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						ChefTags: []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"chef_tags:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// cookbooks
		{
			description: "cookbooks: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a",
						Cookbooks: []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "b",
						Cookbooks: []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "c",
						Cookbooks: []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "d",
						Cookbooks: []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "e",
						Cookbooks: []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"cookbook:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "cookbooks: term lowercase with array values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a",
						Cookbooks: []string{"a2-dev", "a3-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "b",
						Cookbooks: []string{"b2-prod", "A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "c",
						Cookbooks: []string{"A2-prod", "A1-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "d",
						Cookbooks: []string{"c-9", "a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "e",
						Cookbooks: []string{"c-9", "a4-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"cookbook:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "cookbooks: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a",
						Cookbooks: []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "b",
						Cookbooks: []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "c",
						Cookbooks: []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "d",
						Cookbooks: []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "e",
						Cookbooks: []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"cookbook:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// attributes
		{
			description: "attribute: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
					},
					Attributes: []string{"a2-dev"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
					},
					Attributes: []string{"A2-Dev"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
					},
					Attributes: []string{"A2-prod"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
					},
					Attributes: []string{"a2-Prod"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
					},
					Attributes: []string{"a3-Prod"},
				},
			},
			request: request.Nodes{
				Filter: []string{"attribute:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "attribute: term lowercase with array values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
					},
					Attributes: []string{"a2-dev", "a3-dev"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
					},
					Attributes: []string{"b2-prod", "A2-Dev"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
					},
					Attributes: []string{"A2-prod", "A1-prod"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
					},
					Attributes: []string{"c-9", "a2-Prod"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
					},
					Attributes: []string{"c-9", "a4-Prod"},
				},
			},
			request: request.Nodes{
				Filter: []string{"attribute:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "attribute: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
					},
					Attributes: []string{"a2-dev"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
					},
					Attributes: []string{"A2-Dev"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
					},
					Attributes: []string{"A2-prod"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
					},
					Attributes: []string{"a2-Prod"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
					},
					Attributes: []string{"a3-Prod"},
				},
			},
			request: request.Nodes{
				Filter: []string{"attribute:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// chef_version
		{
			description: "chef_version: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "a",
						ChefVersion: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "b",
						ChefVersion: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "c",
						ChefVersion: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "d",
						ChefVersion: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "e",
						ChefVersion: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"chef_version:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "chef_version: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "a",
						ChefVersion: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "b",
						ChefVersion: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "c",
						ChefVersion: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "d",
						ChefVersion: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "e",
						ChefVersion: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"chef_version:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// roles
		{
			description: "roles: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Roles:    []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Roles:    []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Roles:    []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Roles:    []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Roles:    []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"role:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "roles: term lowercase with array values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Roles:    []string{"a2-dev", "a3-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Roles:    []string{"b2-prod", "A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Roles:    []string{"A2-prod", "A1-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Roles:    []string{"c-9", "a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Roles:    []string{"c-9", "a4-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"role:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "roles: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "a",
						Roles:    []string{"a2-dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "b",
						Roles:    []string{"A2-Dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "c",
						Roles:    []string{"A2-prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "d",
						Roles:    []string{"a2-Prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "e",
						Roles:    []string{"a3-Prod"},
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"role:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// policy_group
		{
			description: "policy_group: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "a",
						PolicyGroup: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "b",
						PolicyGroup: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "c",
						PolicyGroup: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "d",
						PolicyGroup: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "e",
						PolicyGroup: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"policy_group:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "policy_group: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "a",
						PolicyGroup: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "b",
						PolicyGroup: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "c",
						PolicyGroup: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "d",
						PolicyGroup: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "e",
						PolicyGroup: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"policy_group:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// policy_name
		{
			description: "policy_name: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "a",
						PolicyName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "b",
						PolicyName: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "c",
						PolicyName: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "d",
						PolicyName: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "e",
						PolicyName: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"policy_name:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "policy_name: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "a",
						PolicyName: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "b",
						PolicyName: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "c",
						PolicyName: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "d",
						PolicyName: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "e",
						PolicyName: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"policy_name:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// policy_revision
		{
			description: "policy_revision: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "a",
						PolicyRevision: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "b",
						PolicyRevision: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "c",
						PolicyRevision: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "d",
						PolicyRevision: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "e",
						PolicyRevision: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"policy_revision:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "policy_revision: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "a",
						PolicyRevision: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "b",
						PolicyRevision: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "c",
						PolicyRevision: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "d",
						PolicyRevision: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:       "e",
						PolicyRevision: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"policy_revision:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},

		// source_fqdn
		{
			description: "source_fqdn: term lowercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "a",
						SourceFqdn: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "b",
						SourceFqdn: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "c",
						SourceFqdn: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "d",
						SourceFqdn: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "e",
						SourceFqdn: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"source_fqdn:a2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
		{
			description: "source_fqdn: term uppercase with values with varying cases",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "a",
						SourceFqdn: "a2-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "b",
						SourceFqdn: "A2-Dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "c",
						SourceFqdn: "A2-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "d",
						SourceFqdn: "a2-Prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   "e",
						SourceFqdn: "a3-Prod",
					},
				},
			},
			request: request.Nodes{
				Filter: []string{"source_fqdn:A2-*"},
			},
			expected: []string{"a", "b", "c", "d"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Wildcard case sensitive: %s", test.description), func(t *testing.T) {

			// Adding required node data
			for index := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add node with project
			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()

			// call GetNodes
			res, err := cfgmgmt.GetNodes(context.Background(), &test.request)
			assert.NoError(t, err)

			names := getFieldValues(res, "name")

			// Test what nodes are returned.
			assert.ElementsMatch(t, test.expected, names)
		})
	}
}

func TestGetRunsProjectFilter(t *testing.T) {
	cases := []struct {
		description string
		node        iBackend.Node
		runs        []iBackend.Run
		request     request.Runs
		ctx         context.Context
		expected    []string
	}{
		{
			description: "Getting runs with node matching projects",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{"project9"}),
			request:  request.Runs{},
			expected: []string{"1", "2"},
		},
		{
			description: "Getting runs with node matching one of two projects",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{"project9", "project3"}),
			request:  request.Runs{},
			expected: []string{"1", "2"},
		},
		{
			description: "Getting runs with node not matching projects",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{"project3"}),
			request:  request.Runs{},
			expected: []string{},
		},
		{
			description: "Getting runs with node matching projects with filters",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					RunID: "2",
					NodeInfo: iBackend.NodeInfo{
						Status: "success",
					},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			request: request.Runs{
				Filter: []string{"status:success"},
			},
			expected: []string{"2"},
		},
		{
			description: "Getting no runs with node matching projects with filters",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
				{
					RunID: "2",
					NodeInfo: iBackend.NodeInfo{
						Status: "failure",
					},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			request: request.Runs{
				Filter: []string{"status:success"},
			},
			expected: []string{},
		},
		{
			description: "Getting runs with matching node all projects allowed",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			request:  request.Runs{},
			expected: []string{"1", "2"},
		},
		{
			description: "Getting runs with matching node with no projects with all projects allowed",
			node: iBackend.Node{
				Projects: []string{},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			request:  request.Runs{},
			expected: []string{"1", "2"},
		},
		{
			description: "Getting runs with matching node with no projects with unassigned project only",
			node: iBackend.Node{
				Projects: []string{},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request:  request.Runs{},
			expected: []string{"1", "2"},
		},
		{
			description: "Getting runs with matching node with no projects with unassigned project only",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request:  request.Runs{},
			expected: []string{},
		},
		{
			description: "Getting runs with matching node with unassigned and normal project",
			node: iBackend.Node{
				Projects: []string{"project9"},
			},
			runs: []iBackend.Run{
				{
					RunID: "1",
				},
				{
					RunID: "2",
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID, "project9"}),
			request:  request.Runs{},
			expected: []string{"1", "2"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			test.node.Exists = true
			test.node.NodeInfo = iBackend.NodeInfo{
				EntityUuid: newUUID(),
			}

			test.request.NodeId = test.node.NodeInfo.EntityUuid

			for index := range test.runs {
				test.runs[index].NodeInfo.EntityUuid = test.node.NodeInfo.EntityUuid
				test.runs[index].StartTime = time.Now()
				test.runs[index].EndTime = time.Now()
			}

			// Add node with project
			suite.IngestNodes([]iBackend.Node{test.node})
			suite.IngestRuns(test.runs)
			defer suite.DeleteAllDocuments()

			// call GetNodes
			res, err := cfgmgmt.GetRuns(test.ctx, &test.request)

			assert.NoError(t, err)

			ids := getFieldValues(res, "id")

			// Test what nodes are returned.
			assert.ElementsMatch(t, test.expected, ids)
		})
	}
}

func TestGetNodesProjectFilter(t *testing.T) {

	cases := []struct {
		description string
		nodes       []iBackend.Node
		request     request.Nodes
		ctx         context.Context
		expected    []string
	}{
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
			ctx:      contextWithProjects([]string{"one"}),
			request:  request.Nodes{},
			expected: []string{"1", "2"},
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
			ctx:      contextWithProjects([]string{"one", "two"}),
			request:  request.Nodes{},
			expected: []string{"1", "2"},
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
			ctx:      contextWithProjects([]string{"one"}),
			request:  request.Nodes{},
			expected: []string{"2"},
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
			ctx:      contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			request:  request.Nodes{},
			expected: []string{"1", "2", "3"},
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
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request:  request.Nodes{},
			expected: []string{"1"},
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
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request:  request.Nodes{},
			expected: []string{},
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
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID, "two"}),
			request:  request.Nodes{},
			expected: []string{"1", "2"},
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
			ctx: contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			request: request.Nodes{
				Filter: []string{"status:success"},
			},
			expected: []string{"2"},
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
			ctx: contextWithProjects([]string{"one"}),
			request: request.Nodes{
				Filter: []string{"status:success"},
			},
			expected: []string{},
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
			ctx: contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request: request.Nodes{
				Filter: []string{"status:success"},
			},
			expected: []string{"2"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {

			// Adding required node data
			for index := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add node with project
			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()

			// call GetNodes
			res, err := cfgmgmt.GetNodes(test.ctx, &test.request)
			assert.NoError(t, err)

			names := getFieldValues(res, "name")

			// Test what nodes are returned.
			assert.ElementsMatch(t, test.expected, names)
		})
	}
}

func TestNodesWithTableDriven(t *testing.T) {
	dataNodes := []struct {
		number int
		node   iBackend.NodeInfo
	}{
		{10, iBackend.NodeInfo{
			Status: "success", Platform: "ubuntu",
			Environment: "dev", OrganizationName: "org1"}},
		{10, iBackend.NodeInfo{
			Status: "success", Platform: "windows",
			Environment: "dev", OrganizationName: "org2"}},
		{10, iBackend.NodeInfo{
			Status: "failure", Platform: "ubuntu",
			Environment: "prod", OrganizationName: "org3"}},
		{20, iBackend.NodeInfo{
			Status: "success", Platform: "centos",
			Environment: "prod", OrganizationName: "org1"}},
		{10, iBackend.NodeInfo{
			Status: "failure", Platform: "oracle",
			Environment: "dev", OrganizationName: "org2"}},
		{20, iBackend.NodeInfo{
			Status: "missing", Platform: "ubuntu",
			Environment: "prod", OrganizationName: "org3"}},
	}

	var (
		nodes      = make([]iBackend.Node, 0)
		namePrefix = "node-"
		index      = 0
	)

	for _, data := range dataNodes {
		for i := 0; i < data.number; i++ {
			data.node.EntityUuid = newUUID()
			data.node.NodeName = namePrefix + fmt.Sprintf("%03d", index)
			node := iBackend.Node{
				NodeInfo: data.node,
				Exists:   true,
			}
			nodes = append(nodes, node)
			index++
		}
	}
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	cases := []struct {
		description string
		request     request.Nodes
		expected    *gp.ListValue
	}{
		{"should return first 10 nodes (default)",
			request.Nodes{},
			expectedNodeListFromNodeArray(nodes[0:10])},
		{"should return all nodes",
			request.Nodes{Pagination: &request.Pagination{Size: int32(index)}},
			expectedNodeListFromNodeArray(nodes)},
		{"should return all 'ubuntu' nodes",
			request.Nodes{
				Filter: []string{"platform:ubuntu"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "platform:ubuntu")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "platform:ubuntu"))},
		{"should return all 'ubuntu' nodes with wildcard",
			request.Nodes{
				Filter: []string{"platform:ubu?tu"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "platform:ubuntu")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "platform:ubuntu"))},
		{"should return all nodes when wildcard org search",
			request.Nodes{
				Filter:     []string{"organization:o*"},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(nodes)},
		{"should return all prod nodes when combined wildcard org and env prod search",
			request.Nodes{
				Filter:     []string{"organization:o*", "environment:prod"},
				Pagination: &request.Pagination{Size: int32(len(filterNodesArray(nodes, "environment:prod")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod"))},
		{"should return all prod nodes when combined wildcard o* and env pro? wildcard search",
			request.Nodes{
				Filter:     []string{"organization:o*", "environment:pro?"},
				Pagination: &request.Pagination{Size: int32(len(filterNodesArray(nodes, "environment:prod")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod"))},
		{"should return all nodes that are marked as 'success'",
			request.Nodes{
				Filter: []string{"status:success"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "status:success")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "status:success"))},
		{"should return only nodes from organization org2 ",
			request.Nodes{
				Filter: []string{"organization:org2"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "organization:org2")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "organization:org2"))},
		{"should return nodes from both dev & prod environments ",
			request.Nodes{
				Filter: []string{"environment:dev", "environment:prod"},
				Pagination: &request.Pagination{
					Size: int32(index + 1)},
			},
			expectedNodeListFromNodeArray(nodes)},
		{"should return only node-010",
			request.Nodes{Filter: []string{"name:node-010"}},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "name:node-010"))},
		{"should return NO nodes",
			request.Nodes{Filter: []string{"name:NOT_FOUND"}},
			new(gp.ListValue)},
		{"should return NO nodes (No missing windows nodes)",
			request.Nodes{Filter: []string{"status:missing", "platform:windows"}},
			new(gp.ListValue)},
		{"should return page 1 for 20 nodes from the prod environment",
			request.Nodes{
				Filter: []string{"environment:prod"},
				Pagination: &request.Pagination{
					Page: 1,
					Size: int32(20)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod")[0:20])},
		{"should return page 2 for 20 nodes from the prod environment",
			request.Nodes{
				Filter: []string{"environment:prod"},
				Pagination: &request.Pagination{
					Page: 2,
					Size: int32(20)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod")[20:40])},
		{"should return the list of all nodes sort by 'name' ASC",
			request.Nodes{
				Sorting: &request.Sorting{
					Field: "name",
					Order: request.Order_asc,
				},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(nodes)},
		{"should return the list of all nodes sort by 'name' DESC",
			request.Nodes{
				Sorting: &request.Sorting{
					Field: "name",
					Order: request.Order_desc,
				},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(reverseNodesArray(nodes))},
		{"should return the list of successful nodes sort by 'name' DESC",
			request.Nodes{
				Filter: []string{"status:success"},
				Sorting: &request.Sorting{
					Field: "name",
					Order: request.Order_desc,
				},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(reverseNodesArray(filterNodesArray(nodes, "status:success")))},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetNodes(ctx, &test.request)
				assert.NoError(t, err)
				assert.Equal(t, test.expected, res)
			})
	}
}

// Node's checkin time is before last converge-history index
func TestNodesMissingRunsBeforeIndex(t *testing.T) {
	uuid := newUUID()
	// create a run with time now
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, 0, -1), // started a day
		EndTime:   time.Now(),                   // the run just finished
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time before the run time
	node := iBackend.Node{
		Checkin:     time.Now().AddDate(-1, 0, 0), // a checkin time for a year ago
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == false
	assert.Equal(t, 1, len(res.Values))
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, !ok || !value.GetBoolValue(),
			"'has_runs_data' should be missing or set to false")
	}
}

// LastCCRReceived is before last index, checkin time after
// This could be the case for a liveness managed node
func TestLastCCRBeforeIndex(t *testing.T) {
	uuid := newUUID()
	// create a run with time now
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, 0, -1), // started a day
		EndTime:   time.Now(),                   // the run just finished
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time before the run time
	node := iBackend.Node{
		Checkin:         time.Now(),
		Exists:          true,
		LatestRunID:     "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		LastCCRReceived: time.Now().AddDate(-1, 0, 0), // a year ago
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == false
	assert.Equal(t, 1, len(res.Values))
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, !ok || !value.GetBoolValue(),
			"'has_runs_data' should be missing or set to false")
	}
}

// Node's checkin time is after last converge-history index
func TestNodesMissingRunsAfterIndex(t *testing.T) {
	uuid := newUUID()

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, -1, -1), // started a month and a day ago
		EndTime:   time.Now().AddDate(0, -1, -1), // finished a month ago
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time after the run time
	node := iBackend.Node{
		Checkin:     time.Now(), // a checkin time is now
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == true
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, ok && value.GetBoolValue(),
			"'has_runs_data' should not be missing and set to true")
	}
}

// converge-history index is 'converge-history-0001.01.01' the nodes with dates
// after this should have has_runs_data set to true
func TestNodesMissingRunsFromYear1(t *testing.T) {
	uuid := newUUID()

	indexDate := time.Date(1, 1, 1, 1, 1, 1, 1, time.UTC)

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: indexDate.Add(-1 / time.Minute),
		EndTime:   indexDate,
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time of now
	node := iBackend.Node{
		Checkin:     time.Now(),
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.NoError(t, err)

	// test that the node has a 'HasRunsData' == true
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, ok, "'has_runs_data' should not be missing")
		assert.True(t, value.GetBoolValue(), "'has_runs_data' should be set to true")
	}
}

// Node's checkin time the beginning of the day the same date as the last converge-history index
func TestNodesMissingRunsEqualIndex(t *testing.T) {
	uuid := newUUID()

	indexDate := time.Now().UTC()

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: indexDate.Add(-1 / time.Minute), // started a minute before now
		EndTime:   indexDate,                       // ended now
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time the same day and at the beginning of the day the run time
	beginningOfTheDay := time.Date(indexDate.Year(), indexDate.Month(), indexDate.Day(),
		0, 0, 0, 0, indexDate.Location())
	node := iBackend.Node{
		Checkin:     beginningOfTheDay,
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == true
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, ok && value.GetBoolValue(),
			"'has_runs_data' should not be missing and set to true")
	}
}

// Node's checkin time is after last converge-history index, but the 'LatestRunID'
// is set to empty so 'HasRunsData' should equal false
func TestNodesMissingRunsLatestRunIDEmpty(t *testing.T) {
	uuid := newUUID()

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, -1, -1), // started a month and a day ago
		EndTime:   time.Now().AddDate(0, -1, -1), // finished a month ago
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time after the run time
	node := iBackend.Node{
		Checkin:     time.Now(), // a checkin time is now
		Exists:      true,
		LatestRunID: "",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == false
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, !ok || !value.GetBoolValue(),
			"'has_runs_data' should be missing or set to false")
	}
}

func TestNodeRunErrors(t *testing.T) {
	t.Run("Get errors with zero errors in the database", func(t *testing.T) {
		nodes := []iBackend.Node{}

		suite.IngestNodes(nodes)
		defer suite.DeleteAllDocuments()

		res, err := cfgmgmt.GetErrors(context.Background(), &apiReq.Errors{})
		require.NoError(t, err)

		assert.Len(t, res.Errors, 0)
	})

	t.Run("Get errors with one error in the database", func(t *testing.T) {
		nodeOne := iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				NodeName:   "a",
				Status:     "failure",
				EntityUuid: newUUID(),
			},
			ErrorMessage: "an example error occurred",
			ErrorType:    "FuntimeError",
			Exists:       true,
		}

		nodes := []iBackend.Node{nodeOne}

		suite.IngestNodes(nodes)
		defer suite.DeleteAllDocuments()

		res, err := cfgmgmt.GetErrors(context.Background(), &apiReq.Errors{})
		require.NoError(t, err)

		require.Len(t, res.Errors, 1)
		errInfo := res.Errors[0]

		assert.Equal(t, int32(1), errInfo.Count)
		assert.Equal(t, "FuntimeError", errInfo.Type)
		assert.Equal(t, "an example error occurred", errInfo.ErrorMessage)
	})

	t.Run("Get errors with several errors in the database", func(t *testing.T) {
		nodeOne := iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				NodeName:   "one",
				Status:     "failure",
				EntityUuid: newUUID(),
			},
			ErrorMessage: "an example error occurred",
			ErrorType:    "FuntimeError",
			Exists:       true,
		}
		nodeTwo := iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				NodeName:   "two",
				Status:     "failure",
				EntityUuid: newUUID(),
			},
			ErrorMessage: "an example error occurred",
			ErrorType:    "FuntimeError",
			Exists:       true,
		}
		nodeThree := iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				NodeName:   "three",
				Status:     "failure",
				EntityUuid: newUUID(),
			},
			ErrorMessage: "an different error occurred",
			ErrorType:    "PuntimeError",
			Exists:       true,
		}

		nodes := []iBackend.Node{nodeOne, nodeTwo, nodeThree}

		suite.IngestNodes(nodes)
		defer suite.DeleteAllDocuments()

		res, err := cfgmgmt.GetErrors(context.Background(), &apiReq.Errors{})
		require.NoError(t, err)

		require.Len(t, res.Errors, 2)
		errInfo := res.Errors[0]

		assert.Equal(t, int32(2), errInfo.Count)
		assert.Equal(t, "FuntimeError", errInfo.Type)
		assert.Equal(t, "an example error occurred", errInfo.ErrorMessage)

		errInfoTwo := res.Errors[1]

		assert.Equal(t, int32(1), errInfoTwo.Count)
		assert.Equal(t, "PuntimeError", errInfoTwo.Type)
		assert.Equal(t, "an different error occurred", errInfoTwo.ErrorMessage)
	})

	t.Run("Get errors with more than 10 error type/message combos in the database", func(t *testing.T) {
		var nodes []iBackend.Node

		for i := 1; i < 16; i++ {
			for j := 0; j <= (i - 1); j++ {
				node := iBackend.Node{
					NodeInfo: iBackend.NodeInfo{
						NodeName:   fmt.Sprintf("node-group-%d-instance-%d", i, j),
						Status:     "failure",
						EntityUuid: newUUID(),
					},
					ErrorMessage: fmt.Sprintf("example-%d error occurred", i),
					ErrorType:    fmt.Sprintf("FuntimeError%d", i),
					Exists:       true,
				}
				nodes = append(nodes, node)
			}
		}

		suite.IngestNodes(nodes)
		defer suite.DeleteAllDocuments()

		res, err := cfgmgmt.GetErrors(context.Background(), &apiReq.Errors{})
		require.NoError(t, err)

		// The 10 top errors are returned
		// They are sorted by total count descending

		require.Len(t, res.Errors, 10)
		errInfo := res.Errors[0]

		assert.Equal(t, int32(15), errInfo.Count)
		assert.Equal(t, "FuntimeError15", errInfo.Type)
		assert.Equal(t, "example-15 error occurred", errInfo.ErrorMessage)

		errInfo = res.Errors[1]

		assert.Equal(t, int32(14), errInfo.Count)
		assert.Equal(t, "FuntimeError14", errInfo.Type)
		assert.Equal(t, "example-14 error occurred", errInfo.ErrorMessage)

		errInfo = res.Errors[9]

		assert.Equal(t, int32(6), errInfo.Count)
		assert.Equal(t, "FuntimeError6", errInfo.Type)
		assert.Equal(t, "example-6 error occurred", errInfo.ErrorMessage)

		// With a Size of 1, we should get one:

		resExpectOne, err := cfgmgmt.GetErrors(context.Background(), &apiReq.Errors{Size: 1})
		require.NoError(t, err)
		assert.Len(t, resExpectOne.Errors, 1)
		assert.Equal(t, "FuntimeError15", resExpectOne.Errors[0].Type)

		// With a Size of -1, we should get all:

		resExpectAll, err := cfgmgmt.GetErrors(context.Background(), &apiReq.Errors{Size: -1})
		require.NoError(t, err)
		require.Len(t, resExpectAll.Errors, 15)
		assert.Equal(t, "FuntimeError15", resExpectAll.Errors[0].Type)
		assert.Equal(t, "FuntimeError1", resExpectAll.Errors[14].Type)
	})

	// the request to elasticsearch will have to scroll over different pages when
	// there are more than 1000 total buckets of error type+message combination
	t.Run("Get errors with more than 1000 error type/message combos in the database", func(t *testing.T) {
		var nodes []iBackend.Node

		for i := 1; i < 1101; i++ {
			node := iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					NodeName:   fmt.Sprintf("node-instance-%d", i),
					Status:     "failure",
					EntityUuid: newUUID(),
				},
				ErrorMessage: fmt.Sprintf("example-%d error occurred", i),
				ErrorType:    fmt.Sprintf("FuntimeError%d", i),
				Exists:       true,
			}
			nodes = append(nodes, node)
		}
		// when i is a multiple of 100, add 1 to its count
		for i := 100; i < 1200; i += 100 {
			node := iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					NodeName:   fmt.Sprintf("node-instance-%d", i),
					Status:     "failure",
					EntityUuid: newUUID(),
				},
				ErrorMessage: fmt.Sprintf("example-%d error occurred", i),
				ErrorType:    fmt.Sprintf("FuntimeError%d", i),
				Exists:       true,
			}
			nodes = append(nodes, node)
		}

		suite.IngestNodes(nodes)
		defer suite.DeleteAllDocuments()

		res, err := cfgmgmt.GetErrors(context.Background(), &apiReq.Errors{})
		require.NoError(t, err)

		require.Len(t, res.Errors, 10)

		// Sorting: error counts are first sorted on number of occurrences, then
		// lexically on error type as a string. Shorter strings sort before longer
		// ones. So you get:
		// FuntimeError100, FuntimeError1000, FuntimeError200, .. FuntimeError800
		// and FuntimeError900 is omitted because it's the 11th item.

		errInfo := res.Errors[0]
		assert.Equal(t, int32(2), errInfo.Count)
		assert.Equal(t, "FuntimeError100", errInfo.Type)
		assert.Equal(t, "example-100 error occurred", errInfo.ErrorMessage)

		errInfo = res.Errors[1]
		assert.Equal(t, int32(2), errInfo.Count)
		assert.Equal(t, "FuntimeError1000", errInfo.Type)
		assert.Equal(t, "example-1000 error occurred", errInfo.ErrorMessage)

		errInfo = res.Errors[9]
		assert.Equal(t, int32(2), errInfo.Count)
		assert.Equal(t, "FuntimeError800", errInfo.Type)
		assert.Equal(t, "example-800 error occurred", errInfo.ErrorMessage)
	})

	// Errors are only collected from nodes when the most recent run has status
	// failure. So you can't filter them on status.
	t.Run("Get errors with invalid filters returns error", func(t *testing.T) {
		var req apiReq.Errors

		req.Filter = []string{"status:success"}
		_, err := cfgmgmt.GetErrors(context.Background(), &req)
		require.Error(t, err)

		req.Filter = []string{"status:aliens"}
		_, err = cfgmgmt.GetErrors(context.Background(), &req)
		require.Error(t, err)
	})

	// node filters (other than 'status') should work
	t.Run("Get errors with filters", func(t *testing.T) {
		nodeOne := iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				NodeName:         "a",
				Status:           "failure",
				EntityUuid:       newUUID(),
				Platform:         "ubuntu",
				OrganizationName: "org1",
			},
			ErrorMessage: "an example error occurred",
			ErrorType:    "NodeAError",
			Exists:       true,
		}

		nodeTwo := iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				NodeName:         "b",
				Status:           "failure",
				EntityUuid:       newUUID(),
				Platform:         "centos",
				OrganizationName: "org1",
			},
			ErrorMessage: "an example error occurred",
			ErrorType:    "NodeBError",
			Exists:       true,
		}

		nodes := []iBackend.Node{nodeOne, nodeTwo}

		suite.IngestNodes(nodes)
		defer suite.DeleteAllDocuments()

		var req apiReq.Errors

		req.Filter = []string{"name:a"}
		res, err := cfgmgmt.GetErrors(context.Background(), &req)
		require.NoError(t, err)
		require.Len(t, res.Errors, 1)
		assert.Equal(t, "NodeAError", res.Errors[0].Type)

		req.Filter = []string{"name:b"}
		res, err = cfgmgmt.GetErrors(context.Background(), &req)
		require.NoError(t, err)
		require.Len(t, res.Errors, 1)
		assert.Equal(t, "NodeBError", res.Errors[0].Type)

		req.Filter = []string{"organization:org1"}
		res, err = cfgmgmt.GetErrors(context.Background(), &req)
		require.NoError(t, err)
		require.Len(t, res.Errors, 2)
		assert.Equal(t, "NodeAError", res.Errors[0].Type)
		assert.Equal(t, "NodeBError", res.Errors[1].Type)

		req.Filter = []string{"organization:org2"}
		res, err = cfgmgmt.GetErrors(context.Background(), &req)
		require.NoError(t, err)
		require.Len(t, res.Errors, 0)

		req.Filter = []string{"platform:centos"}
		res, err = cfgmgmt.GetErrors(context.Background(), &req)
		require.NoError(t, err)
		require.Len(t, res.Errors, 1)
		assert.Equal(t, "NodeBError", res.Errors[0].Type)

	})

	t.Run("Get errors with project filters", func(t *testing.T) {
		nodeOne := iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				NodeName:   "a",
				Status:     "failure",
				EntityUuid: newUUID(),
			},
			ErrorMessage: "an example error occurred",
			ErrorType:    "FuntimeError",
			Exists:       true,
			Projects:     []string{"projectOne"},
		}

		nodes := []iBackend.Node{nodeOne}

		suite.IngestNodes(nodes)
		defer suite.DeleteAllDocuments()

		ctx := contextWithProjects([]string{"projectOne"})
		res, err := cfgmgmt.GetErrors(ctx, &apiReq.Errors{})
		require.NoError(t, err)
		assert.Len(t, res.Errors, 1)

		ctx = contextWithProjects([]string{"projectOne", "projectTwo"})
		res, err = cfgmgmt.GetErrors(ctx, &apiReq.Errors{})
		require.NoError(t, err)
		assert.Len(t, res.Errors, 1)

		ctx = contextWithProjects([]string{"projectNOPE", "projectTwo"})
		res, err = cfgmgmt.GetErrors(ctx, &apiReq.Errors{})
		require.NoError(t, err)
		assert.Len(t, res.Errors, 0) // filtered out
	})

}

func ingestNodeArrayToMessageArray(nodes []iBackend.Node, hasRunsData bool) []proto.Message {
	messages := make([]proto.Message, len(nodes))
	for i, node := range nodes {
		checkin, _ := ptypes.TimestampProto(node.Checkin.UTC())
		messages[i] = &external_response.Node{
			Id:              node.EntityUuid,
			Name:            node.NodeName,
			Fqdn:            node.Fqdn,
			Checkin:         checkin,
			UptimeSeconds:   int32(node.UptimeSeconds),
			Organization:    node.OrganizationName,
			Environment:     node.Environment,
			Platform:        node.Platform,
			PlatformFamily:  node.PlatformFamily,
			PlatformVersion: node.PlatformVersion,
			Status:          node.Status,
			SourceFqdn:      node.SourceFqdn,
			LatestRunId:     node.LatestRunID,
			PolicyName:      node.PolicyName,
			PolicyGroup:     node.PolicyGroup,
			PolicyRevision:  node.PolicyRevision,
			HasRunsData:     hasRunsData,
			LastCcrReceived: checkin,
		}
	}
	return messages
}

func expectedNodeListFromNodeArray(nodes []iBackend.Node) *gp.ListValue {
	// default to hasRunsData == false because there is no converge-history indices
	return expectedNodeListFromNodeArrayWithHasRunsData(nodes, false)
}

func expectedNodeListFromNodeArrayWithHasRunsData(nodes []iBackend.Node, hasRunsData bool) *gp.ListValue {
	expectedList := new(gp.ListValue)
	msgArray := ingestNodeArrayToMessageArray(nodes, hasRunsData)
	// TODO: (@afiune) How do we wanna handle this error? panic¿?
	_ = messageArrayToListValue(msgArray, expectedList)
	return expectedList
}

// filterNodesArray will return a new list of Nodes that contains the provided filter
//
// Example of a filter:
// => "environment:prod"
// => "status:success"
// => "name:node-000"
func filterNodesArray(nodes []iBackend.Node, filter string) []iBackend.Node {
	var (
		filteredNodes = make([]iBackend.Node, 0)
		kv            = strings.Split(filter, ":")
		key           = kv[0]
		value         = kv[1]
	)

	for _, node := range nodes {
		if nodeFieldEqualToValue(node, key, value) {
			filteredNodes = append(filteredNodes, node)
		}
	}
	return filteredNodes
}

// nodeFieldEqualToValue checks if the provided Node matches with the provided Value
//
// Example: Does a Node belong to a specific environment? (prod)
// ```
// nodeFieldEqualToValue(node, "environment", "prod")
// ```
func nodeFieldEqualToValue(node iBackend.Node, key string, value string) bool {
	switch key {
	case "platform":
		return node.Platform == value
	case "environment":
		return node.Environment == value
	case "status":
		return node.Status == value
	case "name":
		return node.NodeName == value
	case "organization":
		return node.OrganizationName == value
	}
	return false
}

// twoNodeArray returns an Array of 2 Nodes
func twoNodeArray() []iBackend.Node {
	return []iBackend.Node{
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "success",
				NodeName:         "node1",
				OrganizationName: "org1",
				Environment:      "env1",
			},
			Exists: true,
		},
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "missing",
				NodeName:         "node2",
				OrganizationName: "org1",
				Environment:      "env1",
			},
			Exists: true,
		},
	}
}

// reverseNodesArray accepts a Nodes Array and reverses it
func reverseNodesArray(input []iBackend.Node) []iBackend.Node {
	if len(input) == 0 {
		return input
	}
	return append(reverseNodesArray(input[1:]), input[0])
}

// messageArrayToListValue Casts a 'Proto Message Array' into a 'Proto ListValue'
//
// Copied from 'grpcserver/util.go'
//
// Why? - We do not want to use private functions that we do not expose to consumers,
//        for that reason we are copy and pasting the functions inside the tests
func messageArrayToListValue(messages []proto.Message, list *gp.ListValue) error {
	list.Values = make([]*gp.Value, len(messages))

	for i, msg := range messages {
		v := gp.Value{}
		content, err := getMessageRawJSON(msg)
		if err != nil {
			return err
		}

		err = protoFromJSON(content, &v)
		if err != nil {
			return err
		}

		list.Values[i] = &v
	}
	return nil
}

func protoFromJSON(content string, pb proto.Message) error {
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	return unmarshaler.Unmarshal(strings.NewReader(content), pb)
}

func getMessageRawJSON(message proto.Message) (string, error) {
	return (&jsonpb.Marshaler{OrigName: true}).MarshalToString(message)
}

//fieldName == name
func getFieldValues(listValue *gp.ListValue, fieldName string) []string {
	names := make([]string, 0)
	for _, value := range listValue.GetValues() {
		m := value.GetStructValue().GetFields()
		names = append(names, m[fieldName].GetStringValue())
	}

	return names
}
