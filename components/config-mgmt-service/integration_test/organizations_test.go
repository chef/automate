//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"strconv"
	"testing"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	gp "github.com/golang/protobuf/ptypes/struct"
	gpStruct "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
)

func TestOrganizationsEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(request.Organizations)
	expected := new(gpStruct.ListValue)

	res, err := cfgmgmt.GetOrganizations(ctx, req)

	t.Log("\nwith NO nodes should return an empty list of values []")
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestOrganizationsWithTwoNodes(t *testing.T) {
	nodes := []iBackend.Node{
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "missing",
				OrganizationName: "cool_org",
			},
			Exists: true,
		},
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "success",
				OrganizationName: "awesome_org",
			},
			Exists: true,
		},
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := new(request.Organizations)
	expected := &gpStruct.ListValue{
		Values: []*gpStruct.Value{
			{Kind: &gpStruct.Value_StringValue{"awesome_org"}},
			{Kind: &gpStruct.Value_StringValue{"cool_org"}},
		},
	}

	res, err := cfgmgmt.GetOrganizations(ctx, req)

	t.Log("\nwith two nodes should return both organizations")
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestOrganizationsProjectFilter(t *testing.T) {
	cases := []struct {
		description string
		nodes       []iBackend.Node
		ctx         context.Context
		expected    []string
	}{
		{
			description: "Two nodes matching on the same project tag",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			expected: []string{"org1", "org2"},
		},
		{
			description: "Two nodes matching with two project tags",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org2",
					},
					Projects: []string{"one"},
				},
			},
			ctx:      contextWithProjects([]string{"one", "two"}),
			expected: []string{"org1", "org2"},
		},
		{
			description: "Two nodes, one matching",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			expected: []string{"org2"},
		},
		{
			description: "Matching all",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org2",
					},
					Projects: []string{"two", "one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org3",
					},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			expected: []string{"org1", "org2", "org3"},
		},
		{
			description: "Match one unassigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expected: []string{"org1"},
		},
		{
			description: "No unassigned; no matches",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expected: []string{},
		},
		{
			description: "Match one unassigned and one assigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						OrganizationName: "org2",
					},
					Projects: []string{"two"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID, "two"}),
			expected: []string{"org1", "org2"},
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

			// call GetOrganizations
			res, err := cfgmgmt.GetOrganizations(test.ctx, &request.Organizations{})
			assert.NoError(t, err)

			orgs := getValues(res)

			// Test what nodes are returned.
			assert.Equal(t, test.expected, orgs)
		})
	}
}

func getValues(listValue *gp.ListValue) []string {
	values := make([]string, 0)
	for _, value := range listValue.GetValues() {
		m := value.GetStringValue()
		values = append(values, m)
	}

	return values
}

func TestOrganizationsWithOverTenNodes(t *testing.T) {

	nodes := make([]iBackend.Node, 15)
	for count := 0; count < 15; count++ {
		orgName := "org_" + strconv.Itoa(count)
		nodes[count] = iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "missing",
				OrganizationName: orgName,
			},
			Exists: true,
		}
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := new(request.Organizations)

	t.Run(fmt.Sprintf("with should return all 15 orgs"),
		func(t *testing.T) {
			res, err := cfgmgmt.GetOrganizations(ctx, req)
			assert.Nil(t, err)
			assert.Equal(t, 15, len(res.Values))
		})
}
