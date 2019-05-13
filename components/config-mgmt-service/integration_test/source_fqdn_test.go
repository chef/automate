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
	gpStruct "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
)

func TestSourceFqdnsEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(request.SourceFQDNS)
	expected := new(gpStruct.ListValue)

	res, err := cfgmgmt.GetSourceFqdns(ctx, req)

	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestSourceFqdnsWithTwoNodes(t *testing.T) {
	nodes := []iBackend.Node{
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: newUUID(),
				Status:     "missing",
				SourceFqdn: "chef-server.example.com",
			},
			Exists: true,
		},
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: newUUID(),
				Status:     "success",
				SourceFqdn: "chef-solo.local",
			},
			Exists: true,
		},
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := new(request.SourceFQDNS)
	expected := &gpStruct.ListValue{
		Values: []*gpStruct.Value{
			{
				Kind: &gpStruct.Value_StringValue{"chef-server.example.com"},
			},
			{
				Kind: &gpStruct.Value_StringValue{"chef-solo.local"},
			},
		},
	}

	res, err := cfgmgmt.GetSourceFqdns(ctx, req)

	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestSourceFqdnsWithOverTenNodes(t *testing.T) {

	nodes := make([]iBackend.Node, 15)
	for count := 0; count < 15; count++ {
		sourceFqdn := "chef-solo.local_" + strconv.Itoa(count)
		nodes[count] = iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: newUUID(),
				Status:     "success",
				SourceFqdn: sourceFqdn,
			},
			Exists: true,
		}
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := new(request.SourceFQDNS)

	t.Run(fmt.Sprintf("with should return all 15 source FQDNs"),
		func(t *testing.T) {
			res, err := cfgmgmt.GetSourceFqdns(ctx, req)
			assert.Nil(t, err)
			assert.Equal(t, 15, len(res.Values))
		})

}

func TestSourceFqdnsProjectFilter(t *testing.T) {
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
						SourceFqdn: "fqdn1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			expected: []string{"fqdn1", "fqdn2"},
		},
		{
			description: "Two nodes matching with two project tags",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn2",
					},
					Projects: []string{"one"},
				},
			},
			ctx:      contextWithProjects([]string{"one", "two"}),
			expected: []string{"fqdn1", "fqdn2"},
		},
		{
			description: "Two nodes, one matching",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			expected: []string{"fqdn2"},
		},
		{
			description: "Matching all",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn2",
					},
					Projects: []string{"two", "one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn3",
					},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			expected: []string{"fqdn1", "fqdn2", "fqdn3"},
		},
		{
			description: "Match one unassigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expected: []string{"fqdn1"},
		},
		{
			description: "No unassigned; no matches",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn2",
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
						SourceFqdn: "fqdn1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						SourceFqdn: "fqdn2",
					},
					Projects: []string{"two"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID, "two"}),
			expected: []string{"fqdn1", "fqdn2"},
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

			// call GetSourceFqdns
			res, err := cfgmgmt.GetSourceFqdns(test.ctx, &request.SourceFQDNS{})
			assert.NoError(t, err)

			fqdns := getValues(res)

			// Test what nodes are returned.
			assert.Equal(t, test.expected, fqdns)
		})
	}
}
