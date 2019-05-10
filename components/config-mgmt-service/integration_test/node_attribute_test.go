//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"testing"

	"google.golang.org/grpc/codes"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestNodeAttributeEmptyRequestReturnsError(t *testing.T) {
	ctx := context.Background()
	req := new(request.Node)
	// Expect an empty response
	expected := new(response.NodeAttribute)

	res, err := cfgmgmt.GetAttributes(ctx, req)
	grpctest.AssertCode(t, codes.InvalidArgument, err)
	assert.Equal(t, expected, res)
}

func TestNodeAttributeWhenNoNodeFoundReturnsError(t *testing.T) {
	ctx := context.Background()
	req := &request.Node{NodeId: "not-found"}
	// Expect an empty response
	expected := new(response.NodeAttribute)

	res, err := cfgmgmt.GetAttributes(ctx, req)
	grpctest.AssertCode(t, codes.NotFound, err)
	assert.Equal(t, expected, res)
}

func TestNodeAttributeWithOneNodeAttribute(t *testing.T) {
	nodeID := newUUID()
	nodesAttribute := []iBackend.NodeAttribute{
		{
			EntityUUID:        nodeID,
			Name:              "test-node",
			RunList:           []string{"recipe[the_cookbook::a_recipe]"},
			Default:           "{\"foo\":\"bar\"}",
			Normal:            "{\"normal_foo\":\"normal_bar\"}",
			Override:          "{\"override_thing\":\"overwritten\"}",
			DefaultValueCount: 1, NormalValueCount: 1, OverrideValueCount: 1, AllValueCount: 3,
		},
	}

	node := iBackend.Node{
		Exists: true,
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: nodeID,
		},
	}

	suite.IngestNodeAttributes(nodesAttribute)
	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := &request.Node{NodeId: nodeID}
	expected := &response.NodeAttribute{
		NodeId:           nodeID,
		Name:             "test-node",
		RunList:          []string{"recipe[the_cookbook::a_recipe]"},
		Normal:           "{\"normal_foo\":\"normal_bar\"}",
		Default:          "{\"foo\":\"bar\"}",
		Override:         "{\"override_thing\":\"overwritten\"}",
		NormalValueCount: 1, DefaultValueCount: 1, OverrideValueCount: 1, AllValueCount: 3,
	}

	res, err := cfgmgmt.GetAttributes(ctx, req)

	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestNodeAttributeWithTableDriven(t *testing.T) {
	ctx := context.Background()
	var (
		idPrefix   = "MOCK-UUID-"
		namePrefix = "test-node-"
	)
	tests := map[string]struct {
		runlist       []string
		ddefault      string
		normal        string
		override      string
		defaultCount  int
		normalCount   int
		overrideCount int
	}{
		"with normal attributes": {
			normal:      "{\"foo\":\"bar\"}",
			normalCount: 1,
		},
		"with override attributes": {
			override:      "{\"foo\":\"bar\"}",
			overrideCount: 1,
		},
		"with default attributes": {
			ddefault:     "{\"foo\":\"bar\"}",
			defaultCount: 1,
		},
		"with all attributes and runlist": {
			runlist:      []string{"recipe[my_cookbook::my_recipe]"},
			ddefault:     "{\"foo\":\"bar\"}",
			override:     "{\"foo\":\"bar\"}",
			normal:       "{\"foo\":\"bar\"}",
			defaultCount: 1, normalCount: 1, overrideCount: 1,
		},
	}

	// Generating all our nodes attributes, requests and expected responses
	var (
		nodesAttribute = make([]iBackend.NodeAttribute, len(tests))
		nodes          = make([]iBackend.Node, len(tests))
		requests       = make([]request.Node, len(tests))
		responses      = make([]response.NodeAttribute, len(tests))
	)

	index := 0
	for _, t := range tests {
		id := idPrefix + string(index)
		name := namePrefix + string(index)

		nodes[index] = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: id,
			},
		}

		nodesAttribute[index] = iBackend.NodeAttribute{
			EntityUUID:         id,
			Name:               name,
			RunList:            t.runlist,
			Default:            t.ddefault,
			Normal:             t.normal,
			Override:           t.override,
			DefaultValueCount:  t.defaultCount,
			NormalValueCount:   t.normalCount,
			OverrideValueCount: t.overrideCount,
			AllValueCount:      t.normalCount + t.defaultCount + t.overrideCount,
		}

		responses[index] = response.NodeAttribute{
			NodeId:             id,
			Name:               name,
			RunList:            t.runlist,
			Default:            t.ddefault,
			Normal:             t.normal,
			Override:           t.override,
			DefaultValueCount:  int32(t.defaultCount),
			NormalValueCount:   int32(t.normalCount),
			OverrideValueCount: int32(t.overrideCount),
			AllValueCount:      int32(t.normalCount + t.defaultCount + t.overrideCount),
		}

		requests[index] = request.Node{NodeId: id}
		index++
	}

	// Ingest the node attributes
	suite.IngestNodeAttributes(nodesAttribute)
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	// Testing all the requests/responses
	index = 0
	for name := range tests {
		t.Run(name, func(t *testing.T) {
			res, err := cfgmgmt.GetAttributes(ctx, &requests[index])
			assert.Nil(t, err)
			assert.Equal(t, &responses[index], res)
		})
		index++
	}
}

func TestNodeAttributeProjectFilter(t *testing.T) {
	nodeID := newUUID()

	nodesAttributeIngested := []iBackend.NodeAttribute{
		{
			EntityUUID:        nodeID,
			Name:              "test-node",
			RunList:           []string{"recipe[the_cookbook::a_recipe]"},
			Default:           "{\"foo\":\"bar\"}",
			Normal:            "{\"normal_foo\":\"normal_bar\"}",
			Override:          "{\"override_thing\":\"overwritten\"}",
			DefaultValueCount: 1, NormalValueCount: 1, OverrideValueCount: 1, AllValueCount: 3,
		},
	}

	expectedSuccess := &response.NodeAttribute{
		NodeId:           nodeID,
		Name:             "test-node",
		RunList:          []string{"recipe[the_cookbook::a_recipe]"},
		Normal:           "{\"normal_foo\":\"normal_bar\"}",
		Default:          "{\"foo\":\"bar\"}",
		Override:         "{\"override_thing\":\"overwritten\"}",
		NormalValueCount: 1, DefaultValueCount: 1, OverrideValueCount: 1, AllValueCount: 3,
	}

	expectedFailure := &response.NodeAttribute{}

	cases := []struct {
		description  string
		ctx          context.Context
		nodeProjects []string
		expected     *response.NodeAttribute
	}{
		{
			description:  "Node project matching request projects",
			ctx:          contextWithProjects([]string{"project9"}),
			nodeProjects: []string{"project9"},
			expected:     expectedSuccess,
		},
		{
			description:  "Node project not matching request projects",
			ctx:          contextWithProjects([]string{"project3"}),
			nodeProjects: []string{"project9"},
			expected:     expectedFailure,
		},
		{
			description:  "Node one project; request all projects allowed",
			ctx:          contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			nodeProjects: []string{"project9"},
			expected:     expectedSuccess,
		},
		{
			description:  "Node has no projects; request all projects allowed",
			ctx:          contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			nodeProjects: []string{},
			expected:     expectedSuccess,
		},
		{
			description:  "Node has no projects; request unassigned projects allowed",
			ctx:          contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			nodeProjects: []string{},
			expected:     expectedSuccess,
		},
		{
			description:  "Node has a projects; request unassigned projects allowed",
			ctx:          contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			nodeProjects: []string{"project9"},
			expected:     expectedFailure,
		},
		{
			description:  "Node has a projects; request unassigned and matching project allowed",
			ctx:          contextWithProjects([]string{authzConstants.UnassignedProjectID, "project9"}),
			nodeProjects: []string{"project9"},
			expected:     expectedSuccess,
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			node := iBackend.Node{
				Exists:   true,
				Projects: test.nodeProjects,
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: nodeID,
				},
			}

			suite.IngestNodeAttributes(nodesAttributeIngested)
			suite.IngestNodes([]iBackend.Node{node})
			defer suite.DeleteAllDocuments()

			req := &request.Node{NodeId: nodeID}

			res, err := cfgmgmt.GetAttributes(test.ctx, req)
			assert.Nil(t, err)
			assert.Equal(t, test.expected, res)
		})
	}
}
