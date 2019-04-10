//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"google.golang.org/grpc/codes"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
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
	nodesAttribute := []iBackend.NodeAttribute{
		iBackend.NodeAttribute{
			EntityUUID:        "MOCK-UUID",
			Name:              "test-node",
			RunList:           []string{"recipe[the_cookbook::a_recipe]"},
			Default:           "{\"foo\":\"bar\"}",
			Normal:            "{\"normal_foo\":\"normal_bar\"}",
			Override:          "{\"override_thing\":\"overwritten\"}",
			DefaultValueCount: 1, NormalValueCount: 1, OverrideValueCount: 1, AllValueCount: 3,
		},
	}

	suite.IngestNodeAttributes(nodesAttribute)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := &request.Node{NodeId: "MOCK-UUID"}
	expected := &response.NodeAttribute{
		NodeId:           "MOCK-UUID",
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
		requests       = make([]request.Node, len(tests))
		responses      = make([]response.NodeAttribute, len(tests))
	)

	index := 0
	for _, t := range tests {
		id := idPrefix + string(index)
		name := namePrefix + string(index)

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
	defer suite.DeleteAllDocuments()

	// Testing all the requests/responses
	index = 0
	for name, _ := range tests {
		t.Run(name, func(t *testing.T) {
			res, err := cfgmgmt.GetAttributes(ctx, &requests[index])
			assert.Nil(t, err)
			assert.Equal(t, &responses[index], res)
		})
		index++
	}
}
