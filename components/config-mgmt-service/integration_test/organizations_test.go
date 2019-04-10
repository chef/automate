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
	iBackend "github.com/chef/automate/components/ingest-service/backend"
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
		iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "missing",
				OrganizationName: "cool_org",
			},
			Exists: true,
		},
		iBackend.Node{
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
			&gpStruct.Value{Kind: &gpStruct.Value_StringValue{"awesome_org"}},
			&gpStruct.Value{Kind: &gpStruct.Value_StringValue{"cool_org"}},
		},
	}

	res, err := cfgmgmt.GetOrganizations(ctx, req)

	t.Log("\nwith two nodes should return both organizations")
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
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
