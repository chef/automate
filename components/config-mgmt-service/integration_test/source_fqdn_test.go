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
		iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: newUUID(),
				Status:     "missing",
				SourceFqdn: "chef-server.example.com",
			},
			Exists: true,
		},
		iBackend.Node{
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
			&gpStruct.Value{
				Kind: &gpStruct.Value_StringValue{"chef-server.example.com"},
			},
			&gpStruct.Value{
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
