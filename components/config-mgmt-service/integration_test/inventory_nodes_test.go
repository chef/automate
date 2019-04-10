package integration_test

import (
	"context"
	"fmt"
	"sort"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/golang/protobuf/ptypes"
	google_protobuf1 "github.com/golang/protobuf/ptypes/timestamp"
	log "github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
)

func TestInventoryNodesReturnsEmptyList(t *testing.T) {
	ctx := context.Background()
	req := request.InventoryNodes{}
	expected := &response.InventoryNodes{
		Nodes: make([]*response.InventoryNode, 0),
	}

	res, err := cfgmgmt.GetInventoryNodes(ctx, &req)

	assert.NoError(t, err)
	assert.Equal(t, expected, res)
}

func TestInventoryNodesWithFilterReturnsListWithOneNode(t *testing.T) {
	nodes := twoNodeArray()

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.InventoryNodes{}

	// Filtering by status:success
	req.Filter = []string{"status:success"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one node", req.Filter),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[0]}
			expected := &response.InventoryNodes{
				Nodes: expectedInventoryNodeListFromNodeArray(expectedNodeArray),
			}

			res, err := cfgmgmt.GetInventoryNodes(ctx, &req)
			assert.NoError(t, err)
			assert.Equal(t, expected, res)
		})

	// Filtering by status:missing
	req.Filter = []string{"status:missing"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one node", req.Filter),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[1]}
			expected := &response.InventoryNodes{
				Nodes: expectedInventoryNodeListFromNodeArray(expectedNodeArray),
			}

			res, err := cfgmgmt.GetInventoryNodes(ctx, &req)
			assert.NoError(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestInventoryNodesWithTableDriven(t *testing.T) {
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

	current := time.Now()
	for _, data := range dataNodes {
		for i := 0; i < data.number; i++ {
			data.node.EntityUuid = newUUID()
			data.node.NodeName = namePrefix + fmt.Sprintf("%03d", index)
			node := iBackend.Node{
				NodeInfo: data.node,
				Exists:   true,
				Checkin:  current,
			}
			nodes = append(nodes, node)

			current = current.Add(time.Duration(time.Hour * -1))
			index++
		}
	}
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	sort.Slice(nodes[:], func(i, j int) bool {
		return nodes[i].Checkin.After(nodes[j].Checkin)
	})

	ctx := context.Background()
	cases := map[string]struct {
		request  request.InventoryNodes
		expected *response.InventoryNodes
	}{
		"should return all the nodes": {
			request.InventoryNodes{
				PageSize: 5000,
			},
			&response.InventoryNodes{
				Nodes: expectedInventoryNodeListFromNodeArray(nodes),
			},
		},
		"should return the first 10 nodes": {
			request.InventoryNodes{
				PageSize: 10,
				Sorting: &request.Sorting{
					Order: request.Order_desc,
				},
			},
			&response.InventoryNodes{
				Nodes: expectedInventoryNodeListFromNodeArray(nodes[0:10]),
			},
		},
		"should return all org2 nodes": {
			request.InventoryNodes{
				PageSize: 1000,
				Filter:   []string{"organization:org2"},
			},
			&response.InventoryNodes{
				Nodes: expectedInventoryNodeListFromNodeArray(filterNodesArray(nodes, "organization:org2")),
			},
		},
		"should return page 2": {
			request.InventoryNodes{
				PageSize:   10,
				CursorDate: toTimestamp(t, nodes[9].Checkin),
				CursorId:   nodes[9].EntityUuid,
				Sorting: &request.Sorting{
					Order: request.Order_desc,
				},
			},
			&response.InventoryNodes{
				Nodes: expectedInventoryNodeListFromNodeArray(nodes[10:20]),
			},
		},
		"should return NO nodes (No missing windows nodes)": {
			request.InventoryNodes{
				Filter: []string{"status:missing", "platform:windows"},
			},
			&response.InventoryNodes{
				Nodes: make([]*response.InventoryNode, 0),
			},
		},
	}

	// Run all the cases!
	for desc, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, desc),
			func(t *testing.T) {
				res, err := cfgmgmt.GetInventoryNodes(ctx, &test.request)
				assert.NoError(t, err)
				assert.ElementsMatch(t, test.expected.Nodes, res.Nodes)
			})
	}
}

func toTimestamp(t *testing.T, date time.Time) *google_protobuf1.Timestamp {
	timestamp, err := ptypes.TimestampProto(date)
	assert.NoError(t, err)
	return timestamp
}

func expectedInventoryNodeListFromNodeArray(nodes []iBackend.Node) []*response.InventoryNode {
	inventoryNodes := make([]*response.InventoryNode, len(nodes))
	for i, node := range nodes {

		checkinTimestamp, err := ptypes.TimestampProto(node.Checkin.UTC())
		if err != nil {
			log.WithFields(log.Fields{
				"err":     err,
				"checkin": node.Checkin.UTC().String(),
			}).Warn("Unable to translate checkin time to timestamp proto")
			//Don't add this node because it won't have a checkin time.
			continue
		}

		var lastCCRTimestamp *google_protobuf1.Timestamp

		if !node.LastCCRReceived.IsZero() {
			lastCCRTimestamp, err = ptypes.TimestampProto(node.LastCCRReceived.UTC())
			if err != nil {
				log.WithFields(log.Fields{
					"err":     err,
					"checkin": node.LastCCRReceived.UTC().String(),
				}).Warn("Unable to translate last ccr time to timestamp proto")
				// The checkin time exists, so we still have some useful information. For that
				// reason, we won't skip.
			}
		}

		inventoryNodes[i] = &response.InventoryNode{
			Id:              node.EntityUuid,
			Checkin:         checkinTimestamp,
			Organization:    node.OrganizationName,
			Platform:        node.Platform,
			PlatformFamily:  node.PlatformFamily,
			PlatformVersion: node.PlatformVersion,
			ClientVersion:   node.ChefVersion,
			Ec2InstanceId:   node.Ec2.InstanceId,
			Ec2InstanceType: node.Ec2.InstanceType,
			LastCcrReceived: lastCCRTimestamp,
			Name:            node.NodeName,
			Fqdn:            node.Fqdn,
		}
	}

	return inventoryNodes
}
