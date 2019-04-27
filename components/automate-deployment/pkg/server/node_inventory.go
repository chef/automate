package server

import (
	"context"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"google.golang.org/grpc"

	tslib "github.com/chef/automate/lib/grpc/timestamp"
	configReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/service"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/compliance-service/api/reporting"
)

func (s *server) NodeInventory(ctx context.Context,
	req *api.NodeInventoryRequest) (*api.NodeInventoryResponse, error) {
	var nodeCollection []*api.InventoryNode

	configMgmtAddr := s.AddressForService("config-mgmt-service")
	configMgmtConnection, err := s.connFactory.DialContext(
		ctx,
		"config-mgmt-service",
		configMgmtAddr,
		grpc.WithBlock())
	if err != nil {
		return nil, errors.Wrap(err, "connecting to config-mgmt-service")
	}

	defer configMgmtConnection.Close() // nolint: errcheck

	configMgmtClient := service.NewCfgMgmtClient(configMgmtConnection)

	hourAgo, err := ptypes.TimestampProto(time.Now().Add(-time.Hour))
	if err != nil {
		return nil, errors.Wrap(err, "creating timestamp")
	}

	nodesRequest := &configReq.InventoryNodes{
		PageSize: 100,
		Start:    hourAgo,
		Sorting: &configReq.Sorting{
			Order: configReq.Order_desc,
		},
	}

	inventoryNodes, err := configMgmtClient.GetInventoryNodes(ctx, nodesRequest)
	if err != nil {
		return nil, errors.Wrap(err, "collecting nodes")
	}

	mapConfigMgmtNodes := make(map[string]*api.InventoryNode)
	for len(inventoryNodes.Nodes) > 0 {

		for _, node := range inventoryNodes.Nodes {
			n := &api.InventoryNode{
				Name:            node.Id,
				Organization:    node.Organization,
				Status:          "active",
				PlatformFamily:  node.PlatformFamily,
				Platform:        node.Platform,
				PlatformVersion: node.PlatformVersion,
				Checkin:         tslib.TimestampString(node.Checkin),
				ClientVersion:   node.ClientVersion,
				Ec2InstanceId:   node.Ec2InstanceId,
				Ec2InstanceType: node.Ec2InstanceType,
			}
			// add the node to the configMgmtNodes map.  this is to make it easier to check if a node
			// has already been included in the nodeCollection later when we dedup the list of nodes
			mapConfigMgmtNodes[node.Id] = n
			nodeCollection = append(nodeCollection, n)
		}
		lastNode := inventoryNodes.Nodes[len(inventoryNodes.Nodes)-1]
		nodesRequest.CursorId = lastNode.Id
		nodesRequest.CursorDate = lastNode.Checkin

		inventoryNodes, err = configMgmtClient.GetInventoryNodes(ctx, nodesRequest)
		if err != nil {
			return nil, errors.Wrap(err, "collecting nodes")
		}
	}

	// get list of nodes that were scanned (report landed in compliance reporting), with a
	// report end_time within last hour
	complianceNodes, err := s.getComplianceNodes(ctx, time.Now().Add(-time.Hour))
	if err != nil {
		return nil, errors.Wrap(err, "collecting compliance nodes")
	}
	// check the list of compliance nodes against the map of configmgmt nodes; return list of not-in-map nodes
	inventoryComplianceNodes := includeIfNotAlreadyInList(mapConfigMgmtNodes, complianceNodes)
	// add the compliance nodes to the node collection list
	nodeCollection = append(nodeCollection, inventoryComplianceNodes...)

	return &api.NodeInventoryResponse{
		Nodes: nodeCollection,
	}, nil
}

func (s *server) getComplianceNodes(ctx context.Context, hourAgo time.Time) ([]*reporting.Node, error) {
	reportingNodesList := make([]*reporting.Node, 0)
	lastHourNodesList := make([]*reporting.Node, 0)
	conn, err := s.connFactory.DialContext(ctx, "compliance-service", s.AddressForService("compliance-service"), grpc.WithBlock())
	if err != nil {
		return nil, errors.Wrap(err, "initialize compliance client conn")
	}
	defer conn.Close() // nolint: errcheck

	client := reporting.NewReportingServiceClient(conn)

	var perPage int32 = 100
	var total int32 = 100
	for cnt := int32(1); int32(len(reportingNodesList)) < total; cnt++ {
		query := &reporting.Query{
			Page:    cnt,
			PerPage: perPage,
			Filters: []*reporting.ListFilter{
				{Type: "end_time", Values: []string{time.Now().UTC().Format(time.RFC3339)}},
			},
		}
		// query the compliance reporting nodes for only nodes that have been scanned
		// today. compliance reporting currently only supports day intervals, so here we get
		// all the nodes for today, then filter through those manually to find only
		// the ones that reported in last hour
		reportingNodes, err := client.ListNodes(ctx, query)
		if err != nil {
			return nil, errors.Wrap(err, "listing compliance nodes")
		}
		total = reportingNodes.GetTotal()
		reportingNodesList = append(reportingNodesList, reportingNodes.GetNodes()...)
	}

	for _, node := range reportingNodesList {
		endTime, err := ptypes.Timestamp(node.GetLatestReport().GetEndTime())
		if err != nil {
			return nil, errors.Wrap(err, "checking node timestamp")
		}
		if endTime.After(hourAgo) {
			lastHourNodesList = append(lastHourNodesList, node)
		}
	}

	return lastHourNodesList, nil
}

func includeIfNotAlreadyInList(configMgmtNodes map[string]*api.InventoryNode, complianceNodes []*reporting.Node) []*api.InventoryNode {
	var nodeCollection []*api.InventoryNode
	for _, node := range complianceNodes {
		if _, found := configMgmtNodes[node.Id]; !found {
			n := &api.InventoryNode{
				Name:            node.Id,
				Status:          "active",
				Platform:        node.GetPlatform().GetName(),
				PlatformVersion: node.GetPlatform().GetRelease(),
				Checkin:         tslib.TimestampString(node.GetLatestReport().GetEndTime()),
			}
			nodeCollection = append(nodeCollection, n)
		}
	}
	return nodeCollection
}
