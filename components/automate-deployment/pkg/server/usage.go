package server

import (
	"context"
	"encoding/json"

	tspb "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/pkg/errors"
	"google.golang.org/grpc"

	configReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/api/interservice/cfgmgmt/service"
	api "github.com/chef/automate/api/interservice/deployment"
	tslib "github.com/chef/automate/lib/grpc/timestamp"
)

func (s *server) Usage(ctx context.Context,
	req *api.UsageRequest) (*api.UsageResponse, error) {
	var nodeCollection []*api.NodeUsage

	// Billing model: we charge on
	// 1. nodes managed by chef (config-mgmt nodes) (implemented)
	// 2. compliance reports triggered by a scan job (not implemented yet)
	startTime := req.StartTime
	nodeCollection, err := s.getConfigMgmtNodes(ctx, startTime)
	if err != nil {
		return nil, err
	}

	return &api.UsageResponse{
		Nodes: nodeCollection,
	}, nil
}

func (s *server) getConfigMgmtNodes(ctx context.Context, startTime *tspb.Timestamp) ([]*api.NodeUsage, error) {
	var nodeCollection []*api.NodeUsage

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

	nodesRequest := &configReq.InventoryNodes{
		PageSize: 100,
		Start:    startTime,
		Sorting: &configReq.Sorting{
			Order: configReq.Order_desc,
		},
	}

	for {
		inventoryNodes, err := configMgmtClient.GetInventoryNodes(ctx, nodesRequest)
		if err != nil {
			return nil, errors.Wrap(err, "collecting config-mgmt nodes")
		}
		if len(inventoryNodes.Nodes) == 0 {
			break
		}

		for _, node := range inventoryNodes.Nodes {
			usageNode, err := populateUsageNode(node)
			if err != nil {
				return nil, errors.Wrap(err, "filling out usage node")
			}

			nodeCollection = append(nodeCollection, usageNode)
		}
		lastNode := inventoryNodes.Nodes[len(inventoryNodes.Nodes)-1]
		nodesRequest.CursorId = lastNode.Id
		nodesRequest.CursorDate = lastNode.Checkin

	}

	return nodeCollection, nil
}

type nodeUsageMetadata struct {
	Name            string `json:"name,omitempty"`
	FQDN            string `json:"fqdn,omitempty"`
	Platform        string `json:"platform,omitempty"`
	PlatformVersion string `json:"platform_version,omitempty"`
	Organization    string `json:"organization,omitempty"`
	PlatformFamily  string `json:"platform_family,omitempty"`
	ClientVersion   string `json:"client_version,omitempty"`
	Ec2InstanceID   string `json:"ec2_instance_id,omitempty"`
	Ec2InstanceType string `json:"ec2_instance_type,omitempty"`
}

func populateUsageNode(node *response.InventoryNode) (*api.NodeUsage, error) {
	lastSeen := tslib.TimestampString(node.Checkin)
	lastCcrReceived := tslib.TimestampString(node.LastCcrReceived)
	checkinType := ""
	if lastSeen == lastCcrReceived {
		checkinType = "chef-client"
	} else {
		checkinType = "liveness-agent"
	}

	in := &nodeUsageMetadata{
		Name:            node.Name,
		FQDN:            node.Fqdn,
		Platform:        node.Platform,
		PlatformVersion: node.PlatformVersion,
		Organization:    node.Organization,
		PlatformFamily:  node.PlatformFamily,
		ClientVersion:   node.ClientVersion,
		Ec2InstanceID:   node.Ec2InstanceId,
		Ec2InstanceType: node.Ec2InstanceType,
	}

	// Deal with potentially empty fields.
	nodeMetadata := make(map[string]string, 0)
	inrec, _ := json.Marshal(in)
	err := json.Unmarshal(inrec, &nodeMetadata)
	if err != nil {
		return nil, err
	}

	n := &api.NodeUsage{
		Id:              node.Id,
		LastSeen:        lastSeen,
		LastCcrReceived: lastCcrReceived,
		CheckinType:     checkinType,
		Metadata:        nodeMetadata,
	}

	return n, nil
}
