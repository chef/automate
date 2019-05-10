package grpcserver

import (
	"context"
	"time"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	gp "github.com/golang/protobuf/ptypes/struct"
	"github.com/golang/protobuf/ptypes/timestamp"
	log "github.com/sirupsen/logrus"

	externalResp "github.com/chef/automate/api/external/cfgmgmt/response"
	pRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	interserviceResp "github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/components/config-mgmt-service/params"
)

func (s *CfgMgmtServer) GetRuns(
	ctx context.Context,
	request *pRequest.Runs) (*gp.ListValue, error) {

	var runs = new(gp.ListValue)
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	if request.GetNodeId() == "" {
		return runs, status.Errorf(codes.InvalidArgument, "Parameter 'node_id' not provided")
	}

	filters, err := params.FormatNodeFilters(request.Filter)
	if err != nil {
		return runs, status.Errorf(codes.InvalidArgument, err.Error())
	}

	// Pagination
	page, pageSize := request.GetPagination().GetParameters()

	// Date Range
	if !params.ValidateDateRange(request.GetStart(), request.GetEnd()) {
		return runs, status.Errorf(codes.InvalidArgument, "Invalid start/end time. (format: YYYY-MM-DD)")
	}

	projectFilters, err := filterByProjects(ctx, map[string][]string{})
	if err != nil {
		return runs, status.Errorf(codes.Internal, err.Error())
	}

	nodeExistsChan := s.nodeExistsAsync(request.GetNodeId(), projectFilters)

	bRuns, err := s.client.GetRuns(
		request.GetNodeId(),
		int(page),
		int(pageSize),
		filters,
		request.GetStart(),
		request.GetEnd(),
	)
	if err != nil {
		return runs, status.Errorf(codes.Internal, err.Error())
	}

	if len(bRuns) == 0 {
		return runs, nil
	}

	nodeExists := <-nodeExistsChan
	if nodeExists.err != nil {
		return runs, nodeExists.err
	}

	// Either the user does not have permissions or the node does not exist
	if !nodeExists.exists {
		return runs, nil
	}

	msgArray := backendRunArrayToMessageArray(bRuns)
	err = messageArrayToListValue(msgArray, runs)
	if err != nil {
		return runs, err
	}

	return runs, nil
}

// GetInventoryNodes - get all the inventory nodes
func (s *CfgMgmtServer) GetInventoryNodes(ctx context.Context,
	request *pRequest.InventoryNodes) (*interserviceResp.InventoryNodes, error) {
	pageSize := int(request.PageSize)

	// default the page size to 100
	if pageSize == 0 {
		pageSize = 100
	}

	filters, err := params.FormatNodeFilters(request.Filter)
	if err != nil {
		return &interserviceResp.InventoryNodes{}, status.Errorf(codes.InvalidArgument, err.Error())
	}

	start, err := ToTime(request.Start)
	if err != nil {
		return &interserviceResp.InventoryNodes{}, status.Errorf(codes.InvalidArgument, err.Error())
	}

	end, err := ToTime(request.End)
	if err != nil {
		return &interserviceResp.InventoryNodes{}, status.Errorf(codes.InvalidArgument, err.Error())
	}

	cursorDate, err := ToTime(request.CursorDate)
	if err != nil {
		return &interserviceResp.InventoryNodes{}, status.Errorf(codes.InvalidArgument, err.Error())
	}

	// Sorting
	sortField, sortAsc := request.GetSorting().GetParameters()

	bNodes, err := s.client.GetInventoryNodes(ctx, start, end,
		filters, cursorDate, request.CursorId, pageSize, sortField, sortAsc)
	if err != nil {
		return &interserviceResp.InventoryNodes{}, status.Errorf(codes.Internal, err.Error())
	}

	inventoryNodeCollection := backendNodeArrayToInventoryNodeArray(bNodes)

	return &interserviceResp.InventoryNodes{
		Nodes: inventoryNodeCollection,
	}, nil
}

func (s *CfgMgmtServer) GetNodes(
	ctx context.Context,
	request *pRequest.Nodes) (*gp.ListValue, error) {

	var nodes = new(gp.ListValue)
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	filters, err := params.FormatNodeFilters(request.Filter)
	if err != nil {
		return nodes, status.Errorf(codes.InvalidArgument, err.Error())
	}

	filters, err = filterByProjects(ctx, filters)
	if err != nil {
		return nodes, status.Errorf(codes.Internal, err.Error())
	}

	// Pagination
	page, pageSize := request.GetPagination().GetParameters()

	// Sorting
	sortField, sortAsc := request.GetSorting().GetParameters()

	// TODO: (@afiune) should we change the backend to understand int32 instead?
	bNodes, err := s.client.GetNodes(int(page), int(pageSize), sortField, sortAsc, filters)
	if err != nil {
		return nodes, status.Errorf(codes.Internal, err.Error())
	}

	if len(bNodes) > 0 {
		oldestConvergeIndexDate, indicesExists, err := s.client.GetDateOfOldestConvergeIndices()
		if err != nil {
			return nodes, status.Errorf(codes.Internal, err.Error())
		}

		msgArray := backendNodeArrayToMessageArray(bNodes, oldestConvergeIndexDate, indicesExists)
		err = messageArrayToListValue(msgArray, nodes)
		if err != nil {
			return nodes, status.Errorf(codes.Internal, err.Error())
		}
	}

	return nodes, nil
}

//GetAttributes get node attributes
func (s *CfgMgmtServer) GetAttributes(
	ctx context.Context,
	request *pRequest.Node) (*interserviceResp.NodeAttribute, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	if request.GetNodeId() == "" {
		return &interserviceResp.NodeAttribute{}, status.Errorf(
			codes.InvalidArgument, "Parameter 'node_id' not provided")
	}

	projectFilters, err := filterByProjects(ctx, map[string][]string{})
	if err != nil {
		return &interserviceResp.NodeAttribute{}, status.Errorf(codes.Internal, err.Error())
	}

	// Async check if the node exits with project filters
	nodeExistsChan := s.nodeExistsAsync(request.GetNodeId(), projectFilters)

	attribute, err := s.client.GetAttribute(request.GetNodeId())
	if err != nil {
		if sErr, ok := err.(*errors.StandardError); ok && sErr.Type == errors.NodeAttributeNotFound {
			return &interserviceResp.NodeAttribute{}, status.Errorf(codes.NotFound,
				"No node attributes for given Node ID")
		}

		return &interserviceResp.NodeAttribute{}, status.Errorf(codes.Internal, err.Error())
	}

	nodeExists := <-nodeExistsChan
	if nodeExists.err != nil {
		return &interserviceResp.NodeAttribute{}, nodeExists.err
	}

	// Either the user does not have permissions or the node does not exist
	if !nodeExists.exists {
		return &interserviceResp.NodeAttribute{}, nil
	}

	return &interserviceResp.NodeAttribute{
		NodeId:              attribute.EntityUUID,
		Name:                attribute.Name,
		ChefEnvironment:     attribute.ChefEnvironment,
		RunList:             attribute.RunList,
		Normal:              attribute.Normal,
		NormalValueCount:    int32(attribute.NormalValueCount),
		Default:             attribute.Default,
		DefaultValueCount:   int32(attribute.DefaultValueCount),
		Override:            attribute.Override,
		OverrideValueCount:  int32(attribute.OverrideValueCount),
		Automatic:           attribute.Automatic,
		AutomaticValueCount: int32(attribute.AutomaticValueCount),
		AllValueCount:       int32(attribute.AllValueCount),
	}, nil
}

// backendNodeArrayToMessageArray Casts a 'Backend Node Array' into a 'Proto Message Array'
func backendNodeArrayToMessageArray(nodes []backend.Node,
	oldestConvergeIndexDate time.Time, indicesExists bool) []proto.Message {
	messages := make([]proto.Message, len(nodes))
	for i, node := range nodes {

		timestamp, err := ptypes.TimestampProto(node.Checkin.UTC())
		if err != nil {
			log.WithFields(log.Fields{
				"func":    nameOfFunc(),
				"err":     err,
				"checkin": node.Checkin.UTC().String(),
			}).Warn("Unable to translate checkin time to timestamp proto")
			//Don't add this node because it won't have a checkin time.
		} else {
			var hasRunsData bool
			var lastCCR time.Time
			if !node.LastCCRReceived.IsZero() {
				lastCCR = node.LastCCRReceived
			} else {
				lastCCR = node.Checkin
			}

			if node.LatestRunID != "" && indicesExists &&
				(lastCCR.Equal(oldestConvergeIndexDate) || lastCCR.After(oldestConvergeIndexDate)) {
				hasRunsData = true
			}
			lastCCRProto, err := ptypes.TimestampProto(lastCCR.UTC())

			if err != nil {
				log.WithFields(log.Fields{
					"func":    nameOfFunc(),
					"err":     err,
					"lastCCR": lastCCR.UTC().String(),
				}).Warn("Unable to translate lastCCR time to timestamp proto")
			}

			messages[i] = &externalResp.Node{
				// TODO: (@afiune) Change this to EntityUUID
				Id:                node.EntityUuid,
				Name:              node.NodeName,
				Fqdn:              node.Fqdn,
				Checkin:           timestamp,
				UptimeSeconds:     int32(node.UptimeSeconds),
				Organization:      node.OrganizationName,
				Environment:       node.Environment,
				Platform:          node.Platform,
				PlatformFamily:    node.PlatformFamily,
				PlatformVersion:   node.PlatformVersion,
				Status:            node.Status,
				SourceFqdn:        node.SourceFqdn,
				LatestRunId:       node.LatestRunID,
				PolicyName:        node.PolicyName,
				PolicyGroup:       node.PolicyGroup,
				PolicyRevision:    node.PolicyRevision,
				HasRunsData:       hasRunsData,
				LastCcrReceived:   lastCCRProto,
				HasDeprecations:   node.HasDeprecations,
				ChefVersion:       node.ChefVersion,
				ChefTags:          node.ChefTags,
				DeprecationsCount: int32(node.DeprecationsCount),
			}
		}
	}
	return messages
}

func backendNodeArrayToInventoryNodeArray(nodes []backend.InventoryNode) []*interserviceResp.InventoryNode {
	inventoryNodes := make([]*interserviceResp.InventoryNode, len(nodes))
	for i, node := range nodes {

		checkinTimestamp, err := ptypes.TimestampProto(node.Checkin.UTC())
		if err != nil {
			log.WithFields(log.Fields{
				"func":    nameOfFunc(),
				"err":     err,
				"checkin": node.Checkin.UTC().String(),
			}).Warn("Unable to translate checkin time to timestamp proto")
			//Don't add this node because it won't have a checkin time.
			continue
		}

		var lastCCRTimestamp *timestamp.Timestamp

		// Convert the last CCR received (Time.time) to google.protobuf.Timestamp proto.
		if !node.LastCCRReceived.IsZero() {
			lastCCRTimestamp, err = ptypes.TimestampProto(node.LastCCRReceived.UTC())
			if err != nil {
				log.WithFields(log.Fields{
					"func":    nameOfFunc(),
					"err":     err,
					"checkin": node.LastCCRReceived.UTC().String(),
				}).Warn("Unable to translate last ccr time to timestamp proto")
				// The checkin time exists, so we still have some useful information. For that
				// reason, we won't skip.
			}
		}

		inventoryNodes[i] = &interserviceResp.InventoryNode{
			Id:              node.EntityUUID,
			Checkin:         checkinTimestamp,
			Organization:    node.OrganizationName,
			Platform:        node.Platform,
			PlatformFamily:  node.PlatformFamily,
			PlatformVersion: node.PlatformVersion,
			ClientVersion:   node.ChefVersion,
			Ec2InstanceId:   node.EC2.InstanceId,
			Ec2InstanceType: node.EC2.InstanceType,
			LastCcrReceived: lastCCRTimestamp,
			Name:            node.NodeName,
			Fqdn:            node.Fqdn,
		}

	}
	return inventoryNodes
}

// backendRunArrayToMessageArray Casts a 'Backend Run Array' into a 'Proto Message Array'
func backendRunArrayToMessageArray(runs []backend.AbridgedConverge) []proto.Message {
	messages := make([]proto.Message, len(runs))
	for i, run := range runs {
		messages[i] = &interserviceResp.AbridgedConverge{
			Id:        run.RunID,
			StartTime: run.StartTime,
			EndTime:   run.EndTime,
			Status:    run.Status,
		}
	}
	return messages
}

type nodeExists struct {
	exists bool
	err    error
}

func (s *CfgMgmtServer) nodeExistsAsync(nodeID string, projectFilters map[string][]string) chan nodeExists {
	nodeExistsChan := make(chan nodeExists)
	go func() {
		exists, err := s.client.NodeExists(nodeID, projectFilters)
		nodeExistsChan <- nodeExists{
			exists: exists,
			err:    err,
		}
	}()

	return nodeExistsChan
}
