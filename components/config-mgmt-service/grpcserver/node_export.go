package grpcserver

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"strings"
	"time"

	pRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/params"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/gocarina/gocsv"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// Chosen somewhat arbitrarily to be a "good enough" value.
// See: https://github.com/chef/automate/pull/1143#discussion_r170428374
const streamBufferSize = 262144

type exportHandler func([]backend.Node) error

// The fields that will be displayed in a CSV and JSON download
type displayNode struct {
	NodeName           string                  `csv:"Node Name" json:"name"`
	ID                 string                  `csv:"ID" json:"id"`
	Platform           string                  `csv:"Platform" json:"platform"`
	Organization       string                  `csv:"Organization" json:"organization"`
	Environment        string                  `csv:"Environment" json:"environment"`
	Checkin            time.Time               `csv:"Checkin" json:"checkin"`
	LastCCRReceived    time.Time               `csv:"Last CCR Received" json:"last_ccr_received"`
	Fqdn               string                  `csv:"Fqdn" json:"fqdn"`
	ClientVersion      string                  `csv:"Client Version" json:"client_version"`
	Ec2InstanceID      string                  `csv:"EC2 Instance ID" json:"ec2_instance_id"`
	Ec2InstanceType    string                  `csv:"EC2 Instance Type" json:"ec2_instance_type"`
	Attributes         []string                `csv:"-" json:"attributes"`
	CloudID            string                  `csv:"Cloud ID" json:"cloud_id"`
	RunList            []string                `csv:"-" json:"run_list"`
	Source             string                  `csv:"Source" json:"source"`
	Status             string                  `csv:"Status" json:"status"`
	TotalResourceCount int                     `csv:"Total Resource Count" json:"total_resource_count"`
	Tags               []string                `csv:"-" json:"tags"`
	ResourceNames      []string                `csv:"-" json:"resource_names"`
	Recipes            []string                `csv:"-" json:"recipes"`
	Cookbooks          []string                `csv:"-" json:"cookbooks"`
	UptimeSeconds      int64                   `csv:"Uptime Seconds" json:"uptime_seconds"`
	Roles              []string                `csv:"-" json:"roles"`
	PolicyName         string                  `csv:"Policy Name" json:"policy_name"`
	PolicyGroup        string                  `csv:"Policy Group" json:"policy_group"`
	PolicyRevision     string                  `csv:"Policy Revision" json:"policy_revision"`
	SourceFqdn         string                  `csv:"Source FQDN" json:"source_fqdn"`
	IpAddress          string                  `csv:"IP Address" json:"ip_address"`
	Deprecations       []backend.Deprecation   `csv:"-" json:"deprecations"`
	Error              backend.ChefError       `csv:"-" json:"error"`
	ExpandedRunList    backend.ExpandedRunList `csv:"-" json:"expanded_run_list"`
}

// === missing fields ===
// VersionedCookbooks

// NodeExport streams a json or csv export
func (s *CfgMgmtServer) NodeExport(request *pRequest.NodeExport, stream service.CfgMgmt_NodeExportServer) error {
	exporter, err := getExportHandler(request.OutputType, stream)
	if err != nil {
		return err
	}

	return s.exportNodes(stream.Context(), request, exporter)
}

func getExportHandler(outputType string, stream service.CfgMgmt_NodeExportServer) (exportHandler, error) {
	switch outputType {
	case "", "json":
		return jsonExport(stream), nil
	case "csv":
		return csvExport(stream), nil
	default:
		return nil, status.Error(codes.Unauthenticated, fmt.Sprintf(outputType+" export is not supported"))
	}
}

func (s *CfgMgmtServer) exportNodes(ctx context.Context, request *pRequest.NodeExport,
	sendResult exportHandler) error {
	pageSize := 100
	start := time.Time{}
	end := time.Time{}
	var cursorField interface{}
	cursorID := ""
	sortField, sortAsc := request.Sorting.GetParameters()
	nodeFilters, err := params.FormatNodeFilters(request.Filter)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	nodeFilters, err = filterByProjects(ctx, nodeFilters)
	if err != nil {
		return err
	}

	// Adding the exists = true filter to the list of filters, because nodes
	// have documents that persist in elasticsearch to hold historical data
	// even after the node no longer exists
	nodeFilters["exists"] = []string{"true"}

	if sortField == "" {
		sortField = backend.CheckIn
	} else {
		sortField = params.ConvertParamToNodeRunBackend(sortField)
	}

	nodes, err := s.client.GetNodesPageByCurser(ctx, start, end,
		nodeFilters, cursorField, cursorID, pageSize, sortField, sortAsc)
	if err != nil {
		return status.Errorf(codes.Internal, err.Error())
	}

	for len(nodes) > 0 {
		err = sendResult(nodes)
		if err != nil {
			return status.Errorf(codes.Internal, "Failed to stream nodes Error: %s", err.Error())
		}

		lastNode := nodes[len(nodes)-1]
		cursorID = lastNode.EntityUuid
		cursorField, err = backend.GetSortableFieldValue(sortField, lastNode)
		if err != nil {
			return err
		}

		nodes, err = s.client.GetNodesPageByCurser(ctx, start, end,
			nodeFilters, cursorField, cursorID, pageSize, sortField, sortAsc)
		if err != nil {
			return status.Errorf(codes.Internal, err.Error())
		}
	}

	return nil
}

func jsonExport(stream service.CfgMgmt_NodeExportServer) exportHandler {
	return func(nodes []backend.Node) error {
		displayNodeCollection := nodeCollectionToDisplayNodeCollection(nodes)

		raw, err := json.Marshal(displayNodeCollection)
		if err != nil {
			return fmt.Errorf("Failed to marshal JSON export data: %+v", err)
		}

		reader := bytes.NewReader(raw)
		buf := make([]byte, streamBufferSize)

		writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
			return stream.Send(&response.ExportData{Content: p})
		})

		_, err = io.CopyBuffer(writer, reader, buf)
		if err != nil {
			return fmt.Errorf("Failed to export JSON: %+v", err)
		}

		return nil
	}
}

func csvExport(stream service.CfgMgmt_NodeExportServer) exportHandler {
	initialRun := true
	return func(data []backend.Node) error {
		res, err := nodesToCSV(data)
		if err != nil {
			return err
		}

		if initialRun {
			initialRun = false
		} else {
			splits := strings.SplitAfterN(res, "\n", 2)
			if len(splits) == 2 {
				res = splits[1]
			}
		}

		reader := bytes.NewReader([]byte(res))
		buf := make([]byte, streamBufferSize)

		writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
			return stream.Send(&response.ExportData{Content: p})
		})
		_, err = io.CopyBuffer(writer, reader, buf)
		if err != nil {
			return fmt.Errorf("Failed to export CSV: %+v", err)
		}

		return err
	}
}

// nodesToCSV converts a node to its CSV representation as a string
// which includes a header line.
func nodesToCSV(nodes []backend.Node) (string, error) {
	displayNodeCollection := nodeCollectionToDisplayNodeCollection(nodes)

	// export everything
	content, err := gocsv.MarshalString(&displayNodeCollection)
	if err != nil {
		return "", fmt.Errorf("Failed to marshal CSV report: %+v", err)
	}
	return content, nil
}

func nodeCollectionToDisplayNodeCollection(nodeCollection []backend.Node) []displayNode {
	var displayNodeCollection = make([]displayNode, len(nodeCollection))
	for index, node := range nodeCollection {
		displayNodeCollection[index] = nodeToDisplayNode(node)
	}

	return displayNodeCollection
}

func nodeToDisplayNode(node backend.Node) displayNode {
	deprecations := make([]backend.Deprecation, len(node.Deprecations))
	for index, dep := range node.Deprecations {
		deprecations[index] = backend.Deprecation(dep)
	}
	var ip string

	ipAddress := net.ParseIP(toString(node.Ipaddress))
	if ipAddress != nil {
		ip = ipAddress.String()
	}

	return displayNode{
		NodeName:           node.NodeName,
		ID:                 node.EntityUuid,
		Platform:           node.Platform,
		Environment:        node.Environment,
		Organization:       node.OrganizationName,
		Checkin:            node.Checkin,
		Fqdn:               node.Fqdn,
		IpAddress:          ip,
		ClientVersion:      node.ChefVersion,
		Ec2InstanceID:      node.Ec2.InstanceId,
		Ec2InstanceType:    node.Ec2.InstanceType,
		LastCCRReceived:    node.LastCCRReceived,
		Attributes:         node.Attributes,
		CloudID:            node.CloudID,
		RunList:            node.RunList,
		Source:             node.Source,
		Status:             node.Status,
		TotalResourceCount: node.TotalResourceCount,
		Tags:               node.Tags,
		ResourceNames:      node.ResourceNames,
		Recipes:            node.Recipes,
		Cookbooks:          node.Cookbooks,
		UptimeSeconds:      node.UptimeSeconds,
		Roles:              node.Roles,
		PolicyName:         node.PolicyName,
		PolicyGroup:        node.PolicyGroup,
		PolicyRevision:     node.PolicyRevision,
		SourceFqdn:         node.SourceFqdn,
		Error:              backend.ChefError(node.Error),
		ExpandedRunList:    backend.ExpandedRunList(node.ExpandedRunList),
		Deprecations:       deprecations,
	}
}
