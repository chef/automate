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
	"github.com/chef/automate/lib/stringutils"
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
	ExpandedRunList    backend.ExpandedRunList `csv:"-" json:"expanded_run_list"`
	ErrorMessage       string                  `csv:"Error Message" json:"error_message"`
}

// === missing fields ===
// VersionedCookbooks

// NodeExport streams a json or csv export
func (s *CfgMgmtServer) NodeExport(request *pRequest.NodeExport, stream service.CfgMgmtService_NodeExportServer) error {
	var sendResult exportHandler
	switch request.OutputType {
	case "", "json":
		sendResult = jsonExport(stream)
	case "csv":
		sendResult = csvExport(stream)
	default:
		return status.Error(codes.Unauthenticated, fmt.Sprintf("%s export is not supported", request.OutputType))
	}

	streamCtx := stream.Context()
	deadline, ok := streamCtx.Deadline()
	if !ok {
		deadline = time.Now().Add(5 * time.Minute)
	}
	ctx, cancel := context.WithDeadline(streamCtx, deadline)
	defer cancel()

	nodeFilters, err := stringutils.FormatFiltersWithKeyConverter(request.Filter,
		params.ConvertParamToNodeStateBackendLowerFilter)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	nodeFilters, err = filterByProjects(ctx, nodeFilters)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	// Adding the exists = true filter to the list of filters, because nodes
	// have documents that persist in elasticsearch to hold historical data
	// even after the node no longer exists
	nodeFilters["exists"] = []string{"true"}

	sortField, sortAsc := request.Sorting.GetParameters()

	nodePager := s.nodePager(ctx, sortField, sortAsc, nodeFilters)

	for {
		nodes, err := nodePager()
		if err != nil {
			return status.Errorf(codes.Internal, err.Error())
		}

		err = sendResult(nodes)
		if err != nil {
			return status.Errorf(codes.Internal, "Failed to stream nodes Error: %s", err.Error())
		}

		if len(nodes) == 0 {
			break
		}
	}

	return nil
}

func (s *CfgMgmtServer) nodePager(ctx context.Context, sortField string, sortAsc bool, nodeFilters map[string][]string) func() ([]backend.Node, error) {
	pageSize := 100
	start := time.Time{}
	end := time.Time{}
	var cursorValue interface{}
	cursorID := ""
	actualSortField := params.ConvertParamToNodeStateBackendLowerFilter(sortField)

	return func() ([]backend.Node, error) {
		nodes, err := s.client.GetNodesPageByCursor(ctx, start, end,
			nodeFilters, cursorValue, cursorID, pageSize, actualSortField, sortAsc)
		if err != nil {
			return []backend.Node{}, err
		}

		if len(nodes) == 0 {
			return []backend.Node{}, nil
		}

		lastNode := nodes[len(nodes)-1]
		cursorID = lastNode.EntityUuid
		cursorValue, err = backend.GetSortableFieldValue(sortField, lastNode)
		if err != nil {
			return []backend.Node{}, err
		}

		return nodes, nil
	}
}

func jsonExport(stream service.CfgMgmtService_NodeExportServer) exportHandler {
	initialRun := true
	return func(nodes []backend.Node) error {
		// If this is the first set of nodes to export, prepend "[" to start a JSON document.
		if initialRun {
			err := stream.Send(&response.ExportData{Content: []byte{'['}})
			if err != nil {
				return err
			}
			initialRun = false
			// If this is not the first set of nodes and the collection has elements,
			// prepend the "," that couldn't get added in the previous call to this function.
		} else if len(nodes) != 0 {
			err := stream.Send(&response.ExportData{Content: []byte{','}})
			if err != nil {
				return err
			}
		}

		// If the collection has no elements, append "]" to close the JSON document and stop.
		if len(nodes) == 0 {
			err := stream.Send(&response.ExportData{Content: []byte{']'}})
			if err != nil {
				return err
			}
			return nil
		}

		writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
			return stream.Send(&response.ExportData{Content: p})
		})
		buf := make([]byte, streamBufferSize)

		for i := range nodes {
			raw, err := json.Marshal(nodeToDisplayNode(nodes[i]))
			if err != nil {
				return fmt.Errorf("Failed to marshal JSON export data: %+v", err)
			}

			// If this is the last element of the collection passed to this function call,
			// there is not enough information to know if this is the last element overall.
			// So, only append "," if it can be determined that there are more elements.
			if i != len(nodes)-1 {
				raw = append(raw, ',')
			}

			reader := bytes.NewReader(raw)

			_, err = io.CopyBuffer(writer, reader, buf)
			if err != nil {
				return fmt.Errorf("Failed to export JSON: %+v", err)
			}
		}

		return nil
	}
}

func csvExport(stream service.CfgMgmtService_NodeExportServer) exportHandler {
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

		return nil
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
		Tags:               node.ChefTags,
		ResourceNames:      node.ResourceNames,
		Recipes:            node.Recipes,
		Cookbooks:          node.Cookbooks,
		UptimeSeconds:      node.UptimeSeconds,
		Roles:              node.Roles,
		PolicyName:         node.PolicyName,
		PolicyGroup:        node.PolicyGroup,
		PolicyRevision:     node.PolicyRevision,
		SourceFqdn:         node.SourceFqdn,
		ExpandedRunList:    backend.ExpandedRunList(node.ExpandedRunList),
		Deprecations:       deprecations,
		ErrorMessage:       node.ErrorMessage,
	}
}
