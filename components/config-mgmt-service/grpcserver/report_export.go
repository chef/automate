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
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

type exportReportHandler func([]backend.Run) error

// The fields that will be displayed in a CSV and JSON download
type displayReport struct {
	RunID                string                  `csv:"Run ID" json:"run_id"`
	StartTime            time.Time               `csv:"Start Time" json:"start_time"`
	EndTime              time.Time               `csv:"End Time" json:"end_time"`
	UpdatedResourceCount int                     `csv:"Updated Resource Count" json:"updated_resource_count"`
	Resources            []backend.Resource      `csv:"-" json:"resources"`
	NodeName             string                  `csv:"Node Name" json:"name"`
	ID                   string                  `csv:"Node ID" json:"node_id"`
	Platform             string                  `csv:"Platform" json:"platform"`
	Organization         string                  `csv:"Organization" json:"organization"`
	Environment          string                  `csv:"Environment" json:"environment"`
	Fqdn                 string                  `csv:"Fqdn" json:"fqdn"`
	ClientVersion        string                  `csv:"Client Version" json:"client_version"`
	RunList              []string                `csv:"-" json:"run_list"`
	Source               string                  `csv:"Source" json:"source"`
	Status               string                  `csv:"Status" json:"status"`
	TotalResourceCount   int                     `csv:"Total Resource Count" json:"total_resource_count"`
	Tags                 []string                `csv:"-" json:"tags"`
	ResourceNames        []string                `csv:"-" json:"resource_names"`
	Recipes              []string                `csv:"-" json:"recipes"`
	Cookbooks            []string                `csv:"-" json:"cookbooks"`
	UptimeSeconds        int64                   `csv:"Uptime Seconds" json:"uptime_seconds"`
	Roles                []string                `csv:"-" json:"roles"`
	PolicyName           string                  `csv:"Policy Name" json:"policy_name"`
	PolicyGroup          string                  `csv:"Policy Group" json:"policy_group"`
	PolicyRevision       string                  `csv:"Policy Revision" json:"policy_revision"`
	SourceFqdn           string                  `csv:"Source FQDN" json:"source_fqdn"`
	IpAddress            string                  `csv:"IP Address" json:"ip_address"`
	Deprecations         []backend.Deprecation   `csv:"-" json:"deprecations"`
	ExpandedRunList      backend.ExpandedRunList `csv:"-" json:"expanded_run_list"`
}

// ReportExport streams a json or csv export
func (s *CfgMgmtServer) ReportExport(request *pRequest.ReportExport, stream service.CfgMgmt_ReportExportServer) error {
	exporter, err := getReportExportHandler(request.OutputType, stream)
	if err != nil {
		return err
	}
	streamCtx := stream.Context()
	deadline, ok := streamCtx.Deadline()
	if !ok {
		deadline = time.Now().Add(time.Minute)
	}
	ctx, cancel := context.WithDeadline(streamCtx, deadline)
	defer cancel()
	return s.exportReports(ctx, request, exporter)
}

func getReportExportHandler(outputType string, stream service.CfgMgmt_ReportExportServer) (exportReportHandler, error) {
	switch outputType {
	case "", "json":
		return jsonReportExport(stream), nil
	case "csv":
		return csvReportExport(stream), nil
	default:
		return nil, status.Error(codes.Unauthenticated, fmt.Sprintf(outputType+" export is not supported"))
	}
}

func (s *CfgMgmtServer) exportReports(ctx context.Context, request *pRequest.ReportExport,
	sendResult exportReportHandler) error {
	// Date Range
	start, err := ToTime(request.Start)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	end, err := ToTime(request.End)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	pageSize := 100
	var cursorEndTime time.Time
	cursorID := ""
	sortAsc := false
	runFilters, err := params.FormatNodeFilters(request.Filter)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	log.Infof("runFilters: %v", runFilters)

	runFilters, err = filterByProjects(ctx, runFilters)
	if err != nil {
		return err
	}

	runs, err := s.client.GetRunsPageByCurser(ctx, request.NodeId, start, end,
		runFilters, cursorEndTime, cursorID, pageSize, sortAsc)
	if err != nil {
		return status.Errorf(codes.Internal, err.Error())
	}

	for len(runs) > 0 {
		err = sendResult(runs)
		if err != nil {
			return status.Errorf(codes.Internal, "Failed to stream nodes Error: %s", err.Error())
		}

		lastRun := runs[len(runs)-1]
		cursorID = lastRun.RunID
		cursorEndTime = lastRun.EndTime

		log.Infof("cursorID: %s cursorEndTime: %v", cursorID, cursorEndTime)

		runs, err = s.client.GetRunsPageByCurser(ctx, request.NodeId, start, end,
			runFilters, cursorEndTime, cursorID, pageSize, sortAsc)
		if err != nil {
			return status.Errorf(codes.Internal, err.Error())
		}
	}

	return nil
}

func jsonReportExport(stream service.CfgMgmt_ReportExportServer) exportReportHandler {
	return func(runs []backend.Run) error {
		displayNodeCollection := reportCollectionToDisplayReportCollection(runs)

		raw, err := json.Marshal(displayNodeCollection)
		if err != nil {
			return fmt.Errorf("Failed to marshal JSON export data: %+v", err)
		}

		reader := bytes.NewReader(raw)
		buf := make([]byte, streamBufferSize)

		writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
			return stream.Send(&response.ReportExportData{Content: p})
		})

		_, err = io.CopyBuffer(writer, reader, buf)
		if err != nil {
			return fmt.Errorf("Failed to export JSON: %+v", err)
		}

		return nil
	}
}

func csvReportExport(stream service.CfgMgmt_ReportExportServer) exportReportHandler {
	initialRun := true
	return func(runs []backend.Run) error {
		res, err := runsToCSV(runs)
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
			return stream.Send(&response.ReportExportData{Content: p})
		})
		_, err = io.CopyBuffer(writer, reader, buf)
		if err != nil {
			return fmt.Errorf("Failed to export CSV: %+v", err)
		}

		return err
	}
}

// runsToCSV converts a run to its CSV representation as a string
// which includes a header line.
func runsToCSV(runs []backend.Run) (string, error) {
	displayReportCollection := reportCollectionToDisplayReportCollection(runs)

	// export everything
	content, err := gocsv.MarshalString(&displayReportCollection)
	if err != nil {
		return "", fmt.Errorf("Failed to marshal CSV report: %+v", err)
	}
	return content, nil
}

func reportCollectionToDisplayReportCollection(runs []backend.Run) []displayReport {
	var displayReportCollection = make([]displayReport, len(runs))
	for index, run := range runs {
		displayReportCollection[index] = runToDisplayReport(run)
	}

	return displayReportCollection
}

func runToDisplayReport(run backend.Run) displayReport {
	deprecations := make([]backend.Deprecation, len(run.Deprecations))
	for index, dep := range run.Deprecations {
		deprecations[index] = backend.Deprecation(dep)
	}

	resources := make([]backend.Resource, len(run.Resources))
	for index, resource := range run.Resources {
		resources[index] = backend.Resource(resource)
	}
	var ip string

	ipAddress := net.ParseIP(toString(run.Ipaddress))
	if ipAddress != nil {
		ip = ipAddress.String()
	}

	return displayReport{
		NodeName:             run.NodeName,
		ID:                   run.EntityUuid,
		Platform:             run.Platform,
		Environment:          run.Environment,
		Organization:         run.OrganizationName,
		Fqdn:                 run.Fqdn,
		IpAddress:            ip,
		ClientVersion:        run.ChefVersion,
		RunList:              run.RunList,
		Source:               run.Source,
		Status:               run.Status,
		TotalResourceCount:   run.TotalResourceCount,
		Tags:                 run.ChefTags,
		ResourceNames:        run.ResourceNames,
		Recipes:              run.Recipes,
		Cookbooks:            run.Cookbooks,
		UptimeSeconds:        run.UptimeSeconds,
		Roles:                run.Roles,
		PolicyName:           run.PolicyName,
		PolicyGroup:          run.PolicyGroup,
		PolicyRevision:       run.PolicyRevision,
		SourceFqdn:           run.SourceFqdn,
		ExpandedRunList:      backend.ExpandedRunList(run.ExpandedRunList),
		Deprecations:         deprecations,
		RunID:                run.RunID,
		StartTime:            run.StartTime,
		EndTime:              run.EndTime,
		UpdatedResourceCount: run.UpdatedResourceCount,
		Resources:            resources,
	}
}
