package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	sorter "sort"
	"strings"
	"time"

	"github.com/chef/automate/api/external/lib/errorutils"
	ingest "github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	reportmanager "github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
	"google.golang.org/protobuf/types/known/structpb"
)

// Chosen somewhat arbitrarily to be a "good enough" value.
// See: https://github.com/chef/automate/pull/1143#discussion_r170428374
const streamBufferSize = 2097152

// Server implementation for reporting
type Server struct {
	es                         *relaxting.ES2Backend
	reportMgr                  reportmanager.ReportManagerServiceClient
	lcr_open_search_requests   int
	db                         *pgdb.DB
	isEnhancedReportingEnabled bool
}

// New creates a new server
func New(es *relaxting.ES2Backend, rm reportmanager.ReportManagerServiceClient, lcrOpenSearchRequest int, db *pgdb.DB,
	isEnhancedReportingEnabled bool) *Server {
	server := Server{
		es:                         es,
		lcr_open_search_requests:   lcrOpenSearchRequest,
		db:                         db,
		isEnhancedReportingEnabled: isEnhancedReportingEnabled,
	}
	if rm != nil {
		server.reportMgr = rm
	}
	return &server
}

// ListReports returns a list of reports based on query
func (srv *Server) ListReports(ctx context.Context, in *reporting.Query) (*reporting.ReportsSummaryLevelOne, error) {
	var reports reporting.ReportsSummaryLevelOne
	var SORT_FIELDS = map[string]string{
		"node_name":                              "node_name.lower",
		"latest_report.end_time":                 "end_time",
		"latest_report.status":                   "status",
		"latest_report.controls.failed.total":    "controls_sums.failed.total",
		"latest_report.controls.failed.critical": "controls_sums.failed.critical",
	}
	pageInfo, err := validatePaginationAndSorting(in, SORT_FIELDS, "latest_report.end_time")
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}
	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err = filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	reportsList, total, err := srv.es.GetReports(pageInfo.from, pageInfo.perPage, formattedFilters, pageInfo.sort, pageInfo.asc)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	reports.Reports = reportsList
	reports.Total = int32(total)
	return &reports, nil
}

// ListReportIds returns a list of reports based on query
func (srv *Server) ListReportIds(ctx context.Context, in *reporting.Query) (*reporting.ReportIds, error) {
	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, err
	}

	err = validateReportQueryParams(formattedFilters)
	if err != nil {
		return nil, err
	}

	// Step 1: Retrieving the latest report ID for each node based on the provided filters
	esIndex, err := relaxting.GetEsIndex(formattedFilters, false)
	if err != nil {
		return nil, status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	reportIDs, err := srv.es.GetReportIds(esIndex, formattedFilters)
	if err != nil {
		return nil, status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	return reportIDs, nil
}

// ReadReport returns a report based on id
func (srv *Server) ReadReport(ctx context.Context, in *reporting.Query) (*reporting.Report, error) {
	formattedFilters := formatFilters(in.Filters)
	logrus.Debugf("ReadReport called with filters %+v", formattedFilters)
	//todo - deep filtering - should we open this up to more than just one?  only for ReadReport?
	if len(formattedFilters["profile_id"]) > 1 {
		return nil, status.Error(codes.InvalidArgument, "Only one 'profile_id' filter is allowed")
	}
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		logrus.Info(errorutils.FormatErrorMsg(err, in.Id))
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	report, err := srv.es.GetReport(in.Id, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	return report, nil
}

// ReadNodeHeader takes the report Id as input and returns a report showing specific header details of report.
func (srv *Server) ReadNodeHeader(ctx context.Context, in *reporting.Query) (*reporting.NodeHeaderInfo, error) {
	formattedFilters := formatFilters(in.Filters)
	logrus.Debugf("ReadNodeHeader called with filters %+v", formattedFilters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	//todo - deep filtering - should we open this up to more than just one?  only for ReadReport?
	if len(formattedFilters["profile_id"]) > 1 {
		return nil, status.Error(codes.InvalidArgument, "Only one 'profile_id' filter is allowed")
	}
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	report, err := srv.es.GetNodeInfoFromReportID(in.Id, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	return report, nil
}

// ListSuggestions returns a list of suggestions based on query
func (srv *Server) ListSuggestions(ctx context.Context, in *reporting.SuggestionRequest) (*reporting.Suggestions, error) {
	var suggestions reporting.Suggestions
	if in.Size == 0 {
		if strings.HasPrefix(in.Type, "control") {
			//for control or control_tag_key or control_tag_value, we only want 10 back by default
			in.Size = 10
		} else {
			in.Size = 100
		}
	}
	if in.Type == "" {
		return nil, status.Error(codes.InvalidArgument, "Parameter 'type' not specified")
	}
	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	suggestionsList, err := srv.es.GetSuggestions(ctx, in.Type, formattedFilters, in.Text, in.Size, in.TypeKey)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	suggestions.Suggestions = suggestionsList
	return &suggestions, nil
}

// ListProfiles returns a list of profiles based on query
func (srv *Server) ListProfiles(ctx context.Context, in *reporting.Query) (*reporting.ProfileMins, error) {
	var profileMins reporting.ProfileMins
	var SORT_FIELDS = map[string]string{
		"name":  "name.lower",
		"title": "title.lower",
	}
	pageInfo, err := validatePaginationAndSorting(in, SORT_FIELDS, "title")
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err = filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	profiles, counts, err := srv.es.GetAllProfilesFromNodes(pageInfo.from, pageInfo.perPage, formattedFilters, pageInfo.sort, pageInfo.asc)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	profileMins.Counts = counts
	profileMins.Profiles = profiles
	return &profileMins, nil
}

// ListControlItems returns a list of controlListItems based on query
func (srv *Server) ListControlItems(ctx context.Context, in *reporting.ControlItemRequest) (*reporting.ControlItems, error) {
	var controlListItems *reporting.ControlItems
	if in.Size == 0 {
		in.Size = 100
	}
	if in.PageNumber == 0 {
		in.PageNumber = 1
	}

	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	controlListItems, err = srv.es.GetControlListItems(ctx, formattedFilters, in.Size, in.PageNumber)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	return controlListItems, nil
}

// ListControlInfo returns a list of controlListItems based on query
func (srv *Server) ListControlInfo(ctx context.Context, in *reporting.Query) (*reporting.ControlElements, error) {
	var nodeControls *reporting.ControlElements

	formattedFilters := formatFilters(in.Filters)
	logrus.Debugf("ListControlInfo called with filters %+v", formattedFilters)
	//todo - deep filtering - should we open this up to more than just one?  only for ReadReport?
	if len(formattedFilters["profile_id"]) > 1 {
		return nil, status.Error(codes.InvalidArgument, "Only one 'profile_id' filter is allowed")
	}
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	nodeControls, err = srv.es.GetNodeControlListItems(ctx, formattedFilters, in.Id)
	if err != nil {
		logrus.Errorf("******* ERROR: %+v", err)
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	return nodeControls, nil
}

type exportHandler func(*reporting.Report) error

// Export streams a json or csv export
func (srv *Server) Export(in *reporting.Query, stream reporting.ReportingService_ExportServer) error {
	formattedFilters := formatFilters(in.Filters)
	logrus.Debugf("Export called with filters %+v", formattedFilters)
	formattedFilters, err := filterByProjects(stream.Context(), formattedFilters)
	if err != nil {
		return err
	}

	err = validateReportQueryParams(formattedFilters)
	if err != nil {
		return err
	}

	exporter, err := getExportHandler(in.Type, stream)
	if err != nil {
		return err
	}

	// Step 1: Retrieving the latest report ID for each node based on the provided filters
	esIndex, err := relaxting.GetEsIndex(formattedFilters, false)
	if err != nil {
		return status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	reportIDs, err := srv.es.GetReportIds(esIndex, formattedFilters)
	if err != nil {
		return status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	if reportIDs == nil {
		return nil
	}

	total := len(reportIDs.Ids)
	// Step 2: get all reports one by one in reverse order to be sorted asc by end_time
	for idx := total - 1; idx >= 0; idx-- {
		cur, err := srv.es.GetReport(reportIDs.Ids[idx], formattedFilters)
		if err != nil {
			return status.Error(codes.NotFound, fmt.Sprintf("Failed to retrieve report %d/%d with ID %s . Error: %s", idx, total, reportIDs.Ids[idx], err))
		}
		err = exporter(cur)
		if err != nil {
			return status.Error(codes.Internal, fmt.Sprintf("Failed to stream report %d/%d with ID %s . Error: %s", idx, total, reportIDs.Ids[idx], err))
		}
	}

	return nil
}

// ExportReportManager populate the report manager request and sent for processing
func (srv *Server) ExportReportManager(ctx context.Context, in *reporting.Query) (*reporting.CustomReportResponse, error) {
	if srv.reportMgr == nil {
		return nil, status.Error(codes.PermissionDenied, "customer not enabled for large reporting")
	}

	responseID := &reporting.CustomReportResponse{}
	formattedFilters := formatFilters(in.Filters)
	logrus.Debugf("Export called with filters %+v", formattedFilters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return responseID, err
	}

	err = validateReportQueryParams(formattedFilters)
	if err != nil {
		return responseID, err
	}

	//get requestor information from context
	requestorID, err := auth_context.RequestorFromIncomingContext(ctx)
	if err != nil {
		return nil, status.Error(codes.NotFound, fmt.Sprintf("error in getting the requestor info from context %s", err.Error()))
	}
	if requestorID == "" {
		return nil, status.Error(codes.NotFound, "missing requestor information in the context")
	}

	//convert formattedFilters to reportmanager.ListFilter
	respFilters := []*reportmanager.ListFilter{}
	for filter, values := range formattedFilters {
		respFilters = append(respFilters, &reportmanager.ListFilter{
			Type:   filter,
			Values: values,
		})
	}

	// get all report manager requests one by one.
	reportMgrRequests := &reportmanager.CustomReportRequest{
		RequestorId: requestorID,
		ReportType:  in.Type,
		Filters:     respFilters,
	}

	// send request to report manager
	reportMgrResponse, err := srv.reportMgr.PrepareCustomReport(ctx, reportMgrRequests)
	if err != nil {
		return responseID, status.Error(codes.Internal, fmt.Sprintf("Failed to retrieve report manager acknowledgement: %s", err))
	}
	responseID.AcknowledgementId = reportMgrResponse.AcknowledgementId

	return responseID, nil
}

func (srv *Server) GetReportListForReportManager(filters *reporting.ListFilters, stream reporting.ReportingService_GetReportListForReportManagerServer) error {
	formattedFilters := make(map[string][]string)

	for _, filter := range filters.Filters {
		formattedFilters[filter.Type] = filter.GetValues()
	}

	// Retrieving the latest report ID for each node based on the provided filters
	esIndex, err := relaxting.GetEsIndex(formattedFilters, false)
	if err != nil {
		return status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	reportIDs, err := srv.es.GetReportIds(esIndex, formattedFilters)
	if err != nil {
		return status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	if reportIDs == nil || len(reportIDs.Ids) == 0 {
		return status.Error(codes.NotFound, "No reports found")
	}

	total := len(reportIDs.Ids)
	resp := reporting.ReportListForReportManagerResponse{}

	cReportRequest := make(chan ReportManagerChan)
	cErr := make(chan ReportManagerErrorChan)
	respMap := make(map[int]ReportManagerChan)
	errsMap := make(map[int]ReportManagerErrorChan)

	fetchInterval := srv.lcr_open_search_requests
	for i := 0; i < total; i = i + fetchInterval {
		count := 0
		if i+fetchInterval < total {
			count = fetchInterval
		} else {
			count = total - i
		}
		logrus.Infof("fetching %d - %d reports of total %d", i+0, i+count-1, total)
		for idx := 0; idx < count; idx++ {
			go srv.getReportManagerReqForNode(i+idx, reportIDs.Ids[i+idx], formattedFilters, cReportRequest, cErr)
		}

		for idx := 0; idx < count; idx++ {
			select {
			case cResponse := <-cReportRequest:
				respMap[cResponse.index] = cResponse
			case errorResponse := <-cErr:
				errsMap[errorResponse.index] = errorResponse
			}
		}
	}

	if len(errsMap) > 0 {
		var errString string
		for key, value := range errsMap {
			errString = fmt.Sprintf("%s.\n failed to retrieve report %d/%d with ID %s . Error: %s", errString, key, total, value.reportID, value.err.Error())
		}
		//return responseID, fmt.Errorf(errString)
		return status.Errorf(codes.Internal, errString)
	}

	for idx := total - 1; idx >= 0; idx-- {
		if result, ok := respMap[idx]; ok {
			resp.Reports = append(resp.Reports, result.reportResp)
		}
	}

	//return &resp, nil
	jsonBytes, err := json.Marshal(resp)
	if err != nil {
		return status.Errorf(codes.Internal, "error in marshalling report list to report manager: %s", err)
	}

	reader := bytes.NewReader(jsonBytes)
	buffer := make([]byte, streamBufferSize)

	for {
		n, err := reader.Read(buffer)
		if err == io.EOF {
			break
		}
		if err != nil {
			return status.Errorf(codes.Internal, "error in reading the report from reader to buffer: %s", err)
		}
		request := &reporting.ReportContentResponse{Content: buffer[:n]}
		logrus.Debugf("sending %d bytes", n)
		err = stream.Send(request)
		if err != nil {
			return status.Errorf(codes.Internal, "Unable to send report stream: %s", err)
		}
	}
	return nil
}

type ReportManagerChan struct {
	index      int
	reportID   string
	reportResp *reporting.ReportResponse
}

type ReportManagerErrorChan struct {
	index    int
	reportID string
	err      error
}

func (srv *Server) getReportManagerReqForNode(index int, reportID string,
	formattedFilters map[string][]string, respChan chan ReportManagerChan,
	cErr chan ReportManagerErrorChan) {

	cur, err := srv.es.GetReportManagerRequest(reportID, formattedFilters)
	if err != nil {
		errResp := ReportManagerErrorChan{
			index:    index,
			reportID: reportID,
			err:      err,
		}
		cErr <- errResp
		return
	}
	resp := ReportManagerChan{
		index:      index,
		reportID:   reportID,
		reportResp: cur,
	}
	respChan <- resp
}

func validateReportQueryParams(formattedFilters map[string][]string) error {
	if len(formattedFilters["profile_name"]) > 0 && len(formattedFilters["profile_id"]) > 0 {
		return status.Error(codes.InvalidArgument, "Invalid: Cannot specify both 'profile_name' and 'profile_id' filters")
	}

	if len(formattedFilters["profile_name"]) > 1 {
		return status.Error(codes.InvalidArgument, "Invalid: Only one 'profile_name' filter is allowed")
	}

	if len(formattedFilters["profile_id"]) > 1 {
		return status.Error(codes.InvalidArgument, "Invalid: Only one 'profile_id' filter is allowed")
	}
	return nil
}

func (srv *Server) ExportNode(in *reporting.Query, stream reporting.ReportingService_ExportNodeServer) error {
	formattedFilters := formatFilters(in.Filters)
	logrus.Debugf("ExportNode called with filters %+v", formattedFilters)
	formattedFilters, err := filterByProjects(stream.Context(), formattedFilters)
	if err != nil {
		return err
	}

	err = validateReportQueryParams(formattedFilters)
	if err != nil {
		return err
	}

	if len(formattedFilters["node_id"]) != 1 {
		return status.Error(codes.InvalidArgument, "Invalid: Must provide only one 'node_id' filter")
	}

	exporter, err := getExportHandler(in.Type, stream)
	if err != nil {
		return err
	}
	logrus.Debugf("getting the report ids for node %s", formattedFilters["node_id"])

	// Step 1: Retrieving all report IDs for the node, based on time filters. Only ask for 9999 results b/c es will choke if we ask for 10k
	reportsList, _, err := srv.es.GetReports(0, 9999, formattedFilters, "end_time", false)
	if err != nil {
		return errorutils.FormatErrorMsg(err, "")
	}

	logrus.Debugf("found %d reports for node %s", len(reportsList), formattedFilters["node_id"])

	// Step 2: get all reports one by one
	for _, report := range reportsList {
		cur, err := srv.es.GetReport(report.GetId(), formattedFilters)
		if err != nil {
			return status.Error(codes.NotFound, fmt.Sprintf("Failed to retrieve report with ID %s. Error: %s", report.GetId(), err))
		}
		err = exporter(cur)
		if err != nil {
			return status.Error(codes.Internal, fmt.Sprintf("Failed to stream report with ID %s. Error: %s", report.GetId(), err))
		}
	}

	return nil
}

func getExportHandler(format string, stream reporting.ReportingService_ExportServer) (exportHandler, error) {
	switch format {
	case "", "json":
		return jsonExport(stream), nil
	case "csv":
		return csvExport(stream), nil
	default:
		return nil, status.Error(codes.Unauthenticated, fmt.Sprintf(format+" export is not supported"))
	}
}

func jsonExport(stream reporting.ReportingService_ExportServer) exportHandler {
	initialRun := true
	return func(data *reporting.Report) error {
		raw, err := json.Marshal(data)
		if err != nil {
			return fmt.Errorf("failed to marshal JSON export data: %+v", err)
		}

		if initialRun {
			initialRun = false
		} else {
			raw = append([]byte(","), raw...)
		}
		reader := bytes.NewReader(raw)
		buf := make([]byte, streamBufferSize)

		writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
			return stream.Send(&reporting.ExportData{Content: p})
		})
		_, err = io.CopyBuffer(writer, reader, buf)
		if err != nil {
			return fmt.Errorf("failed to export JSON: %+v", err)
		}

		return nil
	}
}

func csvExport(stream reporting.ReportingService_ExportServer) exportHandler {
	initialRun := true
	return func(data *reporting.Report) error {
		res, err := util.ReportToCSV(data)
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
			return stream.Send(&reporting.ExportData{Content: p})
		})
		_, err = io.CopyBuffer(writer, reader, buf)
		if err != nil {
			return fmt.Errorf("failed to export CSV: %+v", err)
		}

		return nil
	}
}

// ListNodes returns a list of nodes based on query
func (srv *Server) ListNodes(ctx context.Context, in *reporting.Query) (*reporting.Nodes, error) {
	formattedFilters := formatFilters(in.Filters)
	var nodes reporting.Nodes
	var SORT_FIELDS = map[string]string{
		"name":                                   "node_name.lower",
		"environment":                            "environment.lower",
		"platform":                               "platform.full",
		"status":                                 "status",
		"latest_report.status":                   "status",
		"latest_report.end_time":                 "end_time",
		"latest_report.controls.failed.total":    "controls_sums.failed.total",
		"latest_report.controls.failed.critical": "controls_sums.failed.critical",
	}
	pageInfo, err := validatePaginationAndSorting(in, SORT_FIELDS, "latest_report.end_time")
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	formattedFilters, err = filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	nodesList, totalCounts, err := srv.es.GetNodes(pageInfo.from, pageInfo.perPage, formattedFilters, pageInfo.sort, pageInfo.asc)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	nodes.Total = totalCounts.Total
	nodes.TotalPassed = totalCounts.Passed
	nodes.TotalSkipped = totalCounts.Skipped
	nodes.TotalFailed = totalCounts.Failed
	nodes.TotalWaived = totalCounts.Waived
	nodes.Nodes = nodesList
	return &nodes, nil
}

// ReadNode returns a node based on id
func (srv *Server) ReadNode(ctx context.Context, in *reporting.Id) (*reporting.Node, error) {
	formattedFilters := formatFilters([]*reporting.ListFilter{})
	// This method takes no filters, setting the widest start->end time range to find this node
	formattedFilters["start_time"] = []string{"2017-01-01T00:00:00Z"}
	formattedFilters["end_time"] = []string{time.Now().UTC().Format(time.RFC3339)}
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	node, err := srv.es.GetNode(in.Id, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	return node, nil
}

func formatFilters(filters []*reporting.ListFilter) map[string][]string {
	formattedFilters := make(map[string][]string)
	for _, filter := range filters {
		formattedFilters[filter.Type] = append(formattedFilters[filter.Type], filter.Values...)
	}
	utils.DeDupFilters(formattedFilters)
	return formattedFilters
}

type pageSortInfo struct {
	from    int32
	perPage int32
	sort    string
	asc     bool
}

func validatePaginationAndSorting(in *reporting.Query, validSortFields map[string]string, defaultSortField string) (pageSortInfo, error) {
	ret := pageSortInfo{}
	if in.PerPage == 0 {
		in.PerPage = 10
	}
	ret.perPage = in.PerPage

	if in.Page == 0 {
		in.Page = 1
	}
	ret.from = (in.Page - 1) * in.PerPage

	switch in.Order {
	case reporting.Query_ASC:
		ret.asc = true
	case reporting.Query_DESC:
		ret.asc = false
	}

	sortKey := defaultSortField
	if in.Sort != "" {
		sortKey = in.Sort
	}

	sortValue, ok := validSortFields[sortKey]
	if !ok {
		validFields := make([]string, 0, len(validSortFields))
		for k := range validSortFields {
			validFields = append(validFields, k)
		}
		sorter.Strings(validFields)
		return ret, &errorutils.InvalidError{Msg: fmt.Sprintf("Parameter 'sort' only supports one of the following fields: %v", validFields)}
	}
	ret.sort = sortValue

	return ret, nil
}

func filterByProjects(ctx context.Context, filters map[string][]string) (map[string][]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		return filters, nil
	}

	filters["projects"] = projectsFilter
	return filters, nil
}

func (srv *Server) GetReportContent(ctx context.Context, in *reporting.ReportContentRequest) (*reporting.ReportContentResponse, error) {
	if in.GetId() == "" {
		return nil, fmt.Errorf("id should not be empty")
	}
	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errors.Wrap(err, "error in getting the filters by project in getting the report content")
	}
	report, err := srv.es.GetReport(in.GetId(), formattedFilters)
	if err != nil {
		return nil, errors.Wrap(err, "error in getting the report content")
	}

	//convert es report to ingest report format
	resp := ingest.Report{
		Version: report.Version,
		Platform: &inspec.Platform{
			Name:    report.GetPlatform().GetName(),
			Release: report.GetPlatform().GetRelease(),
		},
		Statistics: &inspec.Statistics{
			Duration: report.GetStatistics().GetDuration(),
		},
		ReportUuid:       report.Id,
		NodeUuid:         report.NodeId,
		JobUuid:          report.JobId,
		NodeName:         report.NodeName,
		Environment:      report.Environment,
		Roles:            report.Roles,
		EndTime:          report.GetEndTime().AsTime().Format(time.RFC3339),
		OrganizationName: report.ChefOrganization,
		ChefTags:         report.ChefTags,
		Ipaddress:        report.Ipaddress,
		Fqdn:             report.Fqdn,
		SourceFqdn:       report.ChefServer,
		Status:           report.Status,
		StatusMessage:    report.StatusMessage,
	}

	for _, profile := range report.Profiles {
		ingestProfile := inspec.Profile{
			Name:           profile.Name,
			Title:          profile.Title,
			Version:        profile.Version,
			Summary:        profile.Summary,
			Maintainer:     profile.Maintainer,
			License:        profile.License,
			Copyright:      profile.Copyright,
			CopyrightEmail: profile.CopyrightEmail,
			Sha256:         profile.Sha256,
			Status:         profile.Status,
			SkipMessage:    profile.SkipMessage,
			StatusMessage:  profile.StatusMessage,
		}

		for _, control := range profile.Controls {
			ingestControl := inspec.Control{
				Id:     control.Id,
				Impact: control.Impact,
				Title:  control.Title,
				Code:   control.Code,
				Desc:   control.Desc,
				SourceLocation: &inspec.SourceLocation{
					Ref:  control.GetSourceLocation().GetRef(),
					Line: control.GetSourceLocation().GetLine(),
				},
				//Refs:                 control.Refs,
				WaiverData: &inspec.WaiverData{
					ExpirationDate:     control.GetWaiverData().GetExpirationDate(),
					Justification:      control.GetWaiverData().GetJustification(),
					Run:                control.GetWaiverData().GetRun(),
					SkippedDueToWaiver: control.GetWaiverData().GetSkippedDueToWaiver(),
					Message:            control.GetWaiverData().GetMessage(),
				},
				RemovedResultsCounts: &inspec.RemovedResultsCounts{
					Failed:  control.GetRemovedResultsCounts().GetFailed(),
					Skipped: control.GetRemovedResultsCounts().GetSkipped(),
					Passed:  control.GetRemovedResultsCounts().GetPassed(),
				},
			}

			var fieldMap map[string]interface{}
			err = json.Unmarshal([]byte(control.Tags), &fieldMap)
			if err != nil {
				return nil, errors.Wrap(err, "error in converting the tags")
			}
			if len(fieldMap) > 0 {
				tempStruct, err := structpb.NewStruct(fieldMap)
				if err != nil {
					return nil, errors.Wrap(err, "error in converting the interface to structpb")
				}
				ingestControl.Tags = tempStruct
			}

			for _, result := range control.Results {
				ingestControl.Results = append(ingestControl.Results, &inspec.Result{
					Status:      result.Status,
					CodeDesc:    result.CodeDesc,
					RunTime:     result.RunTime,
					StartTime:   result.StartTime,
					Message:     result.Message,
					SkipMessage: result.SkipMessage,
				})
			}

			ingestProfile.Controls = append(ingestProfile.Controls, &ingestControl)
		}

		for _, support := range profile.Supports {
			ingestProfile.Supports = append(ingestProfile.Supports, &inspec.Support{
				Inspec:   support.InspecVersion,
				OsName:   support.OsName,
				OsFamily: support.OsFamily,
				Release:  support.Release,
				Platform: support.Platform,
			})
		}

		for _, attribute := range profile.Attributes {
			ingestProfile.Attributes = append(ingestProfile.Attributes, &inspec.Attribute{
				Name: attribute.Name,
			})
		}

		for _, dependency := range profile.Depends {
			ingestProfile.Depends = append(ingestProfile.Depends, &inspec.Dependency{
				Name:        dependency.Name,
				Url:         dependency.Url,
				Path:        dependency.Path,
				Git:         dependency.Git,
				Branch:      dependency.Branch,
				Tag:         dependency.Tag,
				Commit:      dependency.Commit,
				Version:     dependency.Version,
				Supermarket: dependency.Supermarket,
				Compliance:  dependency.Compliance,
				Status:      dependency.Status,
				SkipMessage: dependency.SkipMessage,
			})
		}

		for _, group := range profile.Groups {
			ingestProfile.Groups = append(ingestProfile.Groups, &inspec.Group{
				Id:       group.Id,
				Title:    group.Title,
				Controls: group.GetControls(),
			})
		}

		resp.Profiles = append(resp.Profiles, &ingestProfile)
	}

	jsonBytes, err := json.Marshal(resp)
	if err != nil {
		return nil, errors.Wrap(err, "error in marshalling the report content")
	}

	return &reporting.ReportContentResponse{
		Content: jsonBytes,
	}, nil

	/*reader := bytes.NewReader(jsonBytes)
	buffer := make([]byte, streamBufferSize)

	for {
		n, err := reader.Read(buffer)
		if err == io.EOF {
			break
		}
		if err != nil {
			return status.Errorf(codes.Internal, "error in reading the report from reader to buffer: %s", err)
		}
		request := &reporting.ReportContentResponse{Content: buffer[:n]}
		logrus.Debugf("sending %d bytes", n)
		err = stream.Send(request)
		if err != nil {
			return status.Errorf(codes.Internal, "Unable to send report stream: %s", err)
		}
	}
	return nil*/
}

func (srv *Server) AssetCount(ctx context.Context, in *reporting.ListFilters) (*reporting.AssetSummary, error) {
	if !srv.isEnhancedReportingEnabled {
		return nil, status.Error(codes.PermissionDenied, "customer not enabled for enhanced compliance reporting")
	}
	var assets *reporting.AssetSummary

	formattedFilters := formatFilters(in.Filters)

	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		logrus.Errorf("Unable to get filters by filterbyProject: %v", err)
		return nil, err
	}

	assets, err = srv.es.GetAssetSummary(ctx, formattedFilters)
	if err != nil {
		logrus.Errorf("Unable to get the asset summary: %v", err)
		return nil, err
	}

	return assets, nil
}

func (srv *Server) ListAsset(ctx context.Context, in *reporting.AssetListRequest) (*reporting.AssetListResponse, error) {
	if !srv.isEnhancedReportingEnabled {
		return nil, status.Error(codes.PermissionDenied, "customer not enabled for enhanced compliance reporting")
	}
	formattedFilters := formatFilters(in.Filters)
	var asset []*reporting.Assets

	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		logrus.Errorf("Unable to get filters by filterbyProject: %v", err)
		return nil, err
	}
	asset, err = srv.es.GetAsset(ctx, formattedFilters, in.Size, in.From, in.AssetsType)
	if err != nil {
		logrus.Errorf("Unable to get %v from the assets list: %v", in.AssetsType, err)
		return nil, err
	}
	return &reporting.AssetListResponse{Assets: asset}, nil
}

func (srv *Server) GetAssetConfig(ctx context.Context, in *reporting.GetAssetConfigRequest) (*reporting.ComplianceConfigResponse, error) {
	if !srv.isEnhancedReportingEnabled {
		return nil, status.Error(codes.PermissionDenied, "customer not enabled for enhanced compliance reporting")
	}
	result, err := srv.db.GetConfigs(ctx)
	if err != nil {
		logrus.Errorf("error while getting the conf: %+v", err)
		return nil, err
	}

	return result, nil
}

func (srv *Server) SetAssetConfig(ctx context.Context, in *reporting.ComplianceConfigRequest) (*reporting.ComplianceConfigResponse, error) {
	if !srv.isEnhancedReportingEnabled {
		return nil, status.Error(codes.PermissionDenied, "customer not enabled for enhanced compliance reporting")
	}
	err := srv.db.SetConfigs(ctx, in)
	if err != nil {
		logrus.Errorf("error while updating the conf: %+v", err)
		return nil, err
	}

	res, err := srv.GetAssetConfig(ctx, &reporting.GetAssetConfigRequest{})
	if err != nil {
		logrus.Errorf("error while getting the conf: %+v", err)
		return nil, err
	}

	return res, nil
}

// ListControlItemsRange returns a list of controlListItems based on query
func (srv *Server) ListControlItemsRange(ctx context.Context, in *reporting.ControlItemRequest) (*reporting.ControlItems, error) {
	if !srv.isEnhancedReportingEnabled {
		return nil, status.Error(codes.PermissionDenied, "customer not enabled for enhanced compliance reporting")
	}
	var controlListItems *reporting.ControlItems
	if in.Size == 0 {
		in.Size = 100
	}
	if in.PageNumber == 0 {
		in.PageNumber = 1
	}

	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	controlListItems, err = srv.es.GetControlListItemsRange(ctx, formattedFilters, in.Size, in.PageNumber)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	return controlListItems, nil
}
