package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	sorter "sort"
	"strings"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/io/chunks"
)

// Chosen somewhat arbitrarily to be a "good enough" value.
// See: https://github.com/chef/automate/pull/1143#discussion_r170428374
const streamBufferSize = 262144

// Server implementation for reporting
type Server struct {
	es *relaxting.ES2Backend
}

// New creates a new server
func New(es *relaxting.ES2Backend) *Server {
	return &Server{es: es}
}

// ListReports returns a list of reports based on query
func (srv *Server) ListReports(ctx context.Context, in *reporting.Query) (*reporting.Reports, error) {
	var reports reporting.Reports
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
	var ids reporting.ReportIds

	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, err
	}

	if len(formattedFilters["profile_name"]) > 0 && len(formattedFilters["profile_id"]) > 0 {
		return nil, status.Error(codes.InvalidArgument, "Invalid: Cannot specify both 'profile_name' and 'profile_id' filters")
	}

	if len(formattedFilters["profile_name"]) > 1 {
		return nil, status.Error(codes.InvalidArgument, "Invalid: Only one 'profile_name' filter is allowed")
	}

	if len(formattedFilters["profile_id"]) > 1 {
		return nil, status.Error(codes.InvalidArgument, "Invalid: Only one 'profile_id' filter is allowed")
	}

	if len(formattedFilters["control"]) > 1 {
		return nil, status.Error(codes.InvalidArgument, "Invalid: Only one 'control' filter is allowed")
	}

	// Step 1: Retrieving the latest report ID for each node based on the provided filters
	esIndex, err := relaxting.GetEsIndex(formattedFilters, false, true)
	if err != nil {
		return nil, status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	reportIDs, err := srv.es.GetReportIds(esIndex, formattedFilters)
	if err != nil {
		return nil, status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	ids.Ids = reportIDs

	return &ids, nil
}

// ReadReport returns a reports based on id
func (srv *Server) ReadReport(ctx context.Context, in *reporting.Query) (*reporting.Report, error) {
	formattedFilters := formatFilters(in.Filters)
	//todo - deep filtering - should we open this up to more than just one?  only for ReadReport?
	if len(formattedFilters["profile_id"]) > 1 {
		return nil, status.Error(codes.InvalidArgument, "Only one 'profile_id' filter is allowed")
	}
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	// Using ComplianceTwenty as the report might not be in the latest index
	report, err := srv.es.GetReport(relaxting.ComplianceDailyRepTwenty, in.Id, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	return report, nil
}

// ListSuggestions returns a list of suggestions based on query
func (srv *Server) ListSuggestions(ctx context.Context, in *reporting.SuggestionRequest) (*reporting.Suggestions, error) {
	var suggestions reporting.Suggestions
	if in.Size == 0 {
		in.Size = 100
	}
	if in.Type == "" {
		return nil, status.Error(codes.InvalidArgument, fmt.Sprintf("Parameter 'type' not specified"))
	}
	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	suggestionsList, err := srv.es.GetSuggestions(ctx, in.Type, formattedFilters, in.Text, in.Size)
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

// ListControls returns a list of contolListItems based on query
func (srv *Server) ListControlItems(ctx context.Context, in *reporting.ControlItemRequest) (*reporting.ControlItems, error) {
	var controlListItems *reporting.ControlItems
	if in.Size == 0 {
		in.Size = 100
	}

	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(ctx, formattedFilters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	controlListItems, err = srv.es.GetControlListItems(ctx, formattedFilters, in.Size)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	return controlListItems, nil
}

type exportHandler func(*reporting.Report) error

// Export streams a json or csv export
func (srv *Server) Export(in *reporting.Query, stream reporting.ReportingService_ExportServer) error {
	formattedFilters := formatFilters(in.Filters)
	formattedFilters, err := filterByProjects(stream.Context(), formattedFilters)
	if err != nil {
		return err
	}

	if len(formattedFilters["profile_name"]) > 0 && len(formattedFilters["profile_id"]) > 0 {
		return status.Error(codes.InvalidArgument, "Invalid: Cannot specify both 'profile_name' and 'profile_id' filters")
	}

	if len(formattedFilters["profile_name"]) > 1 {
		return status.Error(codes.InvalidArgument, "Invalid: Only one 'profile_name' filter is allowed")
	}

	if len(formattedFilters["profile_id"]) > 1 {
		return status.Error(codes.InvalidArgument, "Invalid: Only one 'profile_id' filter is allowed")
	}

	if len(formattedFilters["control"]) > 1 {
		return status.Error(codes.InvalidArgument, "Invalid: Only one 'control' filter is allowed")
	}

	exporter, err := getExportHandler(in.Type, stream)
	if err != nil {
		return err
	}

	// Step 1: Retrieving the latest report ID for each node based on the provided filters
	esIndex, err := relaxting.GetEsIndex(formattedFilters, false, true)
	if err != nil {
		return status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	reportIDs, err := srv.es.GetReportIds(esIndex, formattedFilters)
	if err != nil {
		return status.Error(codes.Internal, fmt.Sprintf("Failed to determine how many reports exist: %s", err))
	}

	total := len(reportIDs)
	// Step 2: get all reports one by one in reverse order to be sorted asc by end_time
	for idx := total - 1; idx >= 0; idx-- {
		cur, err := srv.es.GetReport(esIndex, reportIDs[idx], formattedFilters)
		if err != nil {
			return status.Error(codes.NotFound, fmt.Sprintf("Failed to retrieve report %d/%d with ID %s . Error: %s", idx, total, reportIDs[idx], err))
		}
		err = exporter(cur)
		if err != nil {
			return status.Error(codes.Internal, fmt.Sprintf("Failed to stream report %d/%d with ID %s . Error: %s", idx, total, reportIDs[idx], err))
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
			return fmt.Errorf("Failed to marshal JSON export data: %+v", err)
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
			return fmt.Errorf("Failed to export JSON: %+v", err)
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
			return fmt.Errorf("Failed to export CSV: %+v", err)
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
	nodes.Nodes = nodesList
	return &nodes, nil
}

// ReadNode returns a node based on id
func (srv *Server) ReadNode(ctx context.Context, in *reporting.Id) (*reporting.Node, error) {
	formattedFilters := formatFilters([]*reporting.ListFilter{})
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
