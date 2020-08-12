package grpcserver

import (
	"context"
	"strings"
	"time"

	"github.com/golang/protobuf/proto"
	gpStruct "github.com/golang/protobuf/ptypes/struct"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/backend/postgres"
	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/components/config-mgmt-service/params"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/version"
)

// CfgMgmtServer stores client
type CfgMgmtServer struct {
	client backend.Client
	cs     *config.Service
	pg     *postgres.Postgres
}

// NewCfgMgmtServer creates a new server instance and it automatically
// initializes the ChefRun Pipeline by consuming the provided backend client
func NewCfgMgmtServer(cs *config.Service) *CfgMgmtServer {
	return &CfgMgmtServer{
		client: cs.GetBackend(),
		cs:     cs,
	}
}

func (s *CfgMgmtServer) ConnectPg() error {
	pg, err := postgres.Open(&s.cs.Postgres)
	if err != nil {
		return err
	}
	s.pg = pg
	return nil
}

func (s *CfgMgmtServer) PgConnection() *postgres.Postgres {
	return s.pg
}

func (s *CfgMgmtServer) ClearPg() error {
	return s.pg.Clear()
}

// GetPolicyCookbooks returns a list of cookbook name, policy
// identifier and name of policy based on revision id
func (s *CfgMgmtServer) GetPolicyCookbooks(ctx context.Context,
	request *request.PolicyRevision) (*response.PolicyCookbooks, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	pcProto := response.PolicyCookbooks{}

	if request.GetRevisionId() == "" {
		return &pcProto, status.Errorf(codes.InvalidArgument, "Parameter revision_id not provided")
	}

	policyCookbooks, err := s.pg.GetPolicyCookbooks(request.GetRevisionId())
	if err != nil {
		if sErr, ok := err.(*errors.StandardError); ok && sErr.Type == errors.PolicyCookbooksNotFound {
			return &pcProto, status.Errorf(codes.NotFound, err.Error())
		}

		return &pcProto, status.Errorf(codes.Internal, err.Error())
	}
	clProtoList := make([]*response.CookbookLock, len(policyCookbooks.CookbookLocks))

	for index, pc := range policyCookbooks.CookbookLocks {
		clProtoList[index] = &response.CookbookLock{
			Cookbook:         pc.CookbookName,
			PolicyIdentifier: pc.PolicyID,
		}
	}
	pcProto = response.PolicyCookbooks{
		PolicyName:    policyCookbooks.PolicyName,
		CookbookLocks: clProtoList,
	}

	return &pcProto, nil
}

// GetHealth returns the service Health
func (s *CfgMgmtServer) GetHealth(ctx context.Context,
	empty *request.Health) (*response.Health, error) {
	return &response.Health{
		Status: "ok",
	}, nil
}

// GetVersion returns the service version
func (s *CfgMgmtServer) GetVersion(ctx context.Context,
	empty *request.VersionInfo) (*response.VersionInfo, error) {
	return &response.VersionInfo{
		Version: s.cs.Version,
		Built:   version.BuildTime,
		Name:    s.cs.Name,
		Sha:     version.GitSHA,
	}, nil
}

// GetNodesCounts returns the nodes counts
func (s *CfgMgmtServer) GetNodesCounts(ctx context.Context,
	request *request.NodesCounts) (*response.NodesCounts, error) {
	var nodesCounts *response.NodesCounts

	filters, err := stringutils.FormatFiltersWithKeyConverter(request.Filter,
		params.ConvertParamToNodeStateBackendLowerFilter)
	if err != nil {
		return nodesCounts, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Date Range
	if !params.ValidateDateTimeRange(request.GetStart(), request.GetEnd()) {
		return nodesCounts, status.Errorf(codes.InvalidArgument,
			"Invalid start/end time. (format: YYYY-MM-DD'T'HH:mm:ssZ)")
	}

	filters, err = filterByProjects(ctx, filters)
	if err != nil {
		return nodesCounts, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	state, err := s.client.GetNodesCounts(filters, request.GetStart(), request.GetEnd())
	if err != nil {
		return nodesCounts, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	nodesCounts = &response.NodesCounts{
		Total:   int32(state.Total),
		Success: int32(state.Success),
		Failure: int32(state.Failure),
		Missing: int32(state.Missing),
	}
	return nodesCounts, nil
}

// This function provides the status of runs for each 24-hour duration. For multiple runs in one
// 24-hour duration, the most recent failed run will be returned. If there are no failed runs the
// most recent successful run will be returned. If no runs are found in the 24-hour duration, the
// status will be "missing" and no run will be returned.
func (s *CfgMgmtServer) GetNodeRunsDailyStatusTimeSeries(ctx context.Context,
	request *request.NodeRunsDailyStatusTimeSeries) (*response.NodeRunsDailyStatusTimeSeries, error) {
	if request.GetNodeId() == "" {
		return nil, status.Errorf(codes.InvalidArgument, "Parameter 'node_id' not provided")
	}

	daysAgo := request.GetDaysAgo()
	if daysAgo < 0 {
		return &response.NodeRunsDailyStatusTimeSeries{}, errors.GrpcError(codes.InvalidArgument,
			"daysAgo needs to be greater than zero")
	}

	// default to provide a time series for the past 24 hours
	if daysAgo == 0 {
		daysAgo = 1
	}

	startTime, endTime := calculateStartEndCheckInTimeSeries(daysAgo)

	timeSeries, err := s.client.GetNodeRunsDailyStatusTimeSeries(request.GetNodeId(), startTime,
		endTime.Add(-time.Millisecond))
	if err != nil {
		return &response.NodeRunsDailyStatusTimeSeries{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	runDurationStatusCollection := make([]*response.RunDurationStatus, len(timeSeries))
	for index, duration := range timeSeries {
		runDurationStatusCollection[index] = &response.RunDurationStatus{
			Start:  duration.Start.Format(time.RFC3339),
			End:    duration.End.Format(time.RFC3339),
			Status: duration.Status,
			RunId:  duration.RunID,
		}
	}

	return &response.NodeRunsDailyStatusTimeSeries{
		Durations: runDurationStatusCollection,
	}, nil
}

// GetCheckInCountsTimeSeries - Returns a daily time series of unique node check-ins for the number of day requested
func (s *CfgMgmtServer) GetCheckInCountsTimeSeries(ctx context.Context,
	request *request.CheckInCountsTimeSeries) (*response.CheckInCountsTimeSeries, error) {
	filters, err := stringutils.FormatFiltersWithKeyConverter(request.GetFilter(),
		params.ConvertParamToNodeRunBackend)
	if err != nil {
		return &response.CheckInCountsTimeSeries{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	daysAgo := request.GetDaysAgo()
	if daysAgo < 0 {
		return &response.CheckInCountsTimeSeries{}, errors.GrpcError(codes.InvalidArgument,
			"daysAgo needs to be greater than zero")
	}

	// default to provide a time series for the past 24 hours
	if daysAgo == 0 {
		daysAgo = 1
	}

	startTime, endTime := calculateStartEndCheckInTimeSeries(daysAgo)

	checkinTimeseries, err := s.client.GetCheckinCountsTimeSeries(startTime,
		endTime.Add(-time.Millisecond), filters)
	if err != nil {
		return &response.CheckInCountsTimeSeries{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	deleteTimeseries, err := s.client.GetDeletedCountsTimeSeries(startTime,
		endTime.Add(-time.Millisecond), filters)
	if err != nil {
		return &response.CheckInCountsTimeSeries{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	createTimeseries, err := s.client.GetCreateCountsTimeSeries(startTime,
		endTime.Add(-time.Millisecond), filters)
	if err != nil {
		return &response.CheckInCountsTimeSeries{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	if len(checkinTimeseries) != len(deleteTimeseries) &&
		len(checkinTimeseries) != len(createTimeseries) {
		return &response.CheckInCountsTimeSeries{}, errors.GrpcErrorf(codes.Internal,
			"time series bucket lengths do not match %d, %d, %d", len(checkinTimeseries),
			len(deleteTimeseries), len(createTimeseries))
	}

	checkInCountsCollection := make([]*response.CheckInCounts, len(checkinTimeseries))
	for index, period := range checkinTimeseries {
		total := int32(createTimeseries[index].Count - deleteTimeseries[index].Count)
		checkInCountsCollection[index] = &response.CheckInCounts{
			Start: period.Start.Format(time.RFC3339),
			End:   period.End.Format(time.RFC3339),
			Count: int32(period.Count),
			Total: total,
		}
	}

	return &response.CheckInCountsTimeSeries{
		Counts: checkInCountsCollection,
	}, nil
}

// GetRunsCounts returns the runs counts for a node
func (s *CfgMgmtServer) GetRunsCounts(ctx context.Context,
	request *request.RunsCounts) (*response.RunsCounts, error) {
	var runsCounts *response.RunsCounts
	if request.GetNodeId() == "" {
		return nil, status.Errorf(codes.InvalidArgument, "Parameter 'node_id' not provided")
	}

	filters, err := stringutils.FormatFiltersWithKeyConverter(request.Filter,
		params.ConvertParamToNodeRunBackend)
	if err != nil {
		return runsCounts, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}
	if !params.ValidateDateRange(request.GetStart(), request.GetEnd()) {
		return runsCounts, status.Errorf(codes.InvalidArgument,
			"Invalid start/end time. (format: YYYY-MM-DD)")
	}

	projectFilters, err := filterByProjects(ctx, map[string][]string{})
	if err != nil {
		return runsCounts, status.Errorf(codes.Internal, err.Error())
	}

	nodeExistsChan := s.nodeExistsAsync(request.GetNodeId(), projectFilters)

	state, err := s.client.GetRunsCounts(filters, request.GetNodeId(),
		request.GetStart(), request.GetEnd())
	if err != nil {
		return runsCounts, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	nodeExists := <-nodeExistsChan
	if nodeExists.err != nil {
		return runsCounts, nodeExists.err
	}

	// Either the user does not have permissions or the node does not exist
	if !nodeExists.exists {
		return &response.RunsCounts{
			Total:   0,
			Success: 0,
			Failure: 0,
		}, nil
	}

	runsCounts = &response.RunsCounts{
		Total:   int32(state.Total),
		Success: int32(state.Success),
		Failure: int32(state.Failure),
	}

	return runsCounts, nil
}

// GetNodeRun returns the requested run
func (s *CfgMgmtServer) GetNodeRun(ctx context.Context,
	request *request.NodeRun) (*response.Run, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	if request.GetRunId() == "" {
		return nil, status.Errorf(codes.InvalidArgument, "Parameter 'run_id' not provided")
	}
	endTime, err := ToTime(request.EndTime)
	if err != nil {
		log.WithFields(log.Fields{
			"request": request.String(),
			"func":    nameOfFunc(),
		}).Error("Unable to determine index of last ccr")
		return nil, status.Errorf(codes.Internal, err.Error())
	}

	projectFilters, err := filterByProjects(ctx, map[string][]string{})
	if err != nil {
		return nil, status.Errorf(codes.Internal, err.Error())
	}

	// Start the async request for the run's associated node
	nodesAsync := s.getNodeAsync(ctx, request.NodeId, projectFilters)

	run, err := s.client.GetRun(request.GetRunId(), endTime)
	if err != nil {
		if sErr, ok := err.(*errors.StandardError); ok && sErr.Type == errors.RunNotFound {
			return nil, status.Errorf(codes.NotFound, err.Error())
		}
		return nil, status.Errorf(codes.Internal, err.Error())
	}

	// If the requested NodeID does not match the run's associated node's ID.
	if run.EntityUuid != request.NodeId {
		return nil, status.Errorf(codes.NotFound, "Invalid ID")
	}

	// Wait for the request for the run's associated node
	getNodes := <-nodesAsync
	if getNodes.err != nil {
		return nil, status.Errorf(codes.Internal, getNodes.err.Error())
	}

	// Either the user does not have permissions or the node does not exist
	if len(getNodes.nodes) == 0 {
		return nil, status.Errorf(codes.NotFound, "Invalid ID")
	}

	return toResponseRun(run, getNodes.nodes[0])
}

// GetNodeMetadataCounts - For each type of field provided return distinct values the amount for each.
// For example, if the 'platform' field is requested 'windows' 10, 'redhat' 5, and 'ubuntu' 8
// could be returned. The number next to each represents the number of nodes with that type of platform.
func (s *CfgMgmtServer) GetNodeMetadataCounts(ctx context.Context,
	req *request.NodeMetadataCounts) (*response.NodeMetadataCounts, error) {

	filters, err := stringutils.FormatFiltersWithKeyConverter(req.Filter,
		params.ConvertParamToNodeStateBackendLowerFilter)

	// Date Range
	if !params.ValidateDateTimeRange(req.GetStart(), req.GetEnd()) {
		return &response.NodeMetadataCounts{}, status.Errorf(codes.InvalidArgument,
			"Invalid start/end time. (format: YYYY-MM-DD'T'HH:mm:ssZ)")
	}

	typeCounts, err := s.client.GetNodeMetadataCounts(filters,
		req.Type, req.Start, req.End)
	if err != nil {
		return &response.NodeMetadataCounts{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	types := make([]*response.TypeCount, len(typeCounts))
	for index := range typeCounts {
		values := make([]*response.ValueCount, len(typeCounts[index].Values))
		for valueIndex, valueCount := range typeCounts[index].Values {
			values[valueIndex] = &response.ValueCount{
				Count: int32(valueCount.Count),
				Value: valueCount.Value,
			}
		}

		types[index] = &response.TypeCount{
			Type:   typeCounts[index].Type,
			Values: values,
		}
	}

	return &response.NodeMetadataCounts{
		Types: types,
	}, nil
}

func (s *CfgMgmtServer) GetSuggestions(ctx context.Context,
	request *request.Suggestion) (*gpStruct.ListValue, error) {
	var (
		pSuggestions = new(gpStruct.ListValue)
		textParam    = request.GetText()
		typeParam    = request.GetType()
	)

	if typeParam == "" {
		return nil, errors.GrpcErrorf(codes.InvalidArgument, "Parameter 'type' not specified")
	}
	if !params.ValidSuggestionParam(typeParam) {
		return nil, errors.GrpcErrorf(codes.InvalidArgument, "Invalid type parameter '%v'", typeParam)
	}

	adjustedFilter := removeSuggestionTypeFromFilter(request.Filter, typeParam)
	filters, err := stringutils.FormatFiltersWithKeyConverter(adjustedFilter,
		params.ConvertParamToNodeStateBackendLowerFilter)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, err.Error())
	}

	filters, err = filterByProjects(ctx, filters)
	if err != nil {
		return nil, errors.GrpcErrorf(codes.Internal, err.Error())
	}

	suggestions, err := s.client.GetSuggestions(
		params.ConvertParamToNodeRunBackend(typeParam), textParam, filters)
	if err != nil {
		return nil, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}
	messages := backendSuggestionsToProtoArray(suggestions)
	err = messageArrayToListValue(messages, pSuggestions)
	if err != nil {
		return pSuggestions, errors.GrpcErrorFromErr(codes.Internal, err)
	}
	return pSuggestions, nil
}

// GetOrganizations returns the a list of all organizations
func (s *CfgMgmtServer) GetOrganizations(ctx context.Context,
	empty *request.Organizations) (*gpStruct.ListValue, error) {
	var organizations = new(gpStruct.ListValue)

	filters, err := filterByProjects(ctx, map[string][]string{})
	if err != nil {
		return nil, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	orgs, err := s.client.GetListForField("organization_name", filters)
	if err != nil {
		return nil, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	// Lets not transform the message if there are NO organizations
	if len(orgs) > 0 {
		err = stringArrayToListValue(orgs, organizations)
		if err != nil {
			return organizations, errors.GrpcErrorFromErr(codes.Internal, err)
		}
	}
	return organizations, err
}

// GetSourceFqdns returns a list of all source_fqdns
func (s *CfgMgmtServer) GetSourceFqdns(ctx context.Context,
	empty *request.SourceFQDNS) (*gpStruct.ListValue, error) {
	var sourceFqdns = new(gpStruct.ListValue)

	filters, err := filterByProjects(ctx, map[string][]string{})
	if err != nil {
		return nil, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	fqdns, err := s.client.GetListForField("source_fqdn", filters)
	if err != nil {
		return nil, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	// Lets not transform the message if there are NO source fqdns
	if len(fqdns) > 0 {
		err = stringArrayToListValue(fqdns, sourceFqdns)
		if err != nil {
			return sourceFqdns, errors.GrpcErrorFromErr(codes.Internal, err)
		}
	}
	return sourceFqdns, err
}

func removeSuggestionTypeFromFilter(filter []string, suggestionType string) []string {
	adjustedFilter := make([]string, 0)
	for _, filter := range filter {
		keyValuePair := strings.Split(filter, ":")

		if len(keyValuePair) == 2 && keyValuePair[0] != suggestionType {
			adjustedFilter = append(adjustedFilter, filter)
		}
	}

	return adjustedFilter
}

func backendSuggestionsToProtoArray(suggestions []backend.Suggestion) []proto.Message {
	messages := make([]proto.Message, len(suggestions))
	for i, suggestion := range suggestions {
		messages[i] = &response.Suggestion{
			Text:  suggestion.Text,
			Score: suggestion.Score,
		}
	}
	return messages
}

// stringArrayToListValue Casts a []string into a 'Proto ListValue'
func stringArrayToListValue(strings []string, list *gpStruct.ListValue) error {
	list.Values = make([]*gpStruct.Value, len(strings))

	for i, string := range strings {
		v := &gpStruct.Value{
			Kind: &gpStruct.Value_StringValue{StringValue: string},
		}

		list.Values[i] = v
	}
	return nil
}

// getNodeAsync - Getting a single node async to allow two requests to elasticsearch at the same time.
func (s *CfgMgmtServer) getNodeAsync(ctx context.Context, nodeID string, projectFilters map[string][]string) chan struct {
	nodes []backend.Node
	err   error
} {
	nodesChan := make(chan struct {
		nodes []backend.Node
		err   error
	})
	go func() {
		projectFilters["entity_uuid"] = []string{nodeID}
		nodes, err := s.client.GetNodesPageByCursor(ctx, time.Time{}, time.Time{},
			projectFilters, nil, "", 1, "", true)
		nodesChan <- struct {
			nodes []backend.Node
			err   error
		}{nodes, err}
		close(nodesChan)
	}()

	return nodesChan
}

// Calculate the start and end times for the number of days back from now
// end time will be the beginning of the next hour.
// start time will be 24 hours multiples of daysAgo from the end time.
func calculateStartEndCheckInTimeSeries(daysAgo int32) (time.Time, time.Time) {
	now := time.Now()
	endTime := time.Date(now.Year(), now.Month(), now.Day(),
		now.Hour(), 0, 0, 0, time.UTC).Add(time.Hour)

	startTime := endTime.Add(-time.Hour * 24 * time.Duration(daysAgo))

	return startTime, endTime
}
