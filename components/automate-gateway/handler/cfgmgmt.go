package handler

import (
	"context"

	gpStruct "github.com/golang/protobuf/ptypes/struct"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	// Cfgmgmt Request/Response definitions
	cfgService "github.com/chef/automate/api/external/cfgmgmt"
	"github.com/chef/automate/api/external/cfgmgmt/request"
	cfgReq "github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/chef/automate/api/external/cfgmgmt/response"
	cfgRes "github.com/chef/automate/api/external/cfgmgmt/response"

	// Shared Response/Request definitions
	sharedReq "github.com/chef/automate/api/external/common/query"
	version "github.com/chef/automate/api/external/common/version"

	// config-mgmt-service Requests/Response/Service definitions
	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
)

// CfgMgmtServer stores client
type CfgMgmtServer struct {
	cfgMgmtClient cmsService.CfgMgmtServiceClient
}

// NewCfgMgmtServer initializes CfgMgmtServer with client
func NewCfgMgmtServer(cfgMgmtClient cmsService.CfgMgmtServiceClient) *CfgMgmtServer {
	return &CfgMgmtServer{
		cfgMgmtClient: cfgMgmtClient,
	}
}

// GetPolicyCookbooks returns PolicyCookbooks with their policy identifiers based on a policy revision ID.
func (s *CfgMgmtServer) GetPolicyCookbooks(ctx context.Context, request *cfgReq.PolicyRevision) (*cfgRes.PolicyCookbooks, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	policyRevision := cmsReq.PolicyRevision{
		RevisionId: request.GetRevisionId(),
	}

	r, err := s.cfgMgmtClient.GetPolicyCookbooks(ctx, &policyRevision)
	if err != nil {
		return &cfgRes.PolicyCookbooks{}, err
	}

	cookbookLocks := make([]*cfgRes.CookbookLock, len(r.CookbookLocks))
	for index, policyCookbook := range r.CookbookLocks {
		cookbookLocks[index] = &cfgRes.CookbookLock{
			Cookbook:         policyCookbook.Cookbook,
			PolicyIdentifier: policyCookbook.PolicyIdentifier,
		}
	}
	return &cfgRes.PolicyCookbooks{
		PolicyName:    r.PolicyName,
		CookbookLocks: cookbookLocks,
	}, nil
}

// GetRuns returns an array of Runs returned from the Cfgmgmt (a.k.a. config-mgmt) Service
func (s *CfgMgmtServer) GetRuns(ctx context.Context, request *cfgReq.Runs) (*gpStruct.ListValue, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	// Transforming the 'Gateway Runs Message ' to 'config-mgmt Runs Message'
	//                       'cfgReq.Runs'      =>     'cmsReq.Runs'
	//
	// We do this because we are not re-using the messages from the micro-services
	// (@afiune) Maybe it is not a bad idea to re-use ONLY the messages?
	runsRequest := cmsReq.Runs{
		Filter: request.GetFilter(),
	}

	if request.GetPagination() != nil {
		runsRequest.Pagination = &cmsReq.Pagination{
			Page: request.GetPagination().GetPage(),
			Size: request.GetPagination().GetSize(),
		}
	}

	// Should the gateway verify the content of the request as well?
	runsRequest.Start = request.GetStart()
	runsRequest.End = request.GetEnd()
	runsRequest.NodeId = request.GetNodeId()

	return s.cfgMgmtClient.GetRuns(ctx, &runsRequest)
}

// GetNodes returns an array of Nodes returned from the Cfgmgmt (a.k.a. config-mgmt) Service
func (s *CfgMgmtServer) GetNodes(ctx context.Context, request *cfgReq.Nodes) (*gpStruct.ListValue, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	// Transforming the 'Gateway Nodes Message ' to 'config-mgmt Nodes Message'
	//                       'cfgReq.Nodes'      =>     'cmsReq.Nodes'
	//
	// We do this because we are not re-using the messages from the micro-services
	// (@afiune) Maybe it is not a bad idea to re-use ONLY the messages?
	nodesRequest := cmsReq.Nodes{
		Filter: request.GetFilter(),
		Start:  request.Start,
		End:    request.End,
	}

	if request.GetPagination() != nil {
		nodesRequest.Pagination = &cmsReq.Pagination{
			Page: request.GetPagination().GetPage(),
			Size: request.GetPagination().GetSize(),
		}
	}

	if request.GetSorting() != nil {
		nodesRequest.Sorting = &cmsReq.Sorting{
			Field: request.GetSorting().GetField(),
			Order: cmsReq.Order(request.GetSorting().GetOrder()),
		}
	}

	return s.cfgMgmtClient.GetNodes(ctx, &nodesRequest)
}

func (s *CfgMgmtServer) GetErrors(ctx context.Context, request *cfgReq.Errors) (*cfgRes.Errors, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.cfgMgmtClient.GetErrors(ctx, request)
}

func (s *CfgMgmtServer) GetMissingNodeDurationCounts(ctx context.Context,
	request *cfgReq.MissingNodeDurationCounts) (*cfgRes.MissingNodeDurationCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.MissingNodeDurationCounts{
		Durations: request.Durations,
	}

	response, err := s.cfgMgmtClient.GetMissingNodeDurationCounts(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.MissingNodeDurationCounts{}, err
	}

	responseCountedDurations := make([]*cfgRes.CountedDuration, len(response.CountedDurations))
	for index := range response.CountedDurations {
		responseCountedDurations[index] = &cfgRes.CountedDuration{
			Duration: response.CountedDurations[index].Duration,
			Count:    response.CountedDurations[index].Count,
		}
	}

	return &cfgRes.MissingNodeDurationCounts{
		CountedDurations: responseCountedDurations,
	}, nil
}

func (s *CfgMgmtServer) GetAttributes(ctx context.Context, request *cfgReq.Node) (*cfgRes.NodeAttribute, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.Node{
		NodeId: request.NodeId,
	}

	attribute, err := s.cfgMgmtClient.GetAttributes(ctx, cfgMgmtRequest)

	if err != nil {
		return &cfgRes.NodeAttribute{}, err
	}
	return &cfgRes.NodeAttribute{
		NodeId:              attribute.NodeId,
		Name:                attribute.Name,
		ChefEnvironment:     attribute.ChefEnvironment,
		RunList:             attribute.RunList,
		Normal:              attribute.Normal,
		NormalValueCount:    attribute.NormalValueCount,
		Default:             attribute.Default,
		DefaultValueCount:   attribute.DefaultValueCount,
		Override:            attribute.Override,
		OverrideValueCount:  attribute.OverrideValueCount,
		Automatic:           attribute.Automatic,
		AutomaticValueCount: attribute.AutomaticValueCount,
		AllValueCount:       attribute.AllValueCount,
	}, nil
}

// GetVersion returns the version info for the configuration management service
func (s *CfgMgmtServer) GetVersion(ctx context.Context, e *version.VersionInfoRequest) (*version.VersionInfo, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	r, err := s.cfgMgmtClient.GetVersion(ctx, &cmsReq.VersionInfo{})
	if err != nil {
		return &version.VersionInfo{}, err
	}

	return &version.VersionInfo{
		Version: r.GetVersion(),
		Built:   r.GetBuilt(),
		Name:    r.GetName(),
		Sha:     r.GetSha(),
	}, nil
}

// GetNodesCounts returns the nodes counts
func (s *CfgMgmtServer) GetNodesCounts(ctx context.Context, request *cfgReq.NodesCounts) (*cfgRes.NodesCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.NodesCounts{
		Filter: request.Filter,
		Start:  request.Start,
		End:    request.End,
	}

	cfgmgmtNodesCounts, err := s.cfgMgmtClient.GetNodesCounts(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.NodesCounts{}, err
	}

	return &cfgRes.NodesCounts{
		Total:   cfgmgmtNodesCounts.Total,
		Success: cfgmgmtNodesCounts.Success,
		Failure: cfgmgmtNodesCounts.Failure,
		Missing: cfgmgmtNodesCounts.Missing,
	}, nil
}

// GetRunsCounts returns the runs counts
func (s *CfgMgmtServer) GetRunsCounts(ctx context.Context, request *cfgReq.RunsCounts) (*cfgRes.RunsCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.RunsCounts{
		Filter: request.Filter,
		Start:  request.Start,
		End:    request.End,
		NodeId: request.NodeId,
	}

	cfgmgmtRunsCounts, err := s.cfgMgmtClient.GetRunsCounts(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.RunsCounts{}, err
	}

	return &cfgRes.RunsCounts{
		Total:   cfgmgmtRunsCounts.Total,
		Success: cfgmgmtRunsCounts.Success,
		Failure: cfgmgmtRunsCounts.Failure,
	}, nil
}

// GetCheckInCountsTimeSeries - Returns a daily time series of unique node check-ins for the number of day requested
func (s *CfgMgmtServer) GetCheckInCountsTimeSeries(ctx context.Context,
	request *cfgReq.CheckInCountsTimeSeries) (*cfgRes.CheckInCountsTimeSeries, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.CheckInCountsTimeSeries{
		Filter:  request.Filter,
		DaysAgo: request.DaysAgo,
	}

	cfgmgmtResponse, err := s.cfgMgmtClient.GetCheckInCountsTimeSeries(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.CheckInCountsTimeSeries{}, err
	}
	counts := make([]*cfgRes.CheckInCounts, len(cfgmgmtResponse.Counts))
	for index, cfgCount := range cfgmgmtResponse.Counts {
		counts[index] = &cfgRes.CheckInCounts{
			Start: cfgCount.Start,
			End:   cfgCount.End,
			Count: cfgCount.Count,
			Total: cfgCount.Total,
		}
	}

	return &cfgRes.CheckInCountsTimeSeries{
		Counts: counts,
	}, nil
}

// GetNodeRunsDailyStatusTimeSeries -   Provides the status of runs for each 24-hour duration.
// For multiple runs in one 24-hour duration, the most recent failed run will be returned.
// If there are no failed runs the most recent successful run will be returned. If no runs are
// found in the 24-hour duration, the status will be "missing" and no run information will be returned.
func (s *CfgMgmtServer) GetNodeRunsDailyStatusTimeSeries(ctx context.Context,
	request *cfgReq.NodeRunsDailyStatusTimeSeries) (*cfgRes.NodeRunsDailyStatusTimeSeries, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.NodeRunsDailyStatusTimeSeries{
		NodeId:  request.NodeId,
		DaysAgo: request.DaysAgo,
	}

	cfgmgmtResponse, err := s.cfgMgmtClient.GetNodeRunsDailyStatusTimeSeries(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.NodeRunsDailyStatusTimeSeries{}, err
	}
	durations := make([]*cfgRes.RunDurationStatus, len(cfgmgmtResponse.Durations))
	for index, cfgDuration := range cfgmgmtResponse.Durations {
		durations[index] = &cfgRes.RunDurationStatus{
			Start:  cfgDuration.Start,
			End:    cfgDuration.End,
			Status: cfgDuration.Status,
			RunId:  cfgDuration.RunId,
		}
	}

	return &cfgRes.NodeRunsDailyStatusTimeSeries{
		Durations: durations,
	}, nil
}

// GetNodeMetadataCounts - For each type of field provided return distinct values the amount for each.
// For example, if the 'platform' field is requested 'windows' 10, 'redhat' 5, and 'ubuntu' 8
// could be returned. The number next to each represents the number of nodes with that type of platform.
func (s *CfgMgmtServer) GetNodeMetadataCounts(ctx context.Context,
	request *cfgReq.NodeMetadataCounts) (*cfgRes.NodeMetadataCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.NodeMetadataCounts{
		Type:   request.Type,
		Filter: request.Filter,
		Start:  request.Start,
		End:    request.End,
	}

	cfgmgmtResponse, err := s.cfgMgmtClient.GetNodeMetadataCounts(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.NodeMetadataCounts{}, err
	}
	types := make([]*cfgRes.TypeCount, len(cfgmgmtResponse.Types))
	for index, cfgField := range cfgmgmtResponse.Types {
		values := make([]*cfgRes.ValueCount, len(cfgField.Values))
		for valueIndex, cfgValueCount := range cfgField.Values {
			values[valueIndex] = &cfgRes.ValueCount{
				Value: cfgValueCount.Value,
				Count: cfgValueCount.Count,
			}
		}
		types[index] = &cfgRes.TypeCount{
			Values: values,
			Type:   cfgField.Type,
		}
	}

	return &cfgRes.NodeMetadataCounts{
		Types: types,
	}, nil
}

// GetNodeRun returns the requested run
func (s *CfgMgmtServer) GetNodeRun(ctx context.Context, request *cfgReq.NodeRun) (*cfgRes.Run, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.NodeRun{
		NodeId:  request.NodeId,
		RunId:   request.RunId,
		EndTime: request.EndTime,
	}

	cfgmgmtRun, err := s.cfgMgmtClient.GetNodeRun(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.Run{}, err
	}

	return toResponseRun(cfgmgmtRun), nil
}

//Get suggestions returns an array of suggestions from config management
func (s *CfgMgmtServer) GetSuggestions(ctx context.Context, request *sharedReq.Suggestion) (*gpStruct.ListValue, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	textParam := request.GetText()
	typeParam := request.GetType()

	sugRequest := cmsReq.Suggestion{
		Text:   textParam,
		Type:   typeParam,
		Filter: request.Filter,
	}

	return s.cfgMgmtClient.GetSuggestions(ctx, &sugRequest)
}

func (a *CfgMgmtServer) CreateRollout(ctx context.Context, req *cfgReq.CreateRollout) (*cfgRes.Rollout, error) {
	log.WithFields(log.Fields{
		"request": req.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.cfgMgmtClient.CreateRollout(ctx, req)
}
func (a *CfgMgmtServer) GetRollouts(ctx context.Context, req *cfgReq.Rollouts) (*cfgRes.Rollouts, error) {
	log.WithFields(log.Fields{
		"request": req.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.cfgMgmtClient.GetRollouts(ctx, req)
}
func (a *CfgMgmtServer) GetRolloutById(ctx context.Context, req *cfgReq.RolloutById) (*cfgRes.Rollout, error) {
	log.WithFields(log.Fields{
		"request": req.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.cfgMgmtClient.GetRolloutById(ctx, req)
}
func (a *CfgMgmtServer) GetRolloutForChefRun(context.Context, *cfgReq.RolloutForChefRun) (*cfgRes.Rollout, error) {
	// TODO
	return nil, nil
}

func (a *CfgMgmtServer) ListNodeSegmentsWithRolloutProgress(ctx context.Context, req *cfgReq.ListNodeSegmentsWithRolloutProgress) (*cfgRes.NodeSegmentsWithRolloutProgress, error) {
	log.WithFields(log.Fields{
		"request": req.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.cfgMgmtClient.ListNodeSegmentsWithRolloutProgress(ctx, req)
}

func (a *CfgMgmtServer) CreateRolloutTest(ctx context.Context, req *cfgReq.CreateRolloutTest) (*cfgRes.CreateRolloutTest, error) {
	log.WithFields(log.Fields{
		"request": req.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.cfgMgmtClient.CreateRolloutTest(ctx, req)
}

func (a *CfgMgmtServer) NodeExport(*cfgReq.NodeExport, cfgService.ConfigMgmt_NodeExportServer) error {
	// Please see components/automate-gateway/services.go configMgmtNodeExportHandler for implementation
	return nil
}

func (a *CfgMgmtServer) ReportExport(*cfgReq.ReportExport, cfgService.ConfigMgmt_ReportExportServer) error {
	// Please see components/automate-gateway/services.go configMgmtReportExportHandler for implementation
	return nil
}

func toResponseRun(run *cmsRes.Run) *cfgRes.Run {
	return &cfgRes.Run{
		Id:                    run.Id,
		NodeId:                run.NodeId,
		NodeName:              run.NodeName,
		Organization:          run.Organization,
		StartTime:             run.StartTime,
		EndTime:               run.EndTime,
		Source:                run.Source,
		Status:                run.Status,
		TotalResourceCount:    run.TotalResourceCount,
		UpdatedResourceCount:  run.UpdatedResourceCount,
		ChefVersion:           run.ChefVersion,
		UptimeSeconds:         run.UptimeSeconds,
		Environment:           run.Environment,
		Fqdn:                  run.Fqdn,
		SourceFqdn:            run.SourceFqdn,
		Ipaddress:             run.Ipaddress,
		RunList:               run.RunList,
		Tags:                  run.Tags,
		ResourceNames:         run.ResourceNames,
		Recipes:               run.Recipes,
		ChefTags:              run.ChefTags,
		Cookbooks:             run.Cookbooks,
		Platform:              run.Platform,
		PlatformFamily:        run.PlatformFamily,
		PlatformVersion:       run.PlatformVersion,
		Roles:                 run.Roles,
		PolicyName:            run.PolicyName,
		PolicyGroup:           run.PolicyGroup,
		PolicyRevision:        run.PolicyRevision,
		ExpandedRunList:       toResponseExpandedRunList(run.ExpandedRunList),
		Resources:             toResponseResources(run.Resources),
		Deprecations:          toResponseDeprecations(run.Deprecations),
		Error:                 toResponseError(run.Error),
		Projects:              run.Projects,
		VersionedCookbooks:    toVersionsCookbooks(run.VersionedCookbooks),
		Timezone:              run.Timezone,
		CloudProvider:         run.CloudProvider,
		KernelRelease:         run.KernelRelease,
		KernelVersion:         run.KernelVersion,
		VirtualizationSystem:  run.VirtualizationSystem,
		VirtualizationRole:    run.VirtualizationRole,
		DmiSystemManufacturer: run.DmiSystemManufacturer,
		DmiSystemSerialNumber: run.DmiSystemSerialNumber,
		Domain:                run.Domain,
		Hostname:              run.Hostname,
		Macaddress:            run.Macaddress,
		MemoryTotal:           run.MemoryTotal,
		Ip6Address:            run.Ip6Address,
	}
}

func toResponseError(chefError *cmsRes.ChefError) *cfgRes.ChefError {
	gwChefError := &cfgRes.ChefError{
		Class:     chefError.Class,
		Message:   chefError.Message,
		Backtrace: chefError.Backtrace,
	}

	if (chefError.Description != nil && chefError.Description != &cmsRes.Description{}) {
		gwChefError.Description = &cfgRes.Description{
			Title:    chefError.Description.Title,
			Sections: chefError.Description.Sections,
		}
	}
	return gwChefError
}

func toResponseDeprecations(deprecations []*cmsRes.Deprecation) []*cfgRes.Deprecation {
	responseDeprecations := make([]*cfgRes.Deprecation, len(deprecations))

	for index, deprecation := range deprecations {
		responseDeprecations[index] = &cfgRes.Deprecation{
			Message:  deprecation.Message,
			Url:      deprecation.Url,
			Location: deprecation.Location,
		}
	}

	return responseDeprecations
}

func toResponseExpandedRunList(expandedRunList *cmsRes.ExpandedRunList) *cfgRes.ExpandedRunList {
	var responseExpandedRunList cfgRes.ExpandedRunList

	responseExpandedRunList.Id = expandedRunList.Id
	responseExpandedRunList.RunList = make([]*cfgRes.RunList, len(expandedRunList.RunList))
	for index, runList := range expandedRunList.RunList {
		responseExpandedRunList.RunList[index] = &cfgRes.RunList{
			Type:    runList.Type,
			Name:    runList.Name,
			Version: runList.Version,
			Skipped: runList.Skipped,
		}
		responseExpandedRunList.RunList[index] = toResponseRunList(runList)
	}

	return &responseExpandedRunList
}

func toResponseRunList(runListItem *cmsRes.RunList) *cfgRes.RunList {
	children := make([]*cfgRes.RunList, len(runListItem.Children))
	for index, runListChild := range runListItem.Children {
		children[index] = toResponseRunList(runListChild)
	}

	return &cfgRes.RunList{
		Type:     runListItem.Type,
		Name:     runListItem.Name,
		Version:  runListItem.Version,
		Skipped:  runListItem.Skipped,
		Children: children,
	}
}

func toResponseResources(resources []*cmsRes.Resource) []*cfgRes.Resource {
	responseResources := make([]*cfgRes.Resource, len(resources))

	for index, resource := range resources {
		responseResources[index] = &cfgRes.Resource{
			Type:            resource.Type,
			Name:            resource.Name,
			Id:              resource.Id,
			Duration:        resource.Duration,
			Delta:           resource.Delta,
			CookbookName:    resource.CookbookName,
			CookbookVersion: resource.CookbookVersion,
			Status:          resource.Status,
			RecipeName:      resource.RecipeName,
			Result:          resource.Result,
			Conditional:     resource.Conditional,   // might be empty
			IgnoreFailure:   resource.IgnoreFailure, // might be empty
			Error:           toResponseError(resource.Error),
		}
	}

	return responseResources
}

func toVersionsCookbooks(versionedCookbooks []*cmsRes.VersionedCookbook) []*cfgRes.VersionedCookbook {
	responseExpandedCookbooks := make([]*cfgRes.VersionedCookbook, len(versionedCookbooks))

	for index, cookbook := range versionedCookbooks {
		responseExpandedCookbooks[index] = &cfgRes.VersionedCookbook{
			Name:    cookbook.Name,
			Version: cookbook.Version,
		}
	}

	return responseExpandedCookbooks
}

//GetOrganizations returns the names of every organization for the configuration management service
func (s *CfgMgmtServer) GetOrganizations(ctx context.Context, e *cfgReq.Organizations) (*gpStruct.ListValue, error) {
	return s.cfgMgmtClient.GetOrganizations(ctx, &cmsReq.Organizations{})
}

//GetSourceFqdns returns the names of every organization for the configuration management service
func (s *CfgMgmtServer) GetSourceFqdns(ctx context.Context, e *cfgReq.SourceFqdns) (*gpStruct.ListValue, error) {
	return s.cfgMgmtClient.GetSourceFqdns(ctx, &cmsReq.SourceFQDNS{})
}

//UpdateTelemetryReported Updates the last client run telemetry reported date in postgres
func (s *CfgMgmtServer) UpdateTelemetryReported(ctx context.Context, req *cfgReq.UpdateTelemetryReportedRequest) (*cfgRes.UpdateTelemetryReportedResponse, error) {
	log.WithFields(log.Fields{
		"request": req.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	if req.LastTelemetryReportedAt == "" {
		return &cfgRes.UpdateTelemetryReportedResponse{}, errors.New("LastTelemetryReported timestamp is required")
	}

	cfgMgmtRequest := &cmsReq.UpdateTelemetryReportedRequest{
		LastTelemetryReportedAt: req.LastTelemetryReportedAt,
	}

	_, err := s.cfgMgmtClient.UpdateTelemetryReported(ctx, cfgMgmtRequest)
	if err != nil {
		return &cfgRes.UpdateTelemetryReportedResponse{}, err
	}

	return &cfgRes.UpdateTelemetryReportedResponse{}, nil
}

//GetNodesUsageCount returns the count of unique nodes with lastRun in a given time.
func (s *CfgMgmtServer) GetNodesUsageCount(ctx context.Context, req *request.GetNodesUsageCountRequest) (*response.GetNodesUsageCountResponse, error) {
	log.WithFields(log.Fields{
		"request": req.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	cfgMgmtRequest := &cmsReq.GetNodesUsageCountRequest{}

	cfgmgmtResponse, err := s.cfgMgmtClient.GetNodesUsageCount(ctx, cfgMgmtRequest)
	if err != nil {
		return nil, err
	}

	return &cfgRes.GetNodesUsageCountResponse{
		DaysSinceLastPost: cfgmgmtResponse.GetDaysSinceLastPost(),
		NodeCnt:           cfgmgmtResponse.GetNodeCnt(),
	}, nil
}
