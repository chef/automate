package handler

import (
	"context"

	gpStruct "github.com/golang/protobuf/ptypes/struct"
	log "github.com/sirupsen/logrus"

	// Cfgmgmt Request/Response definitions
	cfgReq "github.com/chef/automate/api/external/cfgmgmt/request"
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
	cfgMgmtClient cmsService.CfgMgmtClient
}

// NewCfgMgmtServer initializes CfgMgmtServer with client
func NewCfgMgmtServer(cfgMgmtClient cmsService.CfgMgmtClient) *CfgMgmtServer {
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
		Sha:     r.GetSHA(),
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
		Text: textParam,
		Type: typeParam,
	}

	return s.cfgMgmtClient.GetSuggestions(ctx, &sugRequest)
}

func toResponseRun(run *cmsRes.Run) *cfgRes.Run {
	return &cfgRes.Run{
		Id:                   run.Id,
		NodeId:               run.NodeId,
		NodeName:             run.NodeName,
		Organization:         run.Organization,
		StartTime:            run.StartTime,
		EndTime:              run.EndTime,
		Source:               run.Source,
		Status:               run.Status,
		TotalResourceCount:   run.TotalResourceCount,
		UpdatedResourceCount: run.UpdatedResourceCount,
		ChefVersion:          run.ChefVersion,
		UptimeSeconds:        run.UptimeSeconds,
		Environment:          run.Environment,
		Fqdn:                 run.Fqdn,
		SourceFqdn:           run.SourceFqdn,
		Ipaddress:            run.Ipaddress,
		RunList:              run.RunList,
		Tags:                 run.Tags,
		ResourceNames:        run.ResourceNames,
		Recipes:              run.Recipes,
		ChefTags:             run.ChefTags,
		Cookbooks:            run.Cookbooks,
		Platform:             run.Platform,
		PlatformFamily:       run.PlatformFamily,
		PlatformVersion:      run.PlatformVersion,
		Roles:                run.Roles,
		PolicyName:           run.PolicyName,
		PolicyGroup:          run.PolicyGroup,
		PolicyRevision:       run.PolicyRevision,
		ExpandedRunList:      toResponseExpandedRunList(run.ExpandedRunList),
		Resources:            toResponseResources(run.Resources),
		Deprecations:         toResponseDeprecations(run.Deprecations),
		Error:                toResponseError(run.Error),
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
		}
	}

	return responseResources
}

//GetOrganizations returns the names of every organization for the configuration management service
func (s *CfgMgmtServer) GetOrganizations(ctx context.Context, e *cfgReq.Organizations) (*gpStruct.ListValue, error) {
	return s.cfgMgmtClient.GetOrganizations(ctx, &cmsReq.Organizations{})
}

//GetSourceFqdns returns the names of every organization for the configuration management service
func (s *CfgMgmtServer) GetSourceFqdns(ctx context.Context, e *cfgReq.SourceFqdns) (*gpStruct.ListValue, error) {
	return s.cfgMgmtClient.GetSourceFqdns(ctx, &cmsReq.SourceFQDNS{})
}
