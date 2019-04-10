package grpcserver

import (
	"context"

	"github.com/golang/protobuf/proto"
	gpStruct "github.com/golang/protobuf/ptypes/struct"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/components/config-mgmt-service/params"
	"github.com/chef/automate/lib/version"
)

// CfgMgmtServer stores client
type CfgMgmtServer struct {
	client backend.Client
	cs     *config.Service
}

// NewCfgMgmtServer creates a new server instance and it automatically
// initializes the ChefRun Pipeline by consuming the provided backend client
func NewCfgMgmtServer(cs *config.Service) *CfgMgmtServer {
	return &CfgMgmtServer{
		client: cs.GetBackend(),
		cs:     cs,
	}
}

// GetPolicyCookbooks returns a list of cookbook name, policy identifier and name of policy based on revision id
func (s *CfgMgmtServer) GetPolicyCookbooks(ctx context.Context, request *request.PolicyRevision) (*response.PolicyCookbooks, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	pcProto := response.PolicyCookbooks{}

	if request.GetRevisionId() == "" {
		return &pcProto, status.Errorf(codes.InvalidArgument, "Parameter revision_id not provided")
	}

	policyCookbooks, err := s.client.GetPolicyCookbooks(request.GetRevisionId())
	if err != nil {
		if sErr, ok := err.(*errors.StandardError); ok && sErr.Type == errors.ActionNotFound {
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
func (s *CfgMgmtServer) GetHealth(ctx context.Context, empty *request.Health) (*response.Health, error) {
	return &response.Health{
		Status: "ok",
	}, nil
}

// GetVersion returns the service version
func (s *CfgMgmtServer) GetVersion(ctx context.Context, empty *request.VersionInfo) (*response.VersionInfo, error) {
	return &response.VersionInfo{
		Version: s.cs.Version,
		Built:   version.BuildTime,
		Name:    s.cs.Name,
		SHA:     version.GitSHA,
	}, nil
}

// GetNodesCounts returns the nodes counts
func (s *CfgMgmtServer) GetNodesCounts(ctx context.Context, request *request.NodesCounts) (*response.NodesCounts, error) {
	var nodesCounts *response.NodesCounts

	filters, err := params.FormatNodeFilters(request.Filter)
	if err != nil {
		return nodesCounts, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	state, err := s.client.GetNodesCounts(filters)
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

// GetRunsCounts returns the runs counts
func (s *CfgMgmtServer) GetRunsCounts(ctx context.Context, request *request.RunsCounts) (*response.RunsCounts, error) {
	var runsCounts *response.RunsCounts
	filters, err := params.FormatNodeFilters(request.Filter)
	if err != nil {
		return runsCounts, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}
	if !params.ValidateDateRange(request.GetStart(), request.GetEnd()) {
		return runsCounts, status.Errorf(codes.InvalidArgument, "Invalid start/end time. (format: YYYY-MM-DD)")
	}

	state, err := s.client.GetRunsCounts(filters, request.GetStart(), request.GetEnd())
	if err != nil {
		return runsCounts, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	runsCounts = &response.RunsCounts{
		Total:   int32(state.Total),
		Success: int32(state.Success),
		Failure: int32(state.Failure),
	}

	return runsCounts, nil
}

// GetNodeRun returns the requested run
func (s *CfgMgmtServer) GetNodeRun(ctx context.Context, request *request.NodeRun) (*response.Run, error) {
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

	run, err := s.client.GetRun(request.GetRunId(), endTime)

	if err != nil {
		if sErr, ok := err.(*errors.StandardError); ok && sErr.Type == errors.RunNotFound {
			return nil, status.Errorf(codes.NotFound, err.Error())
		} else {
			return nil, status.Errorf(codes.Internal, err.Error())
		}
	}

	return toResponseRun(run)
}

func (s *CfgMgmtServer) GetSuggestions(ctx context.Context, request *request.Suggestion) (*gpStruct.ListValue, error) {
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

	suggestions, err := s.client.GetSuggestions(params.ConvertParamToNodeRunBackend(typeParam), textParam)
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

// GetOrganizations returns the a list of all organizations
func (s *CfgMgmtServer) GetOrganizations(ctx context.Context, empty *request.Organizations) (*gpStruct.ListValue, error) {
	var organizations = new(gpStruct.ListValue)

	orgs, err := s.client.GetListForField("organization_name")
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
func (s *CfgMgmtServer) GetSourceFqdns(ctx context.Context, empty *request.SourceFQDNS) (*gpStruct.ListValue, error) {
	var sourceFqdns = new(gpStruct.ListValue)

	fqdns, err := s.client.GetListForField("source_fqdn")
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

// stringArrayToListValue Casts a []string into a 'Proto ListValue'
func stringArrayToListValue(strings []string, list *gpStruct.ListValue) error {
	list.Values = make([]*gpStruct.Value, len(strings))

	for i, string := range strings {
		v := &gpStruct.Value{
			Kind: &gpStruct.Value_StringValue{string},
		}

		list.Values[i] = v
	}
	return nil
}
