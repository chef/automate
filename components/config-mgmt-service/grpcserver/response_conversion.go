package grpcserver

import (
	"encoding/json"
	"strings"

	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	ingestBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	google_protobuf "github.com/golang/protobuf/ptypes/struct"
)

func toResponseRun(run backend.Run) (*response.Run, error) {
	chefError, err := toResponseError(run.Error)
	startTime, _ := ptypes.TimestampProto(run.StartTime.UTC())
	endTime, _ := ptypes.TimestampProto(run.EndTime.UTC())
	return &response.Run{
		Id:                   run.RunID,
		NodeId:               run.EntityUuid,
		NodeName:             run.NodeName,
		Organization:         run.OrganizationName,
		StartTime:            startTime,
		EndTime:              endTime,
		Source:               run.Source,
		Status:               run.Status,
		TotalResourceCount:   int32(run.TotalResourceCount),
		UpdatedResourceCount: int32(run.UpdatedResourceCount),
		ChefVersion:          run.ChefVersion,
		UptimeSeconds:        int32(run.UptimeSeconds),
		Environment:          run.Environment,
		Fqdn:                 run.Fqdn,
		SourceFqdn:           run.SourceFqdn,
		Ipaddress:            toString(run.Ipaddress),
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
		Error:                chefError,
	}, err
}

func toString(o interface{}) string {
	s, ok := o.(string)

	if ok {
		return s
	}

	return ""
}

func toResponseError(chefError ingestBackend.ChefError) (*response.ChefError, error) {
	chefErrorResponse := &response.ChefError{
		Class:     chefError.Class,
		Message:   chefError.Message,
		Backtrace: chefError.Backtrace,
	}

	if chefError.Description.Title != "" {
		sections, err := toStructCollection(chefError.Description.Sections)
		if err != nil {
			return chefErrorResponse, err
		}

		chefErrorResponse.Description = &response.Description{
			Title:    chefError.Description.Title,
			Sections: sections,
		}
	}

	return chefErrorResponse, nil
}

func toStructCollection(sections []map[string]interface{}) ([]*google_protobuf.Struct, error) {
	structCollection := make([]*google_protobuf.Struct, len(sections))

	for index, section := range sections {
		protoStruct, err := toStruct(section)
		if err != nil {
			return nil, err
		}

		structCollection[index] = protoStruct
	}

	return structCollection, nil
}

func toStruct(section map[string]interface{}) (*google_protobuf.Struct, error) {
	protoStruct := new(google_protobuf.Struct)

	jsonBytes, err := json.Marshal(section)
	if err != nil {
		return protoStruct, err
	}

	err = protoFromJSON(string(jsonBytes), protoStruct)
	return protoStruct, err
}

func protoFromJSON(content string, pb proto.Message) error {
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	return unmarshaler.Unmarshal(strings.NewReader(content), pb)
}

func getMessageRawJSON(message proto.Message) (string, error) {
	return (&jsonpb.Marshaler{OrigName: true}).MarshalToString(message)
}

func toResponseDeprecations(deprecations []ingestBackend.Deprecation) []*response.Deprecation {
	responseDeprecations := make([]*response.Deprecation, len(deprecations))

	for index, deprecation := range deprecations {
		responseDeprecations[index] = &response.Deprecation{
			Message:  deprecation.Message,
			Url:      deprecation.URL,
			Location: deprecation.Location,
		}
	}

	return responseDeprecations
}

func toResponseExpandedRunList(expandedRunList ingestBackend.ExpandedRunList) *response.ExpandedRunList {
	var responseExpandedRunList response.ExpandedRunList

	responseExpandedRunList.Id = expandedRunList.ID
	responseExpandedRunList.RunList = make([]*response.RunList, len(expandedRunList.RunList))
	for index, runList := range expandedRunList.RunList {
		responseExpandedRunList.RunList[index] = toResponseRunList(runList)
	}

	return &responseExpandedRunList
}

func toResponseRunList(runListItem ingestBackend.ExpandedRunListRunList) *response.RunList {
	children := make([]*response.RunList, len(runListItem.Children))
	for index, runListChild := range runListItem.Children {
		children[index] = toResponseRunList(runListChild)
	}

	return &response.RunList{
		Type:     runListItem.Type,
		Name:     runListItem.Name,
		Version:  toString(runListItem.Version),
		Skipped:  runListItem.Skipped,
		Children: children,
	}
}

func toResponseResources(resources []ingestBackend.Resource) []*response.Resource {
	responseResources := make([]*response.Resource, len(resources))

	for index, resource := range resources {
		responseResources[index] = &response.Resource{
			Type:            resource.Type,
			Name:            resource.Name,
			Id:              resource.ID,
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
