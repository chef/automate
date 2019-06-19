//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package gateway

import (
	"bytes"
	"context"
	"errors"
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/buger/jsonparser"
	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
	protobufStruct "github.com/golang/protobuf/ptypes/struct"
	structpb "github.com/golang/protobuf/ptypes/struct"
	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/status"

	ingestProto "github.com/chef/automate/api/external/ingest/request"
	complianceEvent "github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	inspecEvent "github.com/chef/automate/components/compliance-service/ingest/events/inspec"
)

type dataCollectorMsgType int

const (
	_ dataCollectorMsgType = iota
	UnknownMsg
	ChefActionMsg
	ChefRunStartMsg
	ChefRunConvergeMsg
	LivenessAgentMsg
	ComplianceReportMsg
)

// Custom Unmarshaler
var unmarshaler = jsonpb.Unmarshaler{AllowUnknownFields: true}

func (s *Server) dataCollectorHandler(w http.ResponseWriter, r *http.Request) {
	ctx, cancel := context.WithCancel(r.Context())
	defer cancel()
	r = r.WithContext(ctx)

	// Auth Policy
	const (
		resourceV1 = "ingest:unified_events"
		actionV1   = "create"
		resourceV2 = "infra:ingest:unifiedEvents"
		actionV2   = "infra:ingest:create"
	)

	ctx, err := s.authRequest(r, resourceV1, actionV1, resourceV2, actionV2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}

	// Read the body in bytes
	body, err := ioutil.ReadAll(r.Body)
	r.Body.Close()
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	switch parseMessageType(body) {
	case ChefRunStartMsg:
		log.WithFields(log.Fields{
			"message_type": "run_start",
		}).Debug("Unsupported (currently not processing)")
	case ChefRunConvergeMsg:
		s.processChefRun(ctx, w, body)
	case ChefActionMsg:
		s.processChefAction(ctx, w, body)
	case LivenessAgentMsg:
		s.processLivenessPing(ctx, w, body)
	case ComplianceReportMsg:
		s.processComplianceReport(ctx, w, body)
	default:
		// if we reach this, we don't know what to do with the message
		http.Error(w, "message type not found", http.StatusBadRequest) // Code Unimplemented? Unsupported?
	}
}

// processChefRun process a ChefRun message
func (s *Server) processChefRun(ctx context.Context, w http.ResponseWriter, body []byte) {
	// Create run object from the request body
	run := ParseBytesToChefRun(body)

	// Notifications
	s.AsyncChefRunNotification(ctx, run)

	// Talk directly to ingest-service
	chefIngester, err := s.clientsFactory.ChefIngesterClient()
	if err != nil {
		http.Error(w, "GRPC ChefIngester Unavailable", http.StatusServiceUnavailable)
		return
	}

	_, err = chefIngester.ProcessChefRun(ctx, run)
	if err != nil {
		http.Error(w, err.Error(), runtime.HTTPStatusFromCode(status.Code(err)))
	}
}

// processChefAction process a ChefAction message
func (s *Server) processChefAction(ctx context.Context, w http.ResponseWriter, body []byte) {
	// Create action object from the request body
	action := ParseBytesToChefAction(body)

	// Talk directly to ingest-service
	chefIngester, err := s.clientsFactory.ChefIngesterClient()
	if err != nil {
		http.Error(w, "GRPC ChefIngester Unavailable", http.StatusServiceUnavailable)
		return
	}

	_, err = chefIngester.ProcessChefAction(ctx, action)
	if err != nil {
		http.Error(w, err.Error(), runtime.HTTPStatusFromCode(status.Code(err)))
	}
}

// processLivenessPing process a LivenessPing message
func (s *Server) processLivenessPing(ctx context.Context, w http.ResponseWriter, body []byte) {
	// Create liveness object from the request body
	liveness := ParseBytesToLivenessPing(body)

	// Talk directly to ingest-service
	chefIngester, err := s.clientsFactory.ChefIngesterClient()
	if err != nil {
		http.Error(w, "GRPC ChefIngester Unavailable", http.StatusServiceUnavailable)
		return
	}

	_, err = chefIngester.ProcessLivenessPing(ctx, liveness)
	if err != nil {
		http.Error(w, err.Error(), runtime.HTTPStatusFromCode(status.Code(err)))
	}
}

// processComplianceReport process a ComplianceReport message
func (s *Server) processComplianceReport(ctx context.Context, w http.ResponseWriter, body []byte) {
	// Create report object from the request body
	report := ParseBytesToComplianceReport(body)

	// Talk directly to the ingest-compliance-service
	complianceIngester, err := s.clientsFactory.ComplianceIngesterClient()
	if err != nil {
		http.Error(w, "GRPC ComplianceIngester Unavailable", http.StatusServiceUnavailable)
		return
	}

	_, err = complianceIngester.ProcessComplianceReport(ctx, report)
	if err != nil {
		http.Error(w, err.Error(), runtime.HTTPStatusFromCode(status.Code(err)))
	}
}

// parseMessageType parses a message in bytes to detect its type
func parseMessageType(message []byte) dataCollectorMsgType {
	// isChefData?
	messageTypeB, _, _, err := jsonparser.Get(message, "message_type")
	if err == nil {
		switch string(messageTypeB) {
		case "action":
			return ChefActionMsg
		case "run_converge":
			return ChefRunConvergeMsg
		case "run_start":
			return ChefRunStartMsg
		}
	}

	// isLivenessAgent?
	eventTypeB, _, _, err := jsonparser.Get(message, "event_type")
	if err == nil && string(eventTypeB) == "node_ping" {
		return LivenessAgentMsg
	}

	// isComplianceReport?
	_, _, _, err = jsonparser.Get(message, "profiles")
	if err == nil {
		// We do not need to cast the profiles struct, if it exists then the
		// message is a ComplianceReportMsg, lets not use extra resources pls
		return ComplianceReportMsg
	}

	return UnknownMsg
}

// UnmarshalProtoFromBytes unmarshals a message body in byte to a generic protobuf msg
func UnmarshalProtoFromBytes(body []byte, pb proto.Message) error {
	return unmarshaler.Unmarshal(bytes.NewReader(body), pb)
}

// UnmarshalProtoFromString unmarshals a message body (string) to a generic protobuf msg
func UnmarshalProtoFromString(body string, pb proto.Message) error {
	return unmarshaler.Unmarshal(strings.NewReader(body), pb)
}

// ParseBytesToChefRun converts a message body in bytes to a ChefRun protobuf msg
func ParseBytesToChefRun(body []byte) *ingestProto.Run {
	// We only need a partial Run message for notifications, to speed up the ingestion
	// of messages we are passing the entire content of the request in bytes, then the
	// ingest-service will use it to unmarshal the message
	return &ingestProto.Run{
		Id:                   getStringIfExists("id", body),
		RunId:                getStringIfExists("run_id", body),
		EntityUuid:           getStringIfExists("entity_uuid", body),
		MessageVersion:       getStringIfExists("message_version", body),
		MessageType:          getStringIfExists("message_type", body),
		NodeName:             getStringIfExists("node_name", body),
		StartTime:            getStringIfExists("start_time", body),
		ChefServerFqdn:       getStringIfExists("chef_server_fqdn", body),
		EndTime:              getStringIfExists("end_time", body),
		Status:               getStringIfExists("status", body),
		UpdatedResourceCount: getInt32IfExists("updated_resource_count", body),
		Resources:            getResources(body),
		Error:                getError(body),
		Content:              body, // This is the key for us to speed up our ingestion of msgs
	}
}

// ParseBytesToChefAction converts a message body in bytes to a ChefAction protobuf msg
func ParseBytesToChefAction(body []byte) *ingestProto.Action {
	return &ingestProto.Action{
		Id:               getStringIfExists("id", body),
		MessageType:      getStringIfExists("message_type", body),
		MessageVersion:   getStringIfExists("message_version", body),
		EntityName:       getStringIfExists("entity_name", body),
		EntityType:       getStringIfExists("entity_type", body),
		Task:             getStringIfExists("task", body),
		OrganizationName: getStringIfExists("organization_name", body),
		RemoteHostname:   getStringIfExists("remote_hostname", body),
		RunId:            getStringIfExists("run_id", body),
		NodeId:           getStringIfExists("node_id", body),
		RecordedAt:       getStringIfExists("recorded_at", body),
		RemoteRequestId:  getStringIfExists("remote_request_id", body),
		RequestId:        getStringIfExists("request_id", body),
		RequestorName:    getStringIfExists("requestor_name", body),
		RequestorType:    getStringIfExists("requestor_type", body),
		ServiceHostname:  getStringIfExists("service_hostname", body),
		UserAgent:        getStringIfExists("user_agent", body),
		ParentType:       getStringIfExists("parent_type", body),
		ParentName:       getStringIfExists("parent_name", body),
		RevisionId:       getStringIfExists("revision_id", body),
		Content:          body, // This is the key for us to speed up our ingestion of msgs
	}
}

// ParseBytesToLivenessPing converts a message body in bytes to a Liveness protobuf msg
func ParseBytesToLivenessPing(body []byte) *ingestProto.Liveness {
	return &ingestProto.Liveness{
		EventType:        getStringIfExists("event_type", body),
		EntityUuid:       getStringIfExists("entity_uuid", body),
		ChefServerFqdn:   getStringIfExists("chef_server_fqdn", body),
		Source:           getStringIfExists("source", body),
		MessageVersion:   getStringIfExists("message_version", body),
		OrganizationName: getStringIfExists("organization_name", body),
		NodeName:         getStringIfExists("node_name", body),
	}
}

// ParseBytesToComplianceReport converts a message body in bytes to a Report protobuf msg
func ParseBytesToComplianceReport(body []byte) *complianceEvent.Report {
	return &complianceEvent.Report{
		Version:          getStringIfExists("version", body),
		Platform:         getInspecPlatform(body),
		Statistics:       getInspecStatistics(body),
		Profiles:         getInspecProfiles(body),
		OtherChecks:      getStringArray("other_checks", body),
		ReportUuid:       getStringIfExists("report_uuid", body),
		NodeUuid:         getStringIfExists("node_uuid", body),
		JobUuid:          getStringIfExists("job_uuid", body),
		NodeName:         getStringIfExists("node_name", body),
		Environment:      getStringIfExists("environment", body),
		Roles:            getStringArray("roles", body),
		Recipes:          getStringArray("recipes", body),
		EndTime:          getStringIfExists("end_time", body),
		Type:             getStringIfExists("type", body),
		SourceId:         getStringIfExists("source_id", body),
		SourceRegion:     getStringIfExists("source_region", body),
		SourceAccountId:  getStringIfExists("source_account_id", body),
		PolicyName:       getStringIfExists("policy_name", body),
		PolicyGroup:      getStringIfExists("policy_group", body),
		OrganizationName: getStringIfExists("organization_name", body),
		SourceFqdn:       getStringIfExists("source_fqdn", body),
		ChefTags:         getStringArray("chef_tags", body),
		Ipaddress:        getStringIfExists("ipaddress", body),
		Fqdn:             getStringIfExists("fqdn", body),
	}
}

// Returns empty string if field does not exist
func getStringIfExists(fieldName string, body []byte) string {
	fieldString, err := jsonparser.GetString(body, fieldName)
	if err == nil {
		return fieldString
	}
	return ""
}

// Returns 0 if field does not exist
func getInt32IfExists(fieldName string, body []byte) int32 {
	fieldBytes, _, _, err := jsonparser.Get(body, fieldName)
	if err == nil {
		i64, err := jsonparser.ParseInt(fieldBytes)
		if err == nil {
			return int32(i64)
		}
	}
	return 0
}

// Returns empty array of strings if field does not exist
func getStringArray(fieldName string, body []byte) []string {
	strs := make([]string, 0)

	jsonparser.ArrayEach(body, func(value []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			strs = append(strs, string(value))
		}
	}, fieldName)

	return strs
}

// Returns empty array of resources if field 'resources' does not exist
func getResources(body []byte) []*ingestProto.Resource {
	resources := make([]*ingestProto.Resource, 0)

	jsonparser.ArrayEach(body, func(resource []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			rReq := &ingestProto.Resource{
				Type:            getStringIfExists("type", resource),
				Name:            getStringIfExists("name", resource),
				Id:              getStringIfExists("id", resource),
				After:           getStructIfExists("after", resource),
				Before:          getStructIfExists("before", resource),
				Duration:        getStringIfExists("duration", resource),
				Delta:           getStringIfExists("delta", resource),
				CookbookName:    getStringIfExists("cookbook_name", resource),
				CookbookVersion: getStringIfExists("cookbook_version", resource),
				IgnoreFailure:   getIgnoreFailureValue(resource),
				Status:          getStringIfExists("status", resource),
				RecipeName:      getStringIfExists("recipe_name", resource),
				Conditional:     getStringIfExists("conditional", resource),
				Result:          getStringIfExists("result", resource),
			}
			resources = append(resources, rReq)
		}
	}, "resources")

	return resources
}

// The ignore_failure field is either a string or a boolean.
// Defaults to false.
func getIgnoreFailureValue(resource []byte) *protobufStruct.Value {
	fieldString, err := jsonparser.GetString(resource, "ignore_failure")
	if err == nil {
		return &protobufStruct.Value{
			Kind: &protobufStruct.Value_StringValue{
				StringValue: fieldString,
			},
		}
	}

	return &protobufStruct.Value{
		Kind: &protobufStruct.Value_BoolValue{
			BoolValue: getBoolIfExists("ignore_failure", resource),
		},
	}
}

// Returns false if field does not exist
func getBoolIfExists(fieldName string, body []byte) bool {
	fieldBytes, _, _, err := jsonparser.Get(body, fieldName)
	if err == nil {
		b, err := jsonparser.ParseBoolean(fieldBytes)
		if err == nil {
			return b
		}
	}
	return false
}

func getError(body []byte) *ingestProto.Error {
	var pErr *ingestProto.Error
	errorBytes, _, _, err := jsonparser.Get(body, "error")
	if err == nil {
		description := &ingestProto.Description{}
		descBytes, _, _, err := jsonparser.Get(errorBytes, "description")
		if err == nil {
			description = &ingestProto.Description{
				Title:    getStringIfExists("title", descBytes),
				Sections: getStructArray("sections", descBytes),
			}
		}
		var (
			class     = getStringIfExists("class", errorBytes)
			message   = getStringIfExists("message", errorBytes)
			backtrace = getStringArray("backtrace", errorBytes)
		)
		pErr = &ingestProto.Error{
			Class:       class,
			Message:     message,
			Backtrace:   backtrace,
			Description: description,
		}
	}
	return pErr
}

func getExpandedRunList(body []byte) *ingestProto.ExpandedRunList {
	var expandedRunList *ingestProto.ExpandedRunList
	expandedRunListBytes, _, _, err := jsonparser.Get(body, "expanded_run_list")
	if err == nil {

		pRunLists := make([]*ingestProto.RunList, 0)
		jsonparser.ArrayEach(body, func(runlist []byte, _ jsonparser.ValueType, _ int, err error) {
			if err != nil {
				pRunList := &ingestProto.RunList{
					Type:    getStringIfExists("type", runlist),
					Name:    getStringIfExists("name", runlist),
					Version: getStringIfExists("version", runlist),
					Skipped: getBoolIfExists("skipped", runlist),
				}
				pRunLists = append(pRunLists, pRunList)
			}
		}, "deprecation")

		expandedRunList = &ingestProto.ExpandedRunList{
			Id:      getStringIfExists("id", expandedRunListBytes),
			RunList: pRunLists,
		}

	}
	return expandedRunList
}

// Returns an empty struct if field does not exist
func getStructIfExists(fieldName string, body []byte) *structpb.Struct {
	fieldBytes, dataType, _, err := jsonparser.Get(body, fieldName)
	if err == nil {
		var pbStruct structpb.Struct

		switch dataType {
		case jsonparser.String:
			// If the field is a String we will parse it to remove the scaped characters
			stringBytes, err := jsonparser.ParseString(fieldBytes)
			if err != nil {
				return &structpb.Struct{}
			}
			err = UnmarshalProtoFromString(stringBytes, &pbStruct)
		case jsonparser.Object:
			// If the field is an Object, we can directly unmarshal it from the bytes array
			err = UnmarshalProtoFromBytes(fieldBytes, &pbStruct)
		default:
			// Anything else, we can't convert into a Struct, return empty
			return &structpb.Struct{}
		}

		if err == nil {
			return &pbStruct
		}
	}
	return &structpb.Struct{}
}

func getInspecPlatform(body []byte) *inspecEvent.Platform {
	var inspecPlatform *inspecEvent.Platform
	platformBytes, _, _, err := jsonparser.Get(body, "platform")
	if err == nil {
		inspecPlatform = &inspecEvent.Platform{
			Name:    getStringIfExists("name", platformBytes),
			Release: getStringIfExists("release", platformBytes),
		}
	}
	return inspecPlatform
}

func getInspecStatistics(body []byte) *inspecEvent.Statistics {
	var inspecStatistics *inspecEvent.Statistics
	statisticsBytes, _, _, err := jsonparser.Get(body, "statistics")
	if err == nil {
		inspecStatistics = &inspecEvent.Statistics{
			Duration: getFloat32IfExists("duration", statisticsBytes),
		}
	}
	return inspecStatistics
}

// Returns 0 if field does not exist
func getFloat32IfExists(fieldName string, body []byte) float32 {
	fieldFloat64, err := jsonparser.GetFloat(body, fieldName)
	if err == nil {
		return float32(fieldFloat64)
	}
	return 0
}

func getInspecProfiles(body []byte) []*inspecEvent.Profile {
	inspecProfiles := make([]*inspecEvent.Profile, 0)

	jsonparser.ArrayEach(body, func(profile []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecProfile := &inspecEvent.Profile{
				Name:           getStringIfExists("name", profile),
				Title:          getStringIfExists("title", profile),
				Version:        getStringIfExists("version", profile),
				Summary:        getStringIfExists("summary", profile),
				Maintainer:     getStringIfExists("maintainer", profile),
				License:        getStringIfExists("license", profile),
				Copyright:      getStringIfExists("copyright", profile),
				CopyrightEmail: getStringIfExists("copyright_email", profile),
				Sha256:         getStringIfExists("sha256", profile),
				Controls:       getInspecControls(profile),
				Supports:       getInspecSupports(profile),
				Attributes:     getInspecAttributes(profile),
				Depends:        getInspecDependencies(profile),
				Groups:         getInspecGroups(profile),
				ParentProfile:  getStringIfExists("parent_profile", profile),
				Status:         getStringIfExists("status", profile),
				SkipMessage:    getStringIfExists("skip_message", profile),
			}
			inspecProfiles = append(inspecProfiles, inspecProfile)
		}
	}, "profiles")

	return inspecProfiles
}

func getInspecSupports(profile []byte) []*inspecEvent.Support {
	inspecSupports := make([]*inspecEvent.Support, 0)
	jsonparser.ArrayEach(profile, func(support []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecSupport := &inspecEvent.Support{
				Inspec:   getStringIfExists("inspec", support),
				OsName:   getStringIfExists("os_name", support),
				OsFamily: getStringIfExists("os_family", support),
				Release:  getStringIfExists("release", support),
			}
			inspecSupports = append(inspecSupports, inspecSupport)
		}
	}, "supports")
	return inspecSupports
}

func getInspecAttributes(profile []byte) []*inspecEvent.Attribute {
	inspecAttributes := make([]*inspecEvent.Attribute, 0)
	jsonparser.ArrayEach(profile, func(attribute []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecAttribute := &inspecEvent.Attribute{
				Name:    getStringIfExists("name", attribute),
				Options: getStructIfExists("options", attribute),
			}
			inspecAttributes = append(inspecAttributes, inspecAttribute)
		}
	}, "attributes")
	return inspecAttributes
}

func getInspecDependencies(profile []byte) []*inspecEvent.Dependency {
	inspecDependencies := make([]*inspecEvent.Dependency, 0)
	jsonparser.ArrayEach(profile, func(dependency []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecDependency := &inspecEvent.Dependency{
				Name:        getStringIfExists("name", dependency),
				Url:         getStringIfExists("url", dependency),
				Path:        getStringIfExists("path", dependency),
				Git:         getStringIfExists("git", dependency),
				Branch:      getStringIfExists("branch", dependency),
				Tag:         getStringIfExists("tag", dependency),
				Commit:      getStringIfExists("commit", dependency),
				Version:     getStringIfExists("version", dependency),
				Supermarket: getStringIfExists("supermarket", dependency),
				Compliance:  getStringIfExists("compliance", dependency),
			}
			inspecDependencies = append(inspecDependencies, inspecDependency)
		}
	}, "dependencies")
	return inspecDependencies
}

func getInspecGroups(profile []byte) []*inspecEvent.Group {
	inspecGroups := make([]*inspecEvent.Group, 0)
	jsonparser.ArrayEach(profile, func(group []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecGroup := &inspecEvent.Group{
				Id:       getStringIfExists("id", group),
				Title:    getStringIfExists("title", group),
				Controls: getStringArray("controls", group),
			}
			inspecGroups = append(inspecGroups, inspecGroup)
		}
	}, "groups")
	return inspecGroups
}

func getInspecControls(profile []byte) []*inspecEvent.Control {
	inspecControls := make([]*inspecEvent.Control, 0)
	jsonparser.ArrayEach(profile, func(control []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecControl := &inspecEvent.Control{
				Id:             getStringIfExists("id", control),
				Impact:         getFloat32IfExists("impact", control),
				Title:          getStringIfExists("title", control),
				Code:           getStringIfExists("code", control),
				Desc:           getStringIfExists("desc", control),
				SourceLocation: getInspecSourceLocation(control),
				Refs:           getStructArray("refs", control),
				Tags:           getStructIfExists("tags", control),
				Results:        getInspecResults(control),
			}
			inspecControls = append(inspecControls, inspecControl)
		}
	}, "controls")
	return inspecControls
}

func getInspecResults(control []byte) []*inspecEvent.Result {
	inspecResults := make([]*inspecEvent.Result, 0)
	jsonparser.ArrayEach(control, func(result []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecResult := &inspecEvent.Result{
				Status:      getStringIfExists("status", result),
				CodeDesc:    getStringIfExists("code_desc", result),
				RunTime:     getFloat32IfExists("run_time", result),
				StartTime:   getStringIfExists("start_time", result),
				Resource:    getStringIfExists("resource", result),
				Message:     getStringIfExists("message", result),
				SkipMessage: getStringIfExists("skip_message", result),
			}
			inspecResults = append(inspecResults, inspecResult)
		}
	}, "results")
	return inspecResults
}

func getInspecSourceLocation(control []byte) *inspecEvent.SourceLocation {
	var inspecSourceLocation *inspecEvent.SourceLocation
	sourceLocationBytes, _, _, err := jsonparser.Get(control, "source_location")
	if err == nil {
		inspecSourceLocation = &inspecEvent.SourceLocation{
			Ref:  getStringIfExists("ref", sourceLocationBytes),
			Line: getInt32IfExists("line", sourceLocationBytes),
		}
	}
	return inspecSourceLocation
}

func getStructArray(fieldName string, body []byte) []*structpb.Struct {
	pStructs := make([]*structpb.Struct, 0)
	jsonparser.ArrayEach(body, func(structBytes []byte, dataType jsonparser.ValueType, _ int, err error) {
		if err == nil {
			var pStruct structpb.Struct

			switch dataType {
			case jsonparser.String:
				// If the field is a String we will parse it to remove the scaped characters
				stringBytes, err := jsonparser.ParseString(structBytes)
				if err == nil {
					err = UnmarshalProtoFromString(stringBytes, &pStruct)
				}
			case jsonparser.Object:
				// If the field is an Object, we can directly unmarshal it from the bytes array
				err = UnmarshalProtoFromBytes(structBytes, &pStruct)
			default:
				// Anything else, we can't convert into a Struct, mark it as an error
				err = errors.New("Invalid data type for a pb.Struct")
			}

			if err == nil {
				pStructs = append(pStructs, &pStruct)
			}
		}
	}, fieldName)
	return pStructs
}
