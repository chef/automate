package server

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	ingestProto "github.com/chef/automate/api/external/ingest/request"
	inspecEvent "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	structpb "github.com/golang/protobuf/ptypes/struct"
	"io"
	"strings"

	"github.com/blang/semver"
	"github.com/gofrs/uuid"
	gp "github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	ingest_api "github.com/chef/automate/api/interservice/compliance/ingest/ingest"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline"
	"github.com/chef/automate/components/notifications-client/notifier"

	complianceEvent "github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"

	"github.com/buger/jsonparser"
	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
)
var unmarshaler = jsonpb.Unmarshaler{AllowUnknownFields: true}

type ComplianceIngestServer struct {
	compliancePipeline pipeline.Compliance
	client             *ingestic.ESClient
	mgrClient          manager.NodeManagerServiceClient
	automateURL        string
	notifierClient     notifier.Notifier
}

var MinimumSupportedInspecVersion = semver.MustParse("2.0.0")

func NewComplianceIngestServer(esClient *ingestic.ESClient, mgrClient manager.NodeManagerServiceClient,
	automateURL string, notifierClient notifier.Notifier, authzProjectsClient authz.ProjectsServiceClient,
	messageBufferSize int) *ComplianceIngestServer {

	compliancePipeline := pipeline.NewCompliancePipeline(esClient,
		authzProjectsClient, mgrClient, messageBufferSize, notifierClient, automateURL)

	return &ComplianceIngestServer{
		compliancePipeline: compliancePipeline,
		client:             esClient,
		mgrClient:          mgrClient,
		automateURL:        automateURL,
		notifierClient:     notifierClient,
	}
}

func (srv *ComplianceIngestServer) HandleEvent(ctx context.Context, req *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	log.Debugf("compliance ingest is handling your event %s", req.EventId)

	return nil, status.Error(codes.Unimplemented, "Unimplemented")
}

func (srv *ComplianceIngestServer) ProjectUpdateStatus(ctx context.Context,
	req *ingest_api.ProjectUpdateStatusReq) (*ingest_api.ProjectUpdateStatusResp, error) {
	return nil, status.Error(codes.Unimplemented, "Endpoint no longer used")
}

func (s *ComplianceIngestServer) ProcessComplianceReport(ctx context.Context, in *compliance.Report) (*gp.Empty, error) {
	logrus.Debugf("ProcessComplianceReport with id: %s", in.ReportUuid)
	if s == nil {
		return nil, fmt.Errorf("ProcessComplianceReport, ComplianceIngestServer == nil")
	}

	if len(in.NodeUuid) == 0 {
		return nil, status.Error(codes.InvalidArgument, "invalid report: missing node_uuid")
	}

	_, err := uuid.FromString(in.NodeUuid)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid report: invalid node_uuid: %s", err.Error())
	}

	if len(in.Version) == 0 {
		logrus.Debug("invalid report: missing version")
	} else {
		version, err := semver.Make(in.Version)
		if err != nil {
			logrus.Debugf("invalid report: invalid version number %q: %s", in.Version, err.Error())
		} else {
			if version.LTE(MinimumSupportedInspecVersion) {
				logrus.Debugf("invalid report: Inspec version used to generate the report (%s) is older than minimum supported version (%s)", version, MinimumSupportedInspecVersion)
			}
		}
	}
	logrus.Debugf("Calling compliancePipeline.Run for report id %s", in.ReportUuid)
	err = s.compliancePipeline.Run(in)
	return &gp.Empty{}, err
}

func (s *ComplianceIngestServer) StreamComplianceReport(stream ingest_api.ComplianceIngesterService_StreamComplianceReportServer) error {
	reportData := bytes.Buffer{}
	reportSize := 0
	for {
		req, err := stream.Recv()
		if err == io.EOF {
			log.Print("no more data")
			break
		}

		position := req.GetChunk().GetPosition()
		chunk := req.GetChunk().GetData()
		size := len(chunk)

		log.Printf("received a chunk with size: %d at position %d", size, position)

		reportSize += size

		_, err = reportData.Write(chunk)
		if err != nil {
			logrus.Debugf("Failed to write chunk data: %v", err)
			return fmt.Errorf(codes.Internal.String(), "cannot write chunk data: %v", err)
		}
	}

	res := &compliance.StreamResponse{}
	err := stream.SendAndClose(res)
	if err != nil {
		logrus.Debugf("Cannot send response: %v", err)
		return fmt.Errorf("cannot send response: %v", err)
	}

	logrus.Debugf("Report size:: %d and received size:: %d", len(reportData.Bytes()), reportSize)
	report := ParseBytesToComplianceReport(reportData.Bytes())
	err = s.compliancePipeline.Run(report)
	return nil
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
		RunTimeLimit:     getFloat32IfExists("run_time_limit", body),
		Status:           getStringIfExists("status", body),
		StatusMessage:    getStringIfExists("status_message", body),
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

	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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

	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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
func getIgnoreFailureValue(resource []byte) *structpb.Value {
	fieldString, err := jsonparser.GetString(resource, "ignore_failure")
	if err == nil {
		return &structpb.Value{
			Kind: &structpb.Value_StringValue{
				StringValue: fieldString,
			},
		}
	}

	return &structpb.Value{
		Kind: &structpb.Value_BoolValue{
			BoolValue: getBoolIfExists("ignore_failure", resource),
		},
	}
}

// Returns false if field does not exist
func getBoolIfExists(fieldName string, body []byte) bool {
	fieldBytes, _, _, err := jsonparser.Get(body, fieldName)
	if err == nil {
		b, err := jsonparser.ParseBoolean(fieldBytes)
		// If there is no error (err == nil) return the parsed boolean else return false.
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
			// TODO (tc): Is this err intended to be evaluated below in the if statement?
			// It's currently scoped to the switch statement.
			// nolint: ineffassign
			err = UnmarshalProtoFromString(stringBytes, &pbStruct)
		case jsonparser.Object:
			// If the field is an Object, we can directly unmarshal it from the bytes array
			// TODO (tc): Is this err intended to be evaluated below in the if statement?
			// It's currently scoped to the switch statement.
			// nolint: ineffassign
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

	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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
				StatusMessage:  getStringIfExists("status_message", profile),
			}
			inspecProfiles = append(inspecProfiles, inspecProfile)
		}
	}, "profiles")

	return inspecProfiles
}

func getInspecSupports(profile []byte) []*inspecEvent.Support {
	inspecSupports := make([]*inspecEvent.Support, 0)
	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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
	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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
	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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
	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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
	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
	jsonparser.ArrayEach(profile, func(control []byte, _ jsonparser.ValueType, _ int, err error) {
		if err == nil {
			inspecControl := &inspecEvent.Control{
				Id:                   getStringIfExists("id", control),
				Impact:               getFloat32IfExists("impact", control),
				Title:                getStringIfExists("title", control),
				Code:                 getStringIfExists("code", control),
				Desc:                 getStringIfExists("desc", control),
				SourceLocation:       getInspecSourceLocation(control),
				WaiverData:           getInspecControlWaiverData(control),
				Refs:                 getStructArray("refs", control),
				Tags:                 getStructIfExists("tags", control),
				Results:              getInspecResults(control),
				RemovedResultsCounts: getInspecControlRemovedResultsCounts(control),
			}
			inspecControls = append(inspecControls, inspecControl)
		}
	}, "controls")
	return inspecControls
}

func getInspecResults(control []byte) []*inspecEvent.Result {
	inspecResults := make([]*inspecEvent.Result, 0)
	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
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

func getInspecControlWaiverData(control []byte) *inspecEvent.WaiverData {
	var dataToReturn *inspecEvent.WaiverData
	sourceLocationBytes, _, _, err := jsonparser.Get(control, "waiver_data")
	if err == nil {
		dataToReturn = &inspecEvent.WaiverData{
			ExpirationDate:     getStringIfExists("expiration_date", sourceLocationBytes),
			Justification:      getStringIfExists("justification", sourceLocationBytes),
			Run:                getBoolIfExists("run", sourceLocationBytes),
			SkippedDueToWaiver: getBoolIfExists("skipped_due_to_waiver", sourceLocationBytes),
			Message:            getStringIfExists("message", sourceLocationBytes),
		}
	}
	return dataToReturn
}

func getInspecControlRemovedResultsCounts(control []byte) *inspecEvent.RemovedResultsCounts {
	var dataToReturn *inspecEvent.RemovedResultsCounts
	sourceLocationBytes, _, _, err := jsonparser.Get(control, "removed_results_counts")
	if err == nil {
		dataToReturn = &inspecEvent.RemovedResultsCounts{
			Failed:  getInt32IfExists("failed", sourceLocationBytes),
			Skipped: getInt32IfExists("skipped", sourceLocationBytes),
			Passed:  getInt32IfExists("passed", sourceLocationBytes),
		}
	}
	return dataToReturn
}

func getStructArray(fieldName string, body []byte) []*structpb.Struct {
	pStructs := make([]*structpb.Struct, 0)
	// TODO (tc): Seems like we should check if the json fails to parse?
	// nolint: errcheck
	jsonparser.ArrayEach(body, func(structBytes []byte, dataType jsonparser.ValueType, _ int, err error) {
		if err == nil {
			var pStruct structpb.Struct

			switch dataType {
			case jsonparser.String:
				// If the field is a String we will parse it to remove the scaped characters
				stringBytes, err := jsonparser.ParseString(structBytes)
				if err == nil {
					// TODO (tc): Is this err intended to be evaluated below in the if statement?
					// It's currently scoped to this case statement due to the assignment from jsonparser.ParseString.
					// nolint: ineffassign
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

// UnmarshalProtoFromBytes unmarshals a message body in byte to a generic protobuf msg
func UnmarshalProtoFromBytes(body []byte, pb proto.Message) error {
	return unmarshaler.Unmarshal(bytes.NewReader(body), pb)
}

// UnmarshalProtoFromString unmarshals a message body (string) to a generic protobuf msg
func UnmarshalProtoFromString(body string, pb proto.Message) error {
	return unmarshaler.Unmarshal(strings.NewReader(body), pb)
}
