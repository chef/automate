package server

import (
	"fmt"
	"time"

	tspb "github.com/golang/protobuf/ptypes/timestamp"

	"github.com/golang/protobuf/ptypes"

	"github.com/blang/semver"
	"github.com/gofrs/uuid"
	gp "github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	ingest_api "github.com/chef/automate/components/compliance-service/ingest/ingest"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline"
	event "github.com/chef/automate/components/event-service/server"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/notifications-client/builder"
	"github.com/chef/automate/components/notifications-client/notifier"
	project_update_lib "github.com/chef/automate/lib/authz"
	event_ids "github.com/chef/automate/lib/event"
	log "github.com/sirupsen/logrus"
)

type ComplianceIngestServer struct {
	compliancePipeline pipeline.Compliance
	client             *ingestic.ESClient
	mgrClient          manager.NodeManagerServiceClient
	automateURL        string
	notifierClient     notifier.Notifier
	updateManager      *project_update_lib.DomainProjectUpdateManager
}

var MinimumSupportedInspecVersion = semver.MustParse("2.0.0")

func NewComplianceIngestServer(esClient *ingestic.ESClient, mgrClient manager.NodeManagerServiceClient,
	automateURL string, notifierClient notifier.Notifier, authzProjectsClient iam_v2.ProjectsClient,
	eventServiceClient automate_event.EventServiceClient, configManager *config.ConfigManager) *ComplianceIngestServer {

	compliancePipeline := pipeline.NewCompliancePipeline(esClient, authzProjectsClient)

	updateManager := project_update_lib.NewDomainProjectUpdateManager(esClient, authzProjectsClient, eventServiceClient,
		configManager, event_ids.ComplianceInspecReportProducerID)

	return &ComplianceIngestServer{
		compliancePipeline: compliancePipeline,
		client:             esClient,
		mgrClient:          mgrClient,
		automateURL:        automateURL,
		notifierClient:     notifierClient,
		updateManager:      updateManager,
	}
}

func (srv *ComplianceIngestServer) HandleEvent(ctx context.Context, req *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	log.Debugf("compliance ingest is handling your event %s", req.EventID)

	response := &automate_event.EventResponse{}
	if req.Type.Name == event.ProjectRulesUpdate {
		projectUpdateID, err := getProjectUpdateID(req)
		if err != nil {
			logrus.Errorf("Project Rule Update sent without a ProjectUpdateID eventID %q",
				req.EventID)
			return response, err
		}

		srv.updateManager.Start(projectUpdateID)
	} else if req.Type.Name == event.ProjectRulesCancelUpdate {
		projectUpdateID, err := getProjectUpdateID(req)
		if err != nil {
			logrus.Errorf("Project Rule Update Cancel sent without a ProjectUpdateID. eventID %q",
				req.EventID)
			return response, err
		}

		srv.updateManager.Cancel(projectUpdateID)
	}

	return response, nil
}

func (srv *ComplianceIngestServer) ProjectUpdateStatus(ctx context.Context,
	req *ingest_api.ProjectUpdateStatusReq) (*ingest_api.ProjectUpdateStatusResp, error) {
	time, err := ptypes.TimestampProto(srv.updateManager.EstimatedTimeCompelete())
	if err != nil {
		log.Errorf("Could not convert EstimatedTimeCompelete to protobuf Timestamp %v", err)
		time = &tspb.Timestamp{}
	}
	return &ingest_api.ProjectUpdateStatusResp{
		State:                  srv.updateManager.State(),
		PercentageComplete:     srv.updateManager.PercentageComplete(),
		EstimatedTimeCompelete: time,
	}, nil
}

func getProjectUpdateID(event *automate_event.EventMsg) (string, error) {
	if event.Data != nil && event.Data.Fields != nil && event.Data.Fields["ProjectUpdateID"] != nil &&
		event.Data.Fields["ProjectUpdateID"].GetStringValue() != "" {
		return event.Data.Fields["ProjectUpdateID"].GetStringValue(), nil
	}

	return "", fmt.Errorf("Project Rule Update sent without a ProjectUpdateID eventID: %q", event.EventID)
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

	// if the end_time on the report is not parsable, we want to set
	// the node last_contact time to beg of time, b/c we don't know when
	// the last_contact was
	endTime, err := time.Parse(time.RFC3339, in.EndTime)
	if err != nil {
		logrus.Errorf("ProcessComplianceReport unable to parse report end_time, setting end_time to beg of time")
		endTime = time.Time{}
	}
	endTimeTimestamp, err := ptypes.TimestampProto(endTime)
	if err != nil {
		logrus.Errorf("ProcessComplianceReport unable to parse end_time as proto timestamp, setting end_time timestamp to beg of time")
		endTimeTimestamp = &tspb.Timestamp{}
	}
	logrus.Debugf("Calling sendNodeInfoToManager for report id %s", in.ReportUuid)

	err = s.sendNodeInfoToManager(ctx, manager.NodeMetadata{
		Uuid:            in.NodeUuid,
		Name:            in.NodeName,
		PlatformName:    in.Platform.GetName(),
		PlatformRelease: in.Platform.GetRelease(),
		JobUuid:         in.JobUuid,
		LastContact:     endTimeTimestamp,
		SourceId:        in.SourceId,
		SourceRegion:    in.SourceRegion,
		SourceAccountId: in.SourceAccountId,
		Tags:            in.Tags,
	})
	if err != nil {
		logrus.Errorf("ProcessComplianceReport unable to send node info to manager: %s", err.Error())
	}
	logrus.Debugf("Calling handleNotifications for report id %s", in.ReportUuid)
	err = s.handleNotifications(ctx, in)
	if err != nil {
		logrus.Errorf("ProcessComplianceReport unable to send notification: %s", err.Error())
	}
	logrus.Debugf("Calling compliancePipeline.Run for report id %s", in.ReportUuid)
	err = s.compliancePipeline.Run(in)
	return &gp.Empty{}, err
}

func (s *ComplianceIngestServer) handleNotifications(ctx context.Context, report *compliance.Report) error {
	if s.notifierClient == nil {
		return fmt.Errorf("no notifier client found")
	}

	ev, err := builder.Compliance(s.automateURL, report)
	if err != nil {
		// We treat notification errors as non fatal
		logrus.WithFields(logrus.Fields{"id": report.ReportUuid}).Warnf("Could not build notifications InSpec event: %v", err)
	} else {
		// This happens asynchronously
		s.notifierClient.Send(ctx, ev)
	}
	return nil
}

func (s *ComplianceIngestServer) sendNodeInfoToManager(ctx context.Context, node manager.NodeMetadata) error {
	if s.mgrClient == nil {
		return fmt.Errorf("no manager client found")
	}
	logrus.Debugf("sendNodeInfoToManager handing-over node to manager %+v", s)
	_, err := s.mgrClient.ProcessNode(ctx, &node)
	if err != nil {
		return errors.Wrap(err, "sendNodeInfoToManager error calling ProcessNode")
	}
	return nil
}
