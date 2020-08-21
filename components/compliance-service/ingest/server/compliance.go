package server

import (
	"context"
	"fmt"

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
)

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
