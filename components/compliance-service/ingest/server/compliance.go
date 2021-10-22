package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"

	"github.com/blang/semver"
	"github.com/gofrs/uuid"
	gp "github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
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
	messageBufferSize int, isSupportLCR bool) *ComplianceIngestServer {

	compliancePipeline := pipeline.NewCompliancePipeline(esClient,
		authzProjectsClient, mgrClient, messageBufferSize, notifierClient, automateURL, isSupportLCR)

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

func SendComplianceReport(ctx context.Context, in *compliance.Report, s *ComplianceIngestServer) error {
	logrus.Debugf("ProcessComplianceReport with id: %s", in.ReportUuid)
	if s == nil {
		return fmt.Errorf("ProcessComplianceReport, ComplianceIngestServer == nil")
	}

	if len(in.NodeUuid) == 0 {
		return status.Error(codes.InvalidArgument, "invalid report: missing node_uuid")
	}

	_, err := uuid.FromString(in.NodeUuid)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, "invalid report: invalid node_uuid: %s", err.Error())
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
	return s.compliancePipeline.Run(in)
}

// ProcessComplianceReport receives messages in chunks and creates report
func (s *ComplianceIngestServer) ProcessComplianceReport(stream ingest_api.ComplianceIngesterService_ProcessComplianceReportServer) error {
	var err error
	reportData := bytes.Buffer{}

	// loop until all data is read in chunks or any error occur.
	for {
		err = contextError(stream.Context())
		if err != nil {
			return err
		}
		logrus.Debug("waiting to receive more data")

		req, err := stream.Recv()
		if err == io.EOF {
			logrus.Debug("no more data")
			break
		}
		if err != nil {
			logrus.Debugf("cannot receive chunk data: %v", err)
			return err
		}
		chunk := req.GetContent()
		logrus.Debugf("received a chunk with size: %d", len(chunk))
		_, err = reportData.Write(chunk)
		if err != nil {
			logrus.Debugf("cannot write chunk data: %v", err)
			return err
		}
	}
	in := &compliance.Report{}
	err = json.Unmarshal(reportData.Bytes(), &in)
	if err != nil {
		return fmt.Errorf("error in converting report bytes to compliance report struct: %w", err)
	}
	err = SendComplianceReport(context.Background(), in, s)
	if err != nil {
		return err
	}
	return stream.SendAndClose(&gp.Empty{})
}

func contextError(ctx context.Context) error {
	switch ctx.Err() {
	case context.Canceled:
		return errors.Wrap(ctx.Err(), "request is canceled")
	case context.DeadlineExceeded:
		return errors.Wrap(ctx.Err(), "deadline is exceeded")
	default:
		return nil
	}
}
