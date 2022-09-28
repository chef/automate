package server

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"time"

	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/lib/cereal"

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
	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline"
	"github.com/chef/automate/components/notifications-client/notifier"
)

type ComplianceIngestServer struct {
	compliancePipeline   pipeline.Compliance
	client               *ingestic.ESClient
	mgrClient            manager.NodeManagerServiceClient
	automateURL          string
	notifierClient       notifier.Notifier
	enableLargeReporting bool
}

var MinimumSupportedInspecVersion = semver.MustParse("2.0.0")

func NewComplianceIngestServer(esClient *ingestic.ESClient, mgrClient manager.NodeManagerServiceClient, reportMgrClient report_manager.ReportManagerServiceClient,
	automateURL string, notifierClient notifier.Notifier, authzProjectsClient authz.ProjectsServiceClient,
	messageBufferSize int, enableLargeReporting bool, cerealManager *cereal.Manager) *ComplianceIngestServer {

	compliancePipeline := pipeline.NewCompliancePipeline(esClient, authzProjectsClient, mgrClient,
		reportMgrClient, messageBufferSize, notifierClient, automateURL, enableLargeReporting, cerealManager)

	return &ComplianceIngestServer{
		compliancePipeline:   compliancePipeline,
		client:               esClient,
		mgrClient:            mgrClient,
		automateURL:          automateURL,
		notifierClient:       notifierClient,
		enableLargeReporting: enableLargeReporting,
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
	jsonBytes := reportData.Bytes()
	reportSize := len(jsonBytes)
	if !s.enableLargeReporting && reportSize > 4194304 {
		err := fmt.Errorf("received message larger than max (%d vs %d), please enable large reporting to process big reports", reportSize, 4194304)
		logrus.Error(err)
		return err
	}

	start1 := time.Now()
	in := gateway.ParseBytesToComplianceReport(jsonBytes)
	fmt.Println("::::::: old approach time taken", time.Since(start1))

	// in := &compliance.Report{}
	// start2 := time.Now()
	// str := r(string(jsonBytes), 0)
	// err = json.Unmarshal([]byte(str), &in)
	// if err != nil {
	// 	return fmt.Errorf("error in converting report bytes to compliance report struct: %w", err)
	// }
	// fmt.Println("received report ::::::::::::: 22222222222", in.GetProfiles()[0].GetAttributes()[0].Options.Fields["filesystem"], time.Since(start2))

	// in := &compliance.Report{}
	// re := regexp.MustCompile(`""\s*:`)
	// bb := bytes.Buffer{}

	// t := time.Now()
	// split := re.Split(string(jsonBytes), -1)

	// for i := range split {
	// 	if i < len(split)-1 {
	// 		bb.WriteString(split[i] + fmt.Sprintf(`"unknown-%d":`, i))
	// 	} else {
	// 		bb.WriteString(split[i])
	// 	}
	// }
	// err = json.Unmarshal(bb.Bytes(), &in)
	// if err != nil {
	// 	return fmt.Errorf("error in converting report bytes to compliance report struct: %w", err)
	// }
	// fmt.Println("::::::: new approach time taken", time.Since(t))

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
