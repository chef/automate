package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"time"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	cc_reporting "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/objstore"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/chef/automate/components/report-manager-service/utils"
	"github.com/chef/automate/components/report-manager-service/worker"
	"github.com/chef/automate/lib/cereal"
	"github.com/golang/protobuf/ptypes"
	"github.com/google/uuid"
	"github.com/minio/minio-go/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/types/known/emptypb"
)

// Server implementation for reporting
type Server struct {
	ObjStoreClient            objstore.ObjectStore
	ComplianceReportingClient cc_reporting.ReportingServiceClient
	CerealManager             *cereal.Manager
	ctx                       context.Context
	ObjBucket                 string
	DataStore                 *storage.DB
	EnableLargeReporting      bool
}

// New creates a new server
func New(objStoreClient *minio.Client, cerealManager *cereal.Manager, objBucket string, db *storage.DB,
	enableLargeReporting bool, complianceReportingClient cc_reporting.ReportingServiceClient) *Server {
	return &Server{
		ObjStoreClient: objstore.ReportManagerObjStore{
			ObjStoreClient: objStoreClient,
		},
		ComplianceReportingClient: complianceReportingClient,
		CerealManager:             cerealManager,
		ctx:                       context.Background(),
		ObjBucket:                 objBucket,
		DataStore:                 db,
		EnableLargeReporting:      enableLargeReporting,
	}
}

func (s *Server) StoreReport(stream report_manager.ReportManagerService_StoreReportServer) error {
	if !s.EnableLargeReporting {
		return fmt.Errorf("customer not enabled for large reporting")
	}
	reportData := bytes.Buffer{}

	for {
		req, err := stream.Recv()
		if err == io.EOF {
			//reached end of file
			break
		}
		if err != nil {
			return fmt.Errorf("error received from stream: %w", err)
		}
		chunk := req.GetContent()
		_, err = reportData.Write(chunk)
		if err != nil {
			return fmt.Errorf("cannot write chunk data: %w", err)
		}
	}

	complianceReport := compliance.Report{}
	err := json.Unmarshal(reportData.Bytes(), &complianceReport)
	if err != nil {
		return fmt.Errorf("error in converting report bytes to compliance report struct: %w", err)
	}

	objName := utils.GetObjName(complianceReport.GetReportUuid())

	//TODO:: Add an expiry based on user configuration
	info, err := s.ObjStoreClient.PutObject(s.ctx, s.ObjBucket, objName, &reportData, -1, minio.PutObjectOptions{})
	if err != nil {
		return fmt.Errorf("error in storing the report %s in object store: %w", complianceReport.GetReportUuid(), err)
	}

	logrus.Infof("report with uuid %s of size %d stroed in object store with key:%s", complianceReport.GetReportUuid(), info.Size, info.Key)

	return stream.SendAndClose(&emptypb.Empty{})
}

func (s *Server) PrepareCustomReport(ctx context.Context, req *report_manager.CustomReportRequest) (
	*report_manager.CustomReportResponse, error) {

	if !s.EnableLargeReporting {
		return nil, fmt.Errorf("customer not enabled for large reporting")
	}

	id := uuid.New()
	err := s.CerealManager.EnqueueWorkflow(s.ctx, worker.ReportWorkflowName,
		fmt.Sprintf("%s-%s", "report-workflow", id.String()),
		worker.ReportWorkflowParameters{
			JobID:            id.String(),
			RequestorID:      req.RequestorId,
			Retries:          2,
			RequestToProcess: req,
		})

	if err != nil {
		return &report_manager.CustomReportResponse{},
			fmt.Errorf("error in enqueuing the  workflow for request id %s: %w", id.String(), err)
	}

	return &report_manager.CustomReportResponse{
		AcknowledgementId: id.String(),
	}, nil
}

func (s *Server) GetAllRequestsStatus(ctx context.Context, req *report_manager.AllStatusRequest) (*report_manager.AllStatusResponse, error) {
	if !s.EnableLargeReporting {
		return nil, fmt.Errorf("customer not enabled for large reporting")
	}
	if req.RequestorId == "" {
		return &report_manager.AllStatusResponse{}, fmt.Errorf("empty requestore information")
	}
	resp := &report_manager.AllStatusResponse{}
	dbResp, err := s.DataStore.GetAllStatus(req.RequestorId, time.Now().Add(-24*time.Hour))
	if err != nil {
		return nil, err
	}

	for _, reportStatus := range dbResp {
		createdAt, err := ptypes.TimestampProto(reportStatus.StartTime)
		if err != nil {
			return nil, errors.Wrapf(err, "error in converting the created at with value %s to timestamppb.Timestamp", reportStatus.StartTime)
		}
		endedAt, err := ptypes.TimestampProto(reportStatus.EndTime)
		if err != nil {
			return nil, errors.Wrapf(err, "error in converting the ended at with value %s to timestamppb.Timestamp", reportStatus.EndTime)
		}

		resp.Data = append(resp.Data, &report_manager.StatusResponse{
			AcknowledgementId: reportStatus.ID,
			Status:            reportStatus.Status,
			ReportSize:        reportStatus.ReportSize.Int64,
			ErrMessage:        reportStatus.Message.String,
			CreatedAt:         createdAt,
			EndedAt:           endedAt,
			Duration:          utils.ComputeDuration(reportStatus.StartTime, reportStatus.EndTime),
			ReportType:        reportStatus.ReportType.String,
		})
	}
	return resp, nil
}

func (s *Server) GetPresignedURL(ctx context.Context, req *report_manager.GetPresignedURLRequest) (
	*report_manager.GetPresignedURLResponse, error) {
	if !s.EnableLargeReporting {
		return nil, fmt.Errorf("customer not enabled for large reporting")
	}
	if req.Id == "" || req.RequestorId == "" {
		return &report_manager.GetPresignedURLResponse{}, fmt.Errorf("id and requestor should not be empty")
	}
	return s.DataStore.GetPreSignedURL(req.Id, req.RequestorId)
}
