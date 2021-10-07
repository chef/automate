package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/minio/minio-go/v7"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/types/known/emptypb"
)

type ObjectStore interface {
	PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
		opts minio.PutObjectOptions) (info minio.UploadInfo, err error)
}

type ReportManagerObjStore struct {
	objStoreClient *minio.Client
}

func (rmc ReportManagerObjStore) PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
	opts minio.PutObjectOptions) (info minio.UploadInfo, err error) {
	return rmc.objStoreClient.PutObject(ctx, bucketName, objectName, reader, objectSize, opts)
}

// Server implementation for reporting
type Server struct {
	ObjStoreClient ObjectStore
	ctx            context.Context
}

// New creates a new server
func New(objStoreClient *minio.Client) *Server {
	return &Server{
		ObjStoreClient: ReportManagerObjStore{
			objStoreClient: objStoreClient,
		},
		ctx: context.Background(),
	}
}

func (s *Server) StoreReport(stream report_manager.ReportManagerService_StoreReportServer) error {
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

	objName := fmt.Sprintf("%s.json", complianceReport.GetReportUuid())

	//TODO:: Add an expiry based on user configuration
	info, err := s.ObjStoreClient.PutObject(s.ctx, "default", objName, &reportData, -1, minio.PutObjectOptions{})
	if err != nil {
		return fmt.Errorf("error in storing the report %s in object store: %w", complianceReport.GetReportUuid(), err)
	}

	logrus.Infof("report with uuid %s of size %d stroed in object store with key:%s", complianceReport.GetReportUuid(), info.Size, info.Key)

	return stream.SendAndClose(&emptypb.Empty{})
}
