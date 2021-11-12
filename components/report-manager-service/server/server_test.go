package server_test

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net"
	"os"
	"testing"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"

	"github.com/DATA-DOG/go-sqlmock"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	pb "github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/server"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/go-gorp/gorp"
	"github.com/golang/protobuf/ptypes"
	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
)

type mockObjStore struct {
	T          *testing.T
	ForFailure bool
}

func (m mockObjStore) PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
	opts minio.PutObjectOptions) (info minio.UploadInfo, err error) {

	assert.Equal(m.T, "testBucket", bucketName)
	assert.Equal(m.T, "34567890-36d6-439e-ac70-a41504242605.json", objectName)

	if m.ForFailure {
		return minio.UploadInfo{}, fmt.Errorf("error from object store")
	}

	bytes, err := ioutil.ReadAll(reader)
	assert.NoError(m.T, err)

	complianceReport := compliance.Report{}
	err = json.Unmarshal(bytes, &complianceReport)
	assert.NoError(m.T, err)

	return minio.UploadInfo{
		Bucket: bucketName,
		Key:    objectName,
		Size:   int64(len(bytes)),
	}, nil
}

func (m mockObjStore) GetObject(ctx context.Context, bucketName, objectName string, opts minio.GetObjectOptions) (io.Reader, error) {
	return nil, nil
}

func (m mockObjStore) BucketExists(ctx context.Context, bucketName string) (bool, error) {
	return true, nil
}

func (m mockObjStore) MakeBucket(ctx context.Context, bucketName string, opts minio.MakeBucketOptions) error {
	return nil
}

func dialer(t *testing.T, isForFailure bool, db *storage.DB) func(context.Context, string) (net.Conn, error) {
	listener := bufconn.Listen(1024 * 1024)

	s := grpc.NewServer(grpc.MaxRecvMsgSize(1024 * 2))

	pb.RegisterReportManagerServiceServer(s, &server.Server{
		ObjStoreClient: mockObjStore{
			T:          t,
			ForFailure: isForFailure,
		},
		ObjBucket: "testBucket",
		DataStore: db,
	})

	go func() {
		if err := s.Serve(listener); err != nil {
			log.Fatal(err)
		}
	}()

	return func(context.Context, string) (net.Conn, error) {
		return listener.Dial()
	}
}

func getTestData(t *testing.T) *compliance.Report {
	jsonFile, err := os.Open("../test-data/testReport.json")
	assert.NoError(t, err)
	defer jsonFile.Close()

	jsonBytes, err := ioutil.ReadAll(jsonFile)
	assert.NoError(t, err)
	complianceReport := compliance.Report{}
	err = json.Unmarshal(jsonBytes, &complianceReport)
	assert.NoError(t, err)

	assert.Equal(t, "3.0.12", complianceReport.Version)

	return &complianceReport
}

func TestReportManagerServer_StoreReport_Success(t *testing.T) {
	ctx := context.Background()
	conn, err := grpc.DialContext(ctx, "", grpc.WithInsecure(), grpc.WithContextDialer(dialer(t, false, nil)))
	assert.NoError(t, err)

	defer conn.Close()

	client := pb.NewReportManagerServiceClient(conn)

	report := getTestData(t)
	jsonBytes, err := json.Marshal(report)
	assert.NoError(t, err)
	reader := bytes.NewReader(jsonBytes)

	stream, err := client.StoreReport(ctx)
	assert.NoError(t, err)

	//send as a chunk of 1KB
	buffer := make([]byte, 1024)
	for {
		n, err := reader.Read(buffer)
		if err == io.EOF {
			break
		}
		assert.NoError(t, err)

		req := &pb.StoreReportRequest{
			Content: buffer[:n],
		}

		err = stream.Send(req)
		assert.NoError(t, err)
	}
	_, err = stream.CloseAndRecv()
	assert.NoError(t, err)
}

func TestReportManagerServer_StoreReport_Fail(t *testing.T) {
	ctx := context.Background()
	conn, err := grpc.DialContext(ctx, "", grpc.WithInsecure(), grpc.WithContextDialer(dialer(t, true, nil)))
	assert.NoError(t, err)

	defer conn.Close()

	client := pb.NewReportManagerServiceClient(conn)

	//Test data
	testData1 := []byte("test string")
	report := getTestData(t)
	testData2, err := json.Marshal(report)
	assert.NoError(t, err)

	tests := []struct {
		name         string
		testData     []byte
		sendLargeMsg bool
		errMsgs      []string
	}{
		{
			"invalid data format",
			testData1,
			false,
			[]string{"error in converting report bytes to compliance report struct"},
		},
		{
			"error from object store",
			testData2,
			false,
			[]string{"error in storing the report", "error from object store"},
		},
		{
			"large chunk data",
			testData2,
			true,
			[]string{"received message larger than max"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			reader := bytes.NewReader(tc.testData)

			stream, err := client.StoreReport(ctx)
			assert.NoError(t, err)

			//send as a chunk of 1KB
			buffer := make([]byte, 1024)
			if tc.sendLargeMsg {
				buffer = make([]byte, 1024*2)
			}
			for {
				n, err := reader.Read(buffer)
				if err == io.EOF {
					break
				}
				assert.NoError(t, err)

				req := &pb.StoreReportRequest{
					Content: buffer[:n],
				}

				err = stream.Send(req)
				assert.NoError(t, err)
			}
			_, err = stream.CloseAndRecv()
			assert.Error(t, err)
			for _, errMsg := range tc.errMsgs {
				assert.Contains(t, err.Error(), errMsg)
			}
		})
	}
}

func TestReportManagerServer_GetAllRequestsStatus(t *testing.T) {
	dbConn, mock, err := sqlmock.New(sqlmock.QueryMatcherOption(sqlmock.QueryMatcherEqual))
	assert.NoError(t, err)
	defer dbConn.Close()

	db := &storage.DB{
		DbMap: &gorp.DbMap{Db: dbConn, Dialect: gorp.PostgresDialect{}},
	}

	ctx := context.Background()
	conn, err := grpc.DialContext(ctx, "", grpc.WithInsecure(), grpc.WithContextDialer(dialer(t, false, db)))
	assert.NoError(t, err)

	defer conn.Close()

	columns := []string{"id", "status", "message", "custom_report_size", "created_at", "updated_at"}
	endedAt := time.Now()
	createdAt := endedAt.Add(-10 * time.Minute)
	endedAtProto, err := ptypes.TimestampProto(endedAt)
	assert.NoError(t, err)
	createdAtProto, err := ptypes.TimestampProto(createdAt)
	assert.NoError(t, err)

	query := `SELECT id, status, message, custom_report_size, created_at, updated_at FROM custom_report_requests WHERE requestor = $1 AND created_at >= $2 ORDER BY created_at DESC;`
	mock.ExpectQuery(query).WithArgs("test_requestor_id", sqlmock.AnyArg()).
		WillReturnRows(sqlmock.NewRows(columns).AddRow("1", "success", "", 1024*1024, createdAt, endedAt).
			AddRow("2", "failed", "error in running task", 0, createdAt, endedAt).AddRow("3", "running", "", 0, createdAt, createdAt))

	client := pb.NewReportManagerServiceClient(conn)

	t.Run("Success", func(t *testing.T) {
		resp, err := client.GetAllRequestsStatus(ctx, &pb.AllStatusRequest{
			RequestorId: "test_requestor_id",
		})
		assert.NoError(t, err)
		assert.Equal(t, 3, len(resp.Data))

		assert.Equal(t, "1", resp.Data[0].AcknowledgementId)
		assert.Equal(t, "success", resp.Data[0].Status)
		assert.Equal(t, int64(1048576), resp.Data[0].ReportSize)
		assert.Equal(t, createdAtProto, resp.Data[0].CreatedAt)
		assert.Equal(t, endedAtProto, resp.Data[0].EndedAt)
		assert.Equal(t, "failed", resp.Data[1].Status)
		assert.Equal(t, "error in running task", resp.Data[1].ErrMessage)
		assert.Equal(t, "running", resp.Data[2].Status)
	})

	t.Run("Fail", func(t *testing.T) {
		_, err := client.GetAllRequestsStatus(ctx, &pb.AllStatusRequest{
			RequestorId: "",
		})
		assert.Error(t, err)
		assert.EqualError(t, err, "rpc error: code = Unknown desc = empty requestore information")
	})
}
