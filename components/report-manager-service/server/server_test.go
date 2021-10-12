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

	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	pb "github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/server"
	"github.com/minio/minio-go/v7"
	"github.com/stretchr/testify/assert"
)

type mockObjStore struct {
	T          *testing.T
	ForFailure bool
}

func (m mockObjStore) PutObject(ctx context.Context, bucketName, objectName string, reader io.Reader, objectSize int64,
	opts minio.PutObjectOptions) (info minio.UploadInfo, err error) {

	assert.Equal(m.T, "default", bucketName)
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

func dialer(t *testing.T, isForFailure bool) func(context.Context, string) (net.Conn, error) {
	listener := bufconn.Listen(1024 * 1024)

	s := grpc.NewServer(grpc.MaxRecvMsgSize(1024 * 2))

	pb.RegisterReportManagerServiceServer(s, &server.Server{
		ObjStoreClient: mockObjStore{
			T:          t,
			ForFailure: isForFailure,
		},
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
	conn, err := grpc.DialContext(ctx, "", grpc.WithInsecure(), grpc.WithContextDialer(dialer(t, false)))
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
	conn, err := grpc.DialContext(ctx, "", grpc.WithInsecure(), grpc.WithContextDialer(dialer(t, true)))
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
