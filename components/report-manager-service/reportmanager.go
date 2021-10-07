package reportmanager

import (
	"context"
	"fmt"
	"net"

	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/config"
	"github.com/chef/automate/components/report-manager-service/server"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/sirupsen/logrus"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
)

func Serve(conf config.ReportManager, connFactory *secureconn.Factory) error {
	ctx := context.Background()

	//get object store connection
	objStoreClient, err := getObjectStoreConnection(ctx, conf)
	if err != nil {
		logrus.WithError(err).Fatal("Error in establishing a connection to object store")
		return err
	}
	logrus.Infof("connection established to object store, endPoint:%s", objStoreClient.EndpointURL())

	return serveGrpc(ctx, conf, objStoreClient, connFactory)
}

func getObjectStoreConnection(ctx context.Context, conf config.ReportManager) (*minio.Client, error) {

	//TODO:: Get the below details from configuration
	endpoint := "127.0.0.1:10197"
	accessKeyID := "minioadmin"
	secretAccessKey := "minioadmin"
	useSSL := false

	objStoreClient, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyID, secretAccessKey, ""),
		Secure: useSSL,
	})
	if err != nil {
		return nil, fmt.Errorf("error in creating a connection to object store server with end point %s: %w", endpoint, err)
	}

	//check if the default bucket is available or not
	isExist, err := objStoreClient.BucketExists(ctx, "default")
	if err != nil {
		return nil, fmt.Errorf("error in checking the default bucket existence in object store:%w", err)
	}
	//create a default bucket if not available
	if !isExist {
		err := objStoreClient.MakeBucket(ctx, "default", minio.MakeBucketOptions{})
		if err != nil {
			return nil, fmt.Errorf("error in creating a default bucket in object store to store the report data: %w", err)
		}
	}

	return objStoreClient, nil
}

func serveGrpc(ctx context.Context, conf config.ReportManager, objStoreClient *minio.Client, connFactory *secureconn.Factory) error {

	grpcBinding := fmt.Sprintf("%s:%d", conf.Service.Host, conf.Service.Port)
	lis, err := net.Listen("tcp", grpcBinding)
	if err != nil {
		logrus.Fatalf("failed to listen: %v", err)
	}

	s := connFactory.NewServer()

	//register health server for health status
	health.RegisterHealthServer(s, health.NewService())
	report_manager.RegisterReportManagerServiceServer(s, server.New(objStoreClient))

	logrus.Info("Starting GRPC server on " + grpcBinding)

	return s.Serve(lis)
}
