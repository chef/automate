package reportmanager

import (
	"context"
	"fmt"
	"net"

	cc_reporting "github.com/chef/automate/api/interservice/compliance/reporting"

	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/config"
	"github.com/chef/automate/components/report-manager-service/server"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/chef/automate/components/report-manager-service/worker"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/reflection"

	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"

	"github.com/chef/automate/lib/cereal/grpc"
)

func Serve(conf config.ReportManager, connFactory *secureconn.Factory) error {
	ctx := context.Background()

	var objStoreClient *minio.Client = nil
	var err error
	if conf.Service.EnableLargeReporting {
		if conf.Minio.EndPoint == "" || conf.Minio.RootUser == "" || conf.Minio.RootPassword == "" {
			return fmt.Errorf("minio endpoint, root_user and root_password should not be empty")
		}
		//get object store connection
		objStoreClient, err = getObjectStoreConnection(ctx, conf)
		if err != nil {
			logrus.WithError(err).Fatal("Error in establishing a connection to object store")
			return err
		}
		logrus.Infof("connection established to object store, endPoint:%s", objStoreClient.EndpointURL())
	}

	//get cereal manager
	cerealManager, err := getCerealManager(conf, connFactory)
	if err != nil {
		logrus.WithError(err).Fatal("error in establishing a connection to cereal manager")
		return err
	}

	//establish db connection and perform migrations
	db, err := storage.ConnectAndMigrate(&conf.Storage)
	if err != nil {
		logrus.WithError(err).Fatal("error in establishing a connection and running migrations to db")
		return err
	}

	return serveGrpc(ctx, conf, objStoreClient, connFactory, cerealManager, db)
}

func getCerealManager(conf config.ReportManager, connFactory *secureconn.Factory) (*cereal.Manager, error) {
	conn, err := connFactory.Dial("cereal-service", conf.CerealConfig.Target)
	logrus.Info("Cereal Connection:", conf.CerealConfig.Target)
	if err != nil {
		return nil, err
	}

	backend := grpc.NewGrpcBackendFromConn("report-manager-service", conn)
	return cereal.NewManager(backend)
}

func getObjectStoreConnection(ctx context.Context, conf config.ReportManager) (*minio.Client, error) {
	endpoint := conf.Minio.EndPoint
	accessKeyID := conf.Minio.RootUser
	secretAccessKey := conf.Minio.RootPassword
	useSSL := false

	objStoreClient, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyID, secretAccessKey, ""),
		Secure: useSSL,
	})
	if err != nil {
		return nil, fmt.Errorf("error in creating a connection to object store server with end point %s: %w", endpoint, err)
	}

	//check if the default bucket is available or not
	isExist, err := objStoreClient.BucketExists(ctx, conf.ObjStore.BucketName)
	if err != nil {
		return nil, fmt.Errorf("error in checking the default bucket existence in object store:%w", err)
	}
	//create a default bucket if not available
	if !isExist {
		err := objStoreClient.MakeBucket(ctx, conf.ObjStore.BucketName, minio.MakeBucketOptions{})
		if err != nil {
			return nil, fmt.Errorf("error in creating a default bucket in object store to store the report data: %w", err)
		}
	}

	return objStoreClient, nil
}

func serveGrpc(ctx context.Context, conf config.ReportManager, objStoreClient *minio.Client,
	connFactory *secureconn.Factory, cerealManager *cereal.Manager, db *storage.DB) error {

	grpcBinding := fmt.Sprintf("%s:%d", conf.Service.Host, conf.Service.Port)
	lis, err := net.Listen("tcp", grpcBinding)
	if err != nil {
		logrus.Fatalf("failed to listen: %v", err)
	}

	s := connFactory.NewServer()
	complianceReportingClient := getComplianceReportingClient(connFactory, conf.ComplianceConfig.Target)
	//register health server for health status
	health.RegisterHealthServer(s, health.NewService())
	report_manager.RegisterReportManagerServiceServer(s,
		server.New(objStoreClient, cerealManager, conf.ObjStore.BucketName, db, conf.Service.EnableLargeReporting, complianceReportingClient))

	//Initiate the cereal manager with 2 workers
	err = worker.InitCerealManager(cerealManager, 2, db, objStoreClient, conf.ObjStore.BucketName, complianceReportingClient)
	if err != nil {
		logrus.Fatalf("failed to initiate cereal manager: %v", err)
	}
	cerealManager.Start(ctx)
	logrus.Info("cereal manager started")

	reflection.Register(s)
	logrus.Info("Starting GRPC server on " + grpcBinding)

	return s.Serve(lis)
}

func getComplianceReportingClient(connFactory *secureconn.Factory, address string) cc_reporting.ReportingServiceClient {
	if address == "" || address == ":0" {
		logrus.Fatal("compliance reporting cannot be empty or Dial will get stuck")
	}

	logrus.Debugf("Connecting to compliance reporting %q", address)
	conn, err := connFactory.Dial("compliance-service", address)
	if err != nil {
		logrus.Fatalf("compliance-service, error grpc dialing to manager %s", err.Error())
	}
	return cc_reporting.NewReportingServiceClient(conn)
}
