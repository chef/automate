package server

import (
	"context"
	"fmt"

	uuid "github.com/gofrs/uuid"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/components/data-lifecycle-service/storage"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
)

// Server is the Data Lifecycle Server. It is responsible for calling data lifecycle manageable methods
// such as Purge on other services (and itself).
type Server struct {
	version             string
	serviceConfigStore  storage.ServiceConfigStore
	dataLifecycleConfig *DataLifeCycleConfig
	connFactory         *secureconn.Factory
}

// NewServer returns a new Data Lifecycle Server
func NewServer(version string,
	serviceConfigStore storage.ServiceConfigStore,
	dataLifecycleConfig *DataLifeCycleConfig,
	connFactory *secureconn.Factory) *Server {
	return &Server{
		version:             version,
		serviceConfigStore:  serviceConfigStore,
		dataLifecycleConfig: dataLifecycleConfig,
		connFactory:         connFactory,
	}
}

// Version returns the Version of the Data Lifecycle Service
func (server *Server) Version(ctx context.Context, request *api.VersionRequest) (*api.VersionResponse, error) {
	response := api.VersionResponse{
		Version: version.Version,
	}
	return &response, nil
}

// TriggerPurge triggers a purge to happen on configured services
func (server *Server) TriggerPurge(ctx context.Context,
	request *api.TriggerPurgeRequest) (*api.TriggerPurgeResponse, error) {

	id := request.GetId()
	if id == "" {
		id = uuid.Must(uuid.NewV4()).String()
	}

	var services []string

	if request.GetServiceName() == "" {
		services = server.serviceConfigStore.ListServices()
	} else {
		services = []string{request.GetServiceName()}
	}

	responses := make(map[string]*api.PurgeResponse)
	for _, serviceName := range services {
		resp := server.doPurge(ctx, id, serviceName)
		responses[serviceName] = resp
	}

	return &api.TriggerPurgeResponse{Responses: responses}, nil
}

func (server *Server) doPurge(ctx context.Context, id string, serviceName string) *api.PurgeResponse {
	serviceConfig, err := server.serviceConfigStore.GetServiceConfig(serviceName)
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"purge_id": id,
			"service":  serviceName,
		}).Error("Service configuration not found")

		return &api.PurgeResponse{
			Id: id,
			ComponentStatus: map[string]*api.PurgeStatus{
				"_all": {
					Status: api.PurgeStatus_FAILED,
					Msg:    fmt.Sprintf("Could not find configuration for service (%s)", serviceName),
				},
			},
		}
	}

	//TODO: Move this out to a connection factory thing like automate-gateway
	var conn *grpc.ClientConn
	if serviceConfig.Secure {
		conn, err = server.connFactory.Dial(serviceName, serviceConfig.Address)
	} else {
		conn, err = grpc.Dial(
			serviceConfig.Address,
			grpc.WithInsecure(),
			tracing.GlobalClientInterceptor(),
		)
	}
	defer conn.Close() // nolint: errcheck
	client := api.NewDataLifecycleManageableClient(conn)

	logrus.WithFields(logrus.Fields{
		"purge_id": id,
		"service":  serviceName,
		"secure":   serviceConfig.Secure,
		"target":   serviceConfig.Address,
	}).Info("Triggering purge")

	resp, err := client.Purge(ctx, &api.PurgeRequest{
		Id: id,
	})

	if err != nil {
		logrus.WithFields(logrus.Fields{
			"purge_id": id,
			"service":  serviceName,
			"error":    err,
		}).Error("Purge Errored")

		return &api.PurgeResponse{
			Id: id,
			ComponentStatus: map[string]*api.PurgeStatus{
				"_all": {
					Status: api.PurgeStatus_FAILED,
					Msg:    err.Error(),
				},
			},
		}
	}

	for component, status := range resp.GetComponentStatus() {
		if status.Status == api.PurgeStatus_FAILED {
			logrus.WithFields(logrus.Fields{
				"purge_id":  id,
				"service":   serviceName,
				"component": component,
				"msg":       status.GetMsg(),
			}).Error("Purge component failure")
		} else {
			logrus.WithFields(logrus.Fields{
				"purge_id":  id,
				"service":   serviceName,
				"component": component,
				"msg":       status.GetMsg(),
			}).Info("Purge component success")
		}
	}

	logrus.WithFields(logrus.Fields{
		"purge_id": id,
		"service":  serviceName,
	}).Info("Purge complete")

	return resp
}

// Purge returns success as there is no stored data for this service yet.
func (server *Server) Purge(ctx context.Context, request *api.PurgeRequest) (*api.PurgeResponse, error) {
	if server.dataLifecycleConfig == nil {
		logrus.WithFields(logrus.Fields{
			"purge_id": request.GetId(),
			"service":  "data-lifecycle-service",
		}).Infof("Purge disabled")
	} else {
		logrus.WithFields(logrus.Fields{
			"purge_id":        request.GetId(),
			"service":         "data-lifecycle-service",
			"older_than_days": server.dataLifecycleConfig.PurgeOlderThanDays,
		}).Info("Purging data")
	}

	return &api.PurgeResponse{
		Id: request.GetId(),
		ComponentStatus: map[string]*api.PurgeStatus{
			"history": {
				Status: api.PurgeStatus_SUCCESS,
				Msg:    "No data to purge",
			},
		},
	}, nil
}
