package server

import (
	"context"
	"fmt"

	"github.com/chef/automate/api/external/applications"
	ver_api "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/params"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/version"
	log "github.com/sirupsen/logrus"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// ApplicationsServer is the interface to this component.
type ApplicationsServer struct {
	health        *health.Service
	storageClient storage.Client
}

// New creates a new ApplicationsServer instance.
func New(sc storage.Client) *ApplicationsServer {
	return &ApplicationsServer{
		health:        health.NewService(),
		storageClient: sc,
	}
}

// Health returns the servers embedded health check service
func (a *ApplicationsServer) Health() *health.Service {
	return a.health
}

// GetVersion returns the version of service
func (a *ApplicationsServer) GetVersion(
	context.Context,
	*ver_api.VersionInfoRequest) (*ver_api.VersionInfo, error) {
	return &ver_api.VersionInfo{
		Name:    config.ServiceName,
		Version: version.Version,
		Sha:     version.GitSHA,
		Built:   version.BuildTime,
	}, nil
}

func (a *ApplicationsServer) GetServiceGroups(c context.Context,
	request *applications.ServiceGroupsReq) (*applications.ServiceGroups, error) {

	if !params.IsValidSortField(request.GetSorting()) {
		return new(applications.ServiceGroups),
			status.Error(codes.InvalidArgument,
				fmt.Sprintf("Invalid sort field '%s'.", request.GetSorting().GetField()))
	}

	var (
		sortField, sortAsc = params.GetSortParams(request.GetSorting())
		page, pageSize     = params.GetPageParams(request.GetPagination())
	)

	filters, err := params.FormatFilters(request.GetFilter())
	if err != nil {
		return new(applications.ServiceGroups), status.Error(codes.InvalidArgument, err.Error())
	}
	serviceGroupsList, err := a.storageClient.GetServiceGroups(sortField, sortAsc, page, pageSize, filters)
	if err != nil {
		log.WithError(err).Error("Error retrieving service groups")
		return new(applications.ServiceGroups), err
	}

	if len(serviceGroupsList) == 0 {
		return new(applications.ServiceGroups), nil
	}

	serviceGroupProtos := make([]*applications.ServiceGroup, len(serviceGroupsList))
	for i, sg := range serviceGroupsList {
		serviceGroupProtos[i] = &applications.ServiceGroup{
			Id:               fmt.Sprint(sg.ID),
			Name:             sg.Name,
			Release:          sg.Release,
			Status:           convertHealthStatusToProto(sg.HealthStatus),
			HealthPercentage: sg.HealthPercentage,
			ServicesHealthCounts: &applications.HealthCounts{
				Total:    sg.ServicesHealthCounts.Total,
				Ok:       sg.ServicesHealthCounts.Ok,
				Warning:  sg.ServicesHealthCounts.Warning,
				Critical: sg.ServicesHealthCounts.Critical,
				Unknown:  sg.ServicesHealthCounts.Unknown,
			},
		}
	}

	return &applications.ServiceGroups{ServiceGroups: serviceGroupProtos}, err
}

func convertHealthStatusToProto(healthStatus string) applications.HealthStatus {
	switch healthStatus {
	case storage.Critical:
		return applications.HealthStatus_CRITICAL
	case storage.Warning:
		return applications.HealthStatus_WARNING
	case storage.Ok:
		return applications.HealthStatus_OK
	default:
		// default to unknown, which will catch unexpected values
		return applications.HealthStatus_UNKNOWN
	}
}

// GetServiceGroupsHealthCounts returns the health counts from all service groups
func (app *ApplicationsServer) GetServiceGroupsHealthCounts(
	c context.Context, request *applications.ServiceGroupsHealthCountsReq,
) (*applications.HealthCounts, error) {

	svcsHealthCounts, err := app.storageClient.GetServiceGroupsHealthCounts()
	if err != nil {
		// Transform into gRPC error
		return nil, err
	}

	return &applications.HealthCounts{
		Total:    svcsHealthCounts.Total,
		Ok:       svcsHealthCounts.Ok,
		Warning:  svcsHealthCounts.Warning,
		Critical: svcsHealthCounts.Critical,
		Unknown:  svcsHealthCounts.Unknown,
	}, nil
}
