package server

import (
	"context"
	"fmt"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/applications"
	ver_api "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/ingester"
	"github.com/chef/automate/components/applications-service/pkg/params"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/timef"
	"github.com/chef/automate/lib/version"

	"github.com/golang/protobuf/ptypes"
	timestamp "github.com/golang/protobuf/ptypes/timestamp"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// ApplicationsServer is the interface to this component.
type ApplicationsServer struct {
	health         *health.Service
	storageClient  storage.Client
	ingesterClient ingester.Client
}

// New creates a new ApplicationsServer instance.
func New(sc storage.Client, ic ingester.Client) *ApplicationsServer {
	return &ApplicationsServer{
		health:         health.NewService(),
		storageClient:  sc,
		ingesterClient: ic,
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

	filters, err := params.FormatFilters(request.GetFilter())
	if err != nil {
		return new(applications.ServiceGroups), status.Error(codes.InvalidArgument, err.Error())
	}
	sortField, sortAsc, err := params.GetSortParamsForServiceGroups(request.GetSorting())
	if err != nil {
		return new(applications.ServiceGroups), status.Error(codes.InvalidArgument, err.Error())
	}
	page, pageSize := params.GetPageParams(request.GetPagination())

	serviceGroupsList, err := a.storageClient.GetServiceGroups(sortField, sortAsc, page, pageSize, filters)
	if err != nil {
		log.WithError(err).Error("Error retrieving service groups")
		return nil, status.Error(codes.Internal, err.Error())
	}

	if len(serviceGroupsList) == 0 {
		return new(applications.ServiceGroups), nil
	}

	serviceGroupProtos := make([]*applications.ServiceGroup, len(serviceGroupsList))
	for i, sg := range serviceGroupsList {
		serviceGroupProtos[i] = &applications.ServiceGroup{
			Id:               fmt.Sprint(sg.ID),
			Name:             sg.Name,
			Package:          sg.Package,
			Release:          sg.Release,
			Status:           convertHealthStatusToProto(sg.HealthStatus),
			HealthPercentage: sg.HealthPercentage,
			Application:      sg.Application,
			Environment:      sg.Environment,
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
	case storage.None:
		return applications.HealthStatus_NONE
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
		log.WithError(err).Error("Error retrieving service groups health counts")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &applications.HealthCounts{
		Total:    svcsHealthCounts.Total,
		Ok:       svcsHealthCounts.Ok,
		Warning:  svcsHealthCounts.Warning,
		Critical: svcsHealthCounts.Critical,
		Unknown:  svcsHealthCounts.Unknown,
	}, nil
}

// GetServicesBySG returns a list of services within a service-group
func (app *ApplicationsServer) GetServicesBySG(
	c context.Context, request *applications.ServicesBySGReq,
) (*applications.ServicesBySGRes, error) {

	sortField, sortAsc, err := params.GetSortParamsForServices(request.GetSorting())
	if err != nil {
		return new(applications.ServicesBySGRes), status.Error(codes.InvalidArgument, err.Error())
	}

	// In the database we will never have ID=0 and zero is our default value in our
	// protobuf definition, so if the service group id is zero it means that the user
	// didn't specified and this parameter is required, so we will error
	if request.GetServiceGroupId() == 0 {
		return new(applications.ServicesBySGRes),
			status.Error(codes.InvalidArgument, "Missing service_group_id parameter. [value > 0]")
	}

	var (
		page, pageSize = params.GetPageParams(request.GetPagination())
		sgStringID     = fmt.Sprint(request.GetServiceGroupId())
		filters        = map[string][]string{
			"service_group_id": {sgStringID},
		}
	)

	// Get the HealthCounts for every single service that belongs to the provided service-group id
	// NOTE: @afiune We do not add the 'health' filter here that will alter the result of the services
	// HealthCounts and as we mentioned earlier, we want all services from the provided service-group id
	svcsHealthCounts, err := app.storageClient.GetServicesHealthCounts(filters)
	if err != nil {
		log.WithError(err).Error("Error retrieving services health counts by service_group(s)")
		return nil, status.Error(codes.Internal, err.Error())
	}

	// Adds the health filter if any was specified and converts the string to be uppercases
	if len(request.GetHealth()) != 0 {
		filters["health"] = []string{strings.ToUpper(request.GetHealth())}
	}

	// Verify if the service group exists
	sgName, sgExist := app.storageClient.ServiceGroupExists(sgStringID)
	if !sgExist {
		return new(applications.ServicesBySGRes), status.Error(codes.NotFound, "service-group not found")
	}

	services, err := app.storageClient.GetServices(sortField, sortAsc, page, pageSize, filters)
	if err != nil {
		log.WithError(err).Error("Error retrieving services by service_group(s)")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &applications.ServicesBySGRes{
		Group:    sgName,
		Services: convertStorageServicesToApplicationsServices(services),
		ServicesHealthCounts: &applications.HealthCounts{
			Total:    svcsHealthCounts.Total,
			Ok:       svcsHealthCounts.Ok,
			Warning:  svcsHealthCounts.Warning,
			Critical: svcsHealthCounts.Critical,
			Unknown:  svcsHealthCounts.Unknown,
		},
	}, nil
}

// GetServices returns a list of services
func (app *ApplicationsServer) GetServices(
	c context.Context, request *applications.ServicesReq,
) (*applications.ServicesRes, error) {

	filters, err := params.FormatFilters(request.GetFilter())
	if err != nil {
		return new(applications.ServicesRes), status.Error(codes.InvalidArgument, err.Error())
	}

	sortField, sortAsc, err := params.GetSortParamsForServices(request.GetSorting())
	if err != nil {
		return new(applications.ServicesRes), status.Error(codes.InvalidArgument, err.Error())
	}

	page, pageSize := params.GetPageParams(request.GetPagination())

	services, err := app.storageClient.GetServices(sortField, sortAsc, page, pageSize, filters)
	if err != nil {
		log.WithError(err).Error("Error retrieving services")
		return nil, status.Error(codes.Internal, err.Error())
	}

	if len(services) == 0 {
		return new(applications.ServicesRes), nil
	}

	return &applications.ServicesRes{
		Services: convertStorageServicesToApplicationsServices(services),
	}, nil
}

func (app *ApplicationsServer) GetServicesStats(c context.Context,
	request *applications.ServicesStatsReq) (*applications.ServicesStatsRes, error) {

	servicesCount, err := app.storageClient.GetServicesCount()
	if err != nil {
		log.WithError(err).Error("Error retrieving services count")
		return nil, status.Error(codes.Internal, err.Error())
	}
	serviceGroupsCount, err := app.storageClient.GetServiceGroupsCount()
	if err != nil {
		log.WithError(err).Error("Error retrieving service groups count")
		return nil, status.Error(codes.Internal, err.Error())
	}
	supervisorsCount, err := app.storageClient.GetSupervisorsCount()
	if err != nil {
		log.WithError(err).Error("Error retrieving supervisors count")
		return nil, status.Error(codes.Internal, err.Error())
	}

	// Gather stats from ingested events
	eProcessed, eFailed, eSuccessful := app.ingesterClient.GetEventStats()

	return &applications.ServicesStatsRes{
		TotalServices:      servicesCount,
		TotalServiceGroups: serviceGroupsCount,
		TotalSupervisors:   supervisorsCount,
		IngestStats: &applications.IngestStats{
			TotalEventsProcessed:  eProcessed,
			TotalEventsFailed:     eFailed,
			TotalEventsSuccessful: eSuccessful,
		},
	}, nil
}

// Convert storage.Service array to applications.Service array
func convertStorageServicesToApplicationsServices(svcs []*storage.Service) []*applications.Service {
	services := make([]*applications.Service, len(svcs))
	for i, svc := range svcs {
		services[i] = &applications.Service{
			SupervisorId:        svc.SupMemberID,
			Release:             svc.FullReleaseString(),
			Group:               svc.Group,
			HealthCheck:         convertHealthStatusToProto(svc.Health),
			Status:              applications.ServiceStatus(applications.ServiceStatus_value[svc.Status]),
			Application:         svc.Application,
			Environment:         svc.Environment,
			Fqdn:                svc.Fqdn,
			Channel:             svc.Channel,
			UpdateStrategy:      svc.UpdateStrategy,
			Site:                svc.Site,
			PreviousHealthCheck: convertHealthStatusToProto(svc.PreviousHealth),
			CurrentHealthSince:  timef.IntervalUntilNow(svc.HealthUpdatedAt),
			HealthUpdatedAt:     convertOrCreateProtoTimestamp(svc.HealthUpdatedAt),
		}
	}
	return services
}

// convert a go native time to proto timestamp
// on any error return the current time (now)
func convertOrCreateProtoTimestamp(t time.Time) *timestamp.Timestamp {
	protoTime, err := ptypes.TimestampProto(t)
	if err != nil {
		log.WithError(err).Error("malformed time, using protobuf timestamp for the current time")
		return ptypes.TimestampNow()
	}
	return protoTime
}
