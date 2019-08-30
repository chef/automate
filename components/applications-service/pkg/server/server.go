package server

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	timestamp "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/external/applications"
	ver_api "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/ingester"
	"github.com/chef/automate/components/applications-service/pkg/params"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/timef"
	"github.com/chef/automate/lib/version"
)

// ApplicationsServer is the interface to this component.
type ApplicationsServer struct {
	health         *health.Service
	storageClient  storage.Client
	ingesterClient ingester.Client
	jobScheduler   *JobScheduler
}

// New creates a new ApplicationsServer instance.
func New(sc storage.Client, ic ingester.Client, j *JobScheduler) *ApplicationsServer {
	return &ApplicationsServer{
		health:         health.NewService(),
		storageClient:  sc,
		ingesterClient: ic,
		jobScheduler:   j,
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

func (a *ApplicationsServer) GetServiceGroups(ctx context.Context,
	request *applications.ServiceGroupsReq) (*applications.ServiceGroups, error) {

	filters, err := stringutils.FormatFilters(request.GetFilter())
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

func (a *ApplicationsServer) GetServicesDistinctValues(ctx context.Context,
	request *applications.ServicesDistinctValuesReq) (*applications.ServicesDistinctValuesRes, error) {

	values, err := a.storageClient.GetServicesDistinctValues(request.FieldName, request.QueryFragment)
	if err != nil {
		return nil, err
	}

	return &applications.ServicesDistinctValuesRes{Values: values}, nil
}

// GetServiceGroupsHealthCounts returns the health counts from all service groups
func (app *ApplicationsServer) GetServiceGroupsHealthCounts(
	ctx context.Context, request *applications.ServiceGroupsHealthCountsReq,
) (*applications.HealthCounts, error) {

	filters, err := stringutils.FormatFilters(request.GetFilter())
	if err != nil {
		return new(applications.HealthCounts), status.Error(codes.InvalidArgument, err.Error())
	}
	svcsHealthCounts, err := app.storageClient.GetServiceGroupsHealthCounts(filters)
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
	ctx context.Context, request *applications.ServicesBySGReq,
) (*applications.ServicesBySGRes, error) {

	sortField, sortAsc, err := params.GetSortParamsForServices(request.GetSorting())
	if err != nil {
		return new(applications.ServicesBySGRes), status.Error(codes.InvalidArgument, err.Error())
	}

	// In the database we will never have ID="" and "" is our default value in our
	// protobuf definition, so if the service group id is "" it means that the user
	// didn't specified and this parameter is required, so we will error
	if request.GetServiceGroupId() == "" {
		return new(applications.ServicesBySGRes),
			status.Error(codes.InvalidArgument, "Missing service_group_id parameter. [value cannot be blank]")
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
	ctx context.Context, request *applications.ServicesReq,
) (*applications.ServicesRes, error) {

	filters, err := stringutils.FormatFilters(request.GetFilter())
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

func (app *ApplicationsServer) GetServicesStats(ctx context.Context,
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
	deploymentsCount, err := app.storageClient.GetDeploymentsCount()
	if err != nil {
		log.WithError(err).Error("Error retrieving deployments count")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &applications.ServicesStatsRes{
		TotalServices:      servicesCount,
		TotalServiceGroups: serviceGroupsCount,
		TotalSupervisors:   supervisorsCount,
		TotalDeployments:   deploymentsCount,
	}, nil
}

// GetDisconnectedServices returns a list of disconnected services
func (app *ApplicationsServer) GetDisconnectedServices(ctx context.Context,
	request *applications.DisconnectedServicesReq) (*applications.ServicesRes, error) {

	thresholdSeconds := request.GetThresholdSeconds()
	if thresholdSeconds <= 0 {
		return new(applications.ServicesRes),
			status.Error(codes.InvalidArgument, "threshold must be greater than zero")
	}
	services, err := app.storageClient.GetDisconnectedServices(thresholdSeconds)
	if err != nil {
		log.WithError(err).Error("Error retrieving disconnected services")
		return new(applications.ServicesRes), status.Error(codes.Internal, err.Error())
	}

	return &applications.ServicesRes{
		Services: convertStorageServicesToApplicationsServices(services),
	}, nil
}

// DeleteDisconnectedServices deletes disconnected services and returns the list of services deleted
func (app *ApplicationsServer) DeleteDisconnectedServices(ctx context.Context,
	request *applications.DisconnectedServicesReq) (*applications.ServicesRes, error) {

	thresholdSeconds := request.GetThresholdSeconds()
	if thresholdSeconds <= 0 {
		return new(applications.ServicesRes),
			status.Error(codes.InvalidArgument, "threshold must be greater than zero")
	}

	services, err := app.storageClient.DeleteDisconnectedServices(thresholdSeconds)
	if err != nil {
		log.WithError(err).Error("Error retrieving disconnected services")
		return new(applications.ServicesRes), status.Error(codes.Internal, err.Error())
	}

	return &applications.ServicesRes{
		Services: convertStorageServicesToApplicationsServices(services),
	}, nil
}

func (app *ApplicationsServer) MarkDisconnectedServices(thresholdSeconds int32) ([]*applications.Service, error) {
	svcs, err := app.storageClient.MarkDisconnectedServices(thresholdSeconds)
	if err != nil {
		return nil, err
	}
	return convertStorageServicesToApplicationsServices(svcs), nil
}

func (app *ApplicationsServer) GetDisconnectedServicesConfig(ctx context.Context,
	req *applications.GetDisconnectedServicesConfigReq) (*applications.PeriodicJobConfig, error) {
	config, err := app.jobScheduler.GetDisconnectedServicesJobConfig(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "failed to load disconnected_services job configuration")
	}
	res := &applications.PeriodicJobConfig{
		Running:   config.Enabled,
		Threshold: config.Params.ThresholdDuration,
	}
	return res, nil
}

func (app *ApplicationsServer) UpdateDisconnectedServicesConfig(ctx context.Context,
	req *applications.PeriodicJobConfig) (*applications.UpdateDisconnectedServicesConfigRes, error) {

	if req.GetRunning() {
		err := app.jobScheduler.EnableDisconnectedServicesJob(ctx)
		if err != nil {
			return nil, errors.Wrap(err, "failed to enable disconnected_services job")
		}
	} else {
		err := app.jobScheduler.DisableDisconnectedServicesJob(ctx)
		if err != nil {
			return nil, errors.Wrap(err, "failed to disable disconnected_services job")
		}
	}

	_, err := time.ParseDuration(req.GetThreshold())
	if err != nil {
		return nil, errors.Wrapf(err, "unable to parse disconnected_services threshold %q", req.GetThreshold())
	}
	err = app.jobScheduler.UpdateDisconnectedServicesJobParams(ctx, &DisconnectedServicesParamsV0{ThresholdDuration: req.GetThreshold()})
	if err != nil {
		return nil, errors.Wrapf(err, "unable to update disconnected services parameters to %q", req.GetThreshold())
	}

	return &applications.UpdateDisconnectedServicesConfigRes{}, nil
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
			Disconnected:        svc.Disconnected,
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
