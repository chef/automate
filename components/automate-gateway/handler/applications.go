package handler

import (
	"context"
	"errors"
	"io"

	"github.com/chef/automate/api/external/applications"
	version "github.com/chef/automate/api/external/common/version"

	log "github.com/sirupsen/logrus"
)

// Applications - the applications service data structure
type Applications struct {
	client applications.ApplicationsServiceClient
}

// NewApplicationsHandler - create a new applications service handler
func NewApplicationsHandler(applicationsClient applications.ApplicationsServiceClient) *Applications {
	return &Applications{
		client: applicationsClient,
	}
}

// GetServiceGroupsHealthCounts returns the health counts from all service groups
func (a *Applications) GetServiceGroupsHealthCounts(
	ctx context.Context,
	request *applications.ServiceGroupsHealthCountsReq) (*applications.HealthCounts, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetServiceGroupsHealthCounts(ctx, request)
}

// GetServiceGroups returns a list of service groups
func (a *Applications) GetServiceGroups(
	ctx context.Context,
	request *applications.ServiceGroupsReq) (*applications.ServiceGroups, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetServiceGroups(ctx, request)
}

// GetServices returns a list of services
func (a *Applications) GetServices(
	ctx context.Context,
	request *applications.ServicesReq) (*applications.ServicesRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetServices(ctx, request)
}

func (a *Applications) FindServices(
	request *applications.ServicesReq,
	streamOut applications.ApplicationsService_FindServicesServer) error {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	// This contains auth metadata we want to pass on
	ctx := streamOut.Context()

	streamIn, err := a.client.FindServices(ctx, request)
	if err != nil {
		return err
	}
	for {
		svc, err := streamIn.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		err = streamOut.Send(svc)
		if err != nil {
			return err
		}
	}

	return nil
}

func (a *Applications) GetServicesDistinctValues(ctx context.Context,
	request *applications.ServicesDistinctValuesReq) (*applications.ServicesDistinctValuesRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetServicesDistinctValues(ctx, request)
}

// GetServicesBySG returns a list of services within a service-group
func (a *Applications) GetServicesBySG(
	ctx context.Context,
	request *applications.ServicesBySGReq) (*applications.ServicesBySGRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetServicesBySG(ctx, request)
}

func (a *Applications) GetServicesStats(
	ctx context.Context,
	request *applications.ServicesStatsReq) (*applications.ServicesStatsRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetServicesStats(ctx, request)
}

func (a *Applications) GetDisconnectedServicesConfig(
	ctx context.Context,
	request *applications.GetDisconnectedServicesConfigReq) (*applications.PeriodicMandatoryJobConfig, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetDisconnectedServicesConfig(ctx, request)
}

func (a *Applications) UpdateDisconnectedServicesConfig(
	ctx context.Context,
	request *applications.PeriodicMandatoryJobConfig) (*applications.UpdateDisconnectedServicesConfigRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.UpdateDisconnectedServicesConfig(ctx, request)
}

func (a *Applications) RunDisconnectedServicesJob(
	ctx context.Context,
	request *applications.RunDisconnectedServicesJobReq) (*applications.RunDisconnectedServicesJobResponse, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.RunDisconnectedServicesJob(ctx, request)
}

func (a *Applications) GetDeleteDisconnectedServicesConfig(
	ctx context.Context,
	request *applications.GetDeleteDisconnectedServicesConfigReq) (*applications.PeriodicJobConfig, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetDeleteDisconnectedServicesConfig(ctx, request)
}

func (a *Applications) UpdateDeleteDisconnectedServicesConfig(
	ctx context.Context,
	request *applications.PeriodicJobConfig) (*applications.UpdateDeleteDisconnectedServicesConfigRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.UpdateDeleteDisconnectedServicesConfig(ctx, request)
}

func (a *Applications) RunDeleteDisconnectedServicesJob(
	ctx context.Context,
	request *applications.RunDeleteDisconnectedServicesJobReq) (*applications.RunDeleteDisconnectedServicesJobResponse, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.RunDeleteDisconnectedServicesJob(ctx, request)
}

func (a *Applications) GetDisconnectedServices(
	ctx context.Context,
	request *applications.DisconnectedServicesReq) (*applications.ServicesRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetDisconnectedServices(ctx, request)
}

func (a *Applications) DeleteDisconnectedServices(
	ctx context.Context,
	request *applications.DisconnectedServicesReq) (*applications.ServicesRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.DeleteDisconnectedServices(ctx, request)
}

func (a *Applications) DeleteServicesByID(
	ctx context.Context,
	request *applications.DeleteServicesByIDReq) (*applications.ServicesRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.DeleteServicesByID(ctx, request)
}

// GetVersion fetches the version of team service
func (a *Applications) GetVersion(ctx context.Context,
	e *version.VersionInfoRequest) (*version.VersionInfo, error) {
	return a.client.GetVersion(ctx, e)
}

//UpdateTelemetryReported Updates the application services telemetry reported date in postgres
func (s *Applications) UpdateTelemetryReported(ctx context.Context, req *applications.UpdateTelemetryReportedRequest) (*applications.UpdateTelemetryReportedResponse, error) {

	if req.LastTelemetryReportedAt == "" {
		return &applications.UpdateTelemetryReportedResponse{}, errors.New("LastTelemetryReported timestamp is required")
	}

	aapServiceRequest := &applications.UpdateTelemetryReportedRequest{
		LastTelemetryReportedAt: req.LastTelemetryReportedAt,
	}

	_, err := s.client.UpdateTelemetryReported(ctx, aapServiceRequest)
	if err != nil {
		return &applications.UpdateTelemetryReportedResponse{}, err
	}

	return &applications.UpdateTelemetryReportedResponse{}, nil
}

// GetServicesUsageCount fetches the count of service
func (a *Applications) GetServicesUsageCount(ctx context.Context,
	e *applications.GetServicesUsageCountRequest) (*applications.GetServicesUsageCountResponse, error) {
	return a.client.GetServicesUsageCount(ctx, e)
}
