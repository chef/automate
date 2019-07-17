package handler

import (
	"context"

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

func (a *Applications) GetDisconnectedServices(
	ctx context.Context,
	request *applications.DisconnectedServicesReq) (*applications.ServicesRes, error) {

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return a.client.GetDisconnectedServices(ctx, request)
}

// GetVersion fetches the version of team service
func (a *Applications) GetVersion(ctx context.Context,
	e *version.VersionInfoRequest) (*version.VersionInfo, error) {
	return a.client.GetVersion(ctx, e)
}
