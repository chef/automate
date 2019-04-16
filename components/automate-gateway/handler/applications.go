package handler

import (
	"context"

	"github.com/chef/automate/api/external/applications"
	version "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	"github.com/golang/protobuf/proto"
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
	in *applications.ServiceGroupsHealthCountsReq) (*applications.HealthCounts, error) {

	inDomain := &applications.ServiceGroupsHealthCountsReq{}
	out := &applications.HealthCounts{}
	f := func() (proto.Message, error) {
		return a.client.GetServiceGroupsHealthCounts(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// GetServiceGroups returns a list of service groups
func (a *Applications) GetServiceGroups(
	ctx context.Context,
	in *applications.ServiceGroupsReq) (*applications.ServiceGroups, error) {

	inDomain := &applications.ServiceGroupsReq{}
	out := &applications.ServiceGroups{}
	f := func() (proto.Message, error) {
		return a.client.GetServiceGroups(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// GetServices returns a list of services
func (a *Applications) GetServices(
	ctx context.Context,
	in *applications.ServicesReq) (*applications.ServicesRes, error) {

	inDomain := &applications.ServicesReq{}
	out := &applications.ServicesRes{}
	f := func() (proto.Message, error) {
		return a.client.GetServices(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// GetServicesBySG returns a list of services within a service-group
func (a *Applications) GetServicesBySG(
	ctx context.Context,
	in *applications.ServicesBySGReq) (*applications.ServicesBySGRes, error) {

	inDomain := &applications.ServicesBySGReq{}
	out := &applications.ServicesBySGRes{}
	f := func() (proto.Message, error) {
		return a.client.GetServicesBySG(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// GetVersion fetches the version of team service
func (a *Applications) GetVersion(ctx context.Context,
	e *version.VersionInfoRequest) (*version.VersionInfo, error) {
	return a.client.GetVersion(ctx, e)
}
