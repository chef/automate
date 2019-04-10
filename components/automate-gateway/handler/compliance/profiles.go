package compliance

import (
	"context"

	"github.com/chef/automate/components/automate-gateway/api/compliance/profiles"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	profileService "github.com/chef/automate/components/compliance-service/api/profiles"
	"github.com/golang/protobuf/proto"
	gp "github.com/golang/protobuf/ptypes/empty"
)

type Profiles struct {
	client profileService.ProfilesServiceClient
}

func NewProfilesHandler(profilesClient profileService.ProfilesServiceClient) *Profiles {
	return &Profiles{
		client: profilesClient,
	}
}

func (a *Profiles) Create(stream profiles.ProfilesService_CreateServer) error {
	// grpc gateway is not able to handle multi-part upload; https://github.com/grpc-ecosystem/grpc-gateway/issues/410
	// so we do not auto-generate the route for profile upload; we instead custom handle with mux in gateway/services.go
	return nil
}

func (a *Profiles) Read(ctx context.Context, in *profiles.ProfileDetails) (*profiles.Profile, error) {
	inDomain := &profileService.ProfileDetails{}
	out := &profiles.Profile{}
	f := func() (proto.Message, error) {
		return a.client.Read(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Profiles) ReadFromMarket(ctx context.Context, in *profiles.ProfileDetails) (*profiles.Profile, error) {
	inDomain := &profileService.ProfileDetails{}
	out := &profiles.Profile{}
	f := func() (proto.Message, error) {
		return a.client.Read(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Profiles) ReadTar(*profiles.ProfileDetails, profiles.ProfilesService_ReadTarServer) error {
	// grpc gateway is not able to handle streaming; https://github.com/grpc-ecosystem/grpc-gateway/issues/435
	// so we do not auto-generate the route for profile download; we instead custom handle with mux in gateway/services.go
	return nil
}

func (a *Profiles) Delete(ctx context.Context, in *profiles.ProfileDetails) (*gp.Empty, error) {
	inDomain := &profileService.ProfileDetails{}
	out := &gp.Empty{}
	f := func() (proto.Message, error) {
		return a.client.Delete(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Profiles) List(ctx context.Context, in *profiles.Query) (*profiles.Profiles, error) {
	inDomain := &profileService.Query{}
	out := &profiles.Profiles{}
	f := func() (proto.Message, error) {
		return a.client.List(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}
