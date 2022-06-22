package handler

import (
	"context"
	"fmt"

	gp "github.com/golang/protobuf/ptypes/empty"

	external_user_settings "github.com/chef/automate/api/external/user_settings"
	"github.com/chef/automate/api/interservice/user_settings"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	"github.com/golang/protobuf/proto"
)

type UserSettingsServiceServer struct {
	client user_settings.UserSettingsServiceClient
}

func NewUserSettingsHandler(client user_settings.UserSettingsServiceClient) *UserSettingsServiceServer {
	return &UserSettingsServiceServer{
		client: client,
	}
}

func (s *UserSettingsServiceServer) GetUserSettings(ctx context.Context, in *external_user_settings.GetUserSettingsRequest) (*external_user_settings.GetUserSettingsResponse, error) {
	fmt.Println("IN GATEWAY GetUserSettings.. ")
	inDomain := &user_settings.GetUserSettingsRequest{}
	out := &external_user_settings.GetUserSettingsResponse{}
	f := func() (proto.Message, error) {
		return s.client.GetUserSettings(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (s *UserSettingsServiceServer) PutUserSettings(ctx context.Context, in *external_user_settings.PutUserSettingsRequest) (*external_user_settings.PutUserSettingsResponse, error) {
	inDomain := &user_settings.PutUserSettingsRequest{}
	out := &external_user_settings.PutUserSettingsResponse{}
	f := func() (proto.Message, error) {
		return s.client.PutUserSettings(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil

}

func (s *UserSettingsServiceServer) DeleteUserSettings(ctx context.Context, in *external_user_settings.DeleteUserSettingsRequest) (*gp.Empty, error) {
	inDomain := &user_settings.DeleteUserSettingsRequest{}
	out := &gp.Empty{}
	f := func() (proto.Message, error) {
		return s.client.DeleteUserSettings(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}
