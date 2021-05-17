package handler

import (
	"context"
	external_user_settings "github.com/chef/automate/api/external/user_settings"
	"github.com/chef/automate/api/interservice/user_settings"

	log "github.com/sirupsen/logrus"
)

type UserSettingsServiceServer struct {
	client user_settings.UserSettingsServiceClient
}

func NewUserSettingsHandler(client user_settings.UserSettingsServiceClient) *UserSettingsServiceServer {
	return &UserSettingsServiceServer{
		client: client,
	}
}

func (s *UserSettingsServiceServer) GetUserSettings(ctx context.Context, request *external_user_settings.GetUserSettingsRequest) (*external_user_settings.GetUserSettingsResponse, error) {
	clientReq := &user_settings.GetUserSettingsRequest{
		Id: request.GetId(),
	}
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	clientResponse, err := s.client.GetUserSettings(ctx, clientReq)
	if err != nil {
		return nil, err
	}

	var settings map[string]*external_user_settings.UserSettingValue
	for k, v := range clientResponse.GetSettings() {
		settings[k] = &external_user_settings.UserSettingValue{
			DefaultValue: v.GetDefaultValue(),
			Value:        v.GetValue(),
			Enabled:      v.GetEnabled(),
		}
	}
	return &external_user_settings.GetUserSettingsResponse{
		Id:       clientResponse.GetId(),
		Settings: settings,
	}, nil
}

func (s *UserSettingsServiceServer) PutUserSettings(ctx context.Context, request *external_user_settings.PutUserSettingsRequest) (*external_user_settings.PutUserSettingsResponse, error) {
	return &external_user_settings.PutUserSettingsResponse{
		Id: request.GetId(),
	}, nil
}

func (s *UserSettingsServiceServer) DeleteUserSettings(ctx context.Context, request *external_user_settings.DeleteUserSettingsRequest) (*external_user_settings.DeleteUserSettingsResponse, error) {
	return &external_user_settings.DeleteUserSettingsResponse{
		Id: request.GetId(),
	}, nil
}
