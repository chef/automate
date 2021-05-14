package server

import (
	"context"
	"errors"
	"sync"

	"github.com/chef/automate/api/interservice/user_settings"
	"github.com/sirupsen/logrus"
)

//UserSettingsServer is a map of user Id to map of
type UserSettingsServer struct {
	mutex sync.Mutex
	vals  map[string]map[string]*user_settings.UserSettingValue
}

func New() *UserSettingsServer {
	return &UserSettingsServer{
		vals: make(map[string]map[string]*user_settings.UserSettingValue),
	}
}

func (s *UserSettingsServer) GetUserSettings(ctx context.Context, req *user_settings.GetUserSettingsRequest) (*user_settings.GetUserSettingsResponse, error) {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	id := req.GetId()
	if id == "" {
		return nil, errors.New("invalid user Id")
	}

	logrus.Infof("Get settings for user %q", id)

	return &user_settings.GetUserSettingsResponse{Id: id, Settings: s.vals[id]}, nil
}

//	rpc UpdateUserSettings(PutUserSettings) returns (UpdateUserSettingsResponse) {
func (s *UserSettingsServer) PutUserSettings(ctx context.Context, req *user_settings.PutUserSettingsRequest) (*user_settings.PutUserSettingsResponse, error) {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	id := req.GetId()
	if id == "" {
		return nil, errors.New("invalid user Id")
	}

	if req.GetSettings() == nil {
		return nil, errors.New("invalid settings")
	}

	//impl here
	s.vals[id] = req.GetSettings()

	logrus.Infof("Set %q to %q", id, req.GetSettings())

	return &user_settings.PutUserSettingsResponse{Id: id}, nil
}

//rpc DeleteUserSettings(DeleteUserSettingsRequest) returns (DeleteUserSettingsResponse){
func (s *UserSettingsServer) DeleteUserSettings(ctx context.Context, req *user_settings.DeleteUserSettingsRequest) (*user_settings.DeleteUserSettingsResponse, error) {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	id := req.GetId()
	if id == "" {
		return nil, errors.New("invalid user Id")
	}

	_, ok := s.vals[id]
	if ok {
		delete(s.vals, id)
	}
	logrus.Infof("Deleted user settings for %q", id)

	return &user_settings.DeleteUserSettingsResponse{Id: id}, nil
}
