package server

import (
	"context"
	"errors"
	"sync"

	pb "github.com/golang/protobuf/ptypes/empty"

	"github.com/chef/automate/api/external/lib/errorutils"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/user_settings"
	"github.com/chef/automate/components/compliance-service/inspec-agent/scheduler"
	"github.com/chef/automate/components/user-settings-service/pkg/storage"
	"github.com/chef/automate/components/user-settings-service/pkg/storage/postgres"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/sirupsen/logrus"
)

var empty = pb.Empty{}

//UserSettingsServer is a map of user Id to map of
type UserSettingsServer struct {
	db            *postgres.DB
	storageClient storage.Client
	health        *health.Service
	mutex         sync.Mutex
	vals          map[string]map[string]*user_settings.UserSettingValue
}

type Server struct {
	db              *postgres.DB
	connFactory     *secureconn.Factory
	eventsClient    automate_event.EventServiceClient
	schedulerServer *scheduler.Scheduler
}

func New(db *postgres.DB, storage storage.Client) *UserSettingsServer {
	return &UserSettingsServer{
		db:            db,
		storageClient: storage,
		health:        health.NewService(),
		vals:          make(map[string]map[string]*user_settings.UserSettingValue),
	}
}

func (s *UserSettingsServer) GetUserSettings(ctx context.Context, req *user_settings.GetUserSettingsRequest) (*user_settings.GetUserSettingsResponse, error) {
	id := req.GetId()
	if id == "" {
		return nil, errors.New("invalid user Id")
	}

	logrus.Infof("Get settings for user %q", id)
	userSettings, err := s.db.GetUserSettings(id)
	if err != nil {
		logrus.Error(err)
	}

	//fmt.Printf("%+v", resp)
	return &user_settings.GetUserSettingsResponse{Id: id, Settings: userSettings.GetSettings()}, nil
}

//	rpc UpdateUserSettings(PutUserSettings) returns (UpdateUserSettingsResponse) {
func (s *UserSettingsServer) PutUserSettings(ctx context.Context, req *user_settings.PutUserSettingsRequest) (*user_settings.PutUserSettingsResponse, error) {
	logrus.Infof("RIGHT HERE!!!!!!")
	id := req.GetId()
	logrus.Infof("PutUserSettings for id: %s", id)
	if id == "" {
		return nil, errors.New("invalid user Id")
	}
	logrus.Infof("PutUserSettings set %q to %q", id, req.GetSettings())
	putUserSettingsResp, err := s.db.PutUserSettings(req)
	if err != nil {
		return nil, err
	}
	logrus.Infof("PutUserSettings response: %v", putUserSettingsResp)
	return nil, nil
}

//rpc DeleteUserSettings(DeleteUserSettingsRequest) returns (DeleteUserSettingsResponse){
func (s *UserSettingsServer) DeleteUserSettings(ctx context.Context, req *user_settings.DeleteUserSettingsRequest) (*user_settings.DeleteUserSettingsResponse, error) {
	logrus.Debugf("Deleting job id: %+v", req.Id)
	id := req.GetId()
	if id == "" {
		return nil, errors.New("invalid user Id")
	}
	// delete the user settings for this id
	err := s.db.DeleteUserSettings(id)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, id)
	}

	return &user_settings.DeleteUserSettingsResponse{Id: id}, nil
}

//from maps.. this is throw away code
func (s *UserSettingsServer) GetUserSettingsFromMap(ctx context.Context, req *user_settings.GetUserSettingsRequest) (*user_settings.GetUserSettingsResponse, error) {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	id := req.GetId()
	if id == "" {
		return nil, errors.New("invalid user Id")
	}

	logrus.Infof("Get settings for user %q", id)
	//err, resp := s.storageClient.GetUserSettings(req)
	//if err != nil {
	//	logrus.Error(err)
	//}

	//fmt.Printf("%+v", resp)
	return &user_settings.GetUserSettingsResponse{Id: id, Settings: s.vals[id]}, nil
}

//	rpc UpdateUserSettings(PutUserSettings) returns (UpdateUserSettingsResponse) {
func (s *UserSettingsServer) PutUserSettingsFromMap(ctx context.Context, req *user_settings.PutUserSettingsRequest) (*user_settings.PutUserSettingsResponse, error) {
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
func (s *UserSettingsServer) DeleteUserSettingsFromMap(ctx context.Context, req *user_settings.DeleteUserSettingsRequest) (*user_settings.DeleteUserSettingsResponse, error) {
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
