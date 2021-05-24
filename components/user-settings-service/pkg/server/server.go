package server

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"github.com/chef/automate/api/external/lib/errorutils"

	pb "github.com/golang/protobuf/ptypes/empty"

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
	logrus.Infof("GetUserSettings from server.go - RIGHT HERE!!!!!!")

	name := req.GetUser().GetName()
	if name == "" {
		return nil, errors.New("invalid user Id")
	}
	connector := req.GetUser().GetConnector()
	if connector == "" {
		return nil, errors.New("invalid connector")
	}

	logrus.Infof("Get settings for connector: %s user: %s", connector, name)
	getUserSettingsResp, err := s.db.GetUserSettings(name, connector)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err,
			fmt.Sprintf("for connector: %s and user: %s", connector, name))
	}
	logrus.Infof("GetUserSettings response: %v", getUserSettingsResp)
	return getUserSettingsResp, nil
}

//	rpc UpdateUserSettings(PutUserSettings) returns (UpdateUserSettingsResponse) {
func (s *UserSettingsServer) PutUserSettings(ctx context.Context, req *user_settings.PutUserSettingsRequest) (*user_settings.PutUserSettingsResponse, error) {
	logrus.Infof("PutUserSettings from server.go")
	name := req.GetUser().GetName()
	if name == "" {
		return nil, errors.New("invalid user Id")
	}
	connector := req.GetUser().GetConnector()
	if connector == "" {
		return nil, errors.New("invalid connector")
	}
	logrus.Infof("Put settings for connector: %s user: %s", connector, name)
	var userSettings map[string]*user_settings.UserSettingValue
	userSettings = req.GetSettings()
	logrus.Infof("userSettings: %v", userSettings)

	putUserSettingsResp, err := s.db.PutUserSettings(name, connector, userSettings)
	if err != nil {
		return nil, err
	}
	logrus.Infof("PutUserSettings response: %v", putUserSettingsResp)
	return putUserSettingsResp, nil
}

//rpc DeleteUserSettings(DeleteUserSettingsRequest) returns (DeleteUserSettingsResponse){
func (s *UserSettingsServer) DeleteUserSettings(ctx context.Context, req *user_settings.DeleteUserSettingsRequest) (*pb.Empty, error) {
	logrus.Infof("DeleteUserSettings from server.go")
	name := req.GetUser().GetName()
	if name == "" {
		return nil, errors.New("invalid user Id")
	}
	connector := req.GetUser().GetConnector()
	if connector == "" {
		return nil, errors.New("invalid connector")
	}
	logrus.Infof("Put settings for connector: %s user: %s", connector, name)
	// delete the user settings for this id
	err := s.db.DeleteUserSettings(name, connector)
	if err != nil {
		return nil, err
	}

	return &empty, nil
}
