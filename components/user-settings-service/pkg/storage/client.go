package storage

import "github.com/chef/automate/api/interservice/user_settings"

type Client interface {
	GetUserSettings(string, string) (*user_settings.GetUserSettingsResponse, error)
	PutUserSettings(string, string, map[string]*user_settings.UserSettingValue) (*user_settings.PutUserSettingsResponse, error)
	DeleteUserSettings(string, string) error
}
