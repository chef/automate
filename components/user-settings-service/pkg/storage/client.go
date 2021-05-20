package storage

import "github.com/chef/automate/api/interservice/user_settings"

type Client interface {
	GetUserSettings(string) (*user_settings.GetUserSettingsResponse, error)
	PutUserSettings(*user_settings.PutUserSettingsRequest) (*user_settings.PutUserSettingsResponse, error)
	DeleteUserSettings(string) error
}
