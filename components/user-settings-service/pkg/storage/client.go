package storage

import "github.com/chef/automate/api/interservice/user_settings"

type Client interface {
	GetUserSettings(string, string) (*user_settings.GetUserSettingsResponse, error)
}
