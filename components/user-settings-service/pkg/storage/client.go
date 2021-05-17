package storage

import "github.com/chef/automate/api/interservice/user_settings"

type Client interface {
	GetUserSettings(*user_settings.GetUserSettingsRequest) (error, *user_settings.GetUserSettingsResponse)
}
