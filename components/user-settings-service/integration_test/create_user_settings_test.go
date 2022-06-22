package integration_test

import (
	"context"
	"github.com/chef/automate/api/interservice/user_settings"

	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCreateSecretEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(user_settings.PutUserSettingsRequest)

	_, err := userSettingsServer.PutUserSettings(ctx, req)

	assert.Error(t, err)
}

func TestCreateUserSettingsSuccessful(t *testing.T) {
	ctx := context.Background()

	userSettings := struct {
		name      string
		connector string
		settings  []struct {
			settingName  string
			defaultValue string
			value        string
			enabled      bool
			validValues  []string
		}
		//settings map[string]user_settings.UserSettingValue
	}{
		name:      "rick",
		connector: "local",
		settings: []struct {
			settingName  string
			defaultValue string
			value        string
			enabled      bool
			validValues  []string
		}{
			{
				settingName:  "date_format",
				defaultValue: "yy-mm-dd",
				value:        "YYYY-DD-MM",
				enabled:      true,
				validValues: []string{
					"blah blah blah",
					"blah1 blah blah",
					"blah2 blah blah",
				},
			},
		},
	}

	initialSetting := &user_settings.PutUserSettingsRequest{
		User: &user_settings.User{
			Name:      userSettings.name,
			Connector: userSettings.connector,
		},
	}
	for _, data := range userSettings.settings {
		initialSetting.Settings[data.settingName] = &user_settings.UserSettingValue{
			DefaultValue: data.defaultValue,
			Value:        data.value,
			Enabled:      data.enabled,
			ValidValues:  data.validValues,
		}
	}

	resp, err := userSettingsServer.PutUserSettings(ctx, initialSetting)
	assert.NoError(t, err)
	assert.NotNil(t, resp)

	responseUserSetting, err := userSettingsServer.GetUserSettings(
		ctx,
		&user_settings.GetUserSettingsRequest{User: &user_settings.User{Name: "rick", Connector: "local"}},
	)
	assert.NoError(t, err)

	assert.Equal(t, resp.User, responseUserSetting.User)
	assert.Equal(t, initialSetting.User, responseUserSetting.User)

	//deleteAllSecrets()
}

func TestCreateSecretFailure(t *testing.T) {

}
