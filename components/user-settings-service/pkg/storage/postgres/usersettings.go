package postgres

import (
	"encoding/json"
	"github.com/chef/automate/api/interservice/user_settings"
)

const selectUserSettings = `
SELECT 
user_name,
connector,
settings
from user_settings
where user_name = %s
`

type UserSettings struct {
	UserName  string          `db:"user_name"`
	Connector string          `db:"connector"`
	Settings  json.RawMessage `db:"settings"`
}

func (db *Postgres) GetUserSettings(request *user_settings.GetUserSettingsRequest) (error, *user_settings.GetUserSettingsResponse) {
	userSettingsData := UserSettings{}
	var Id string = request.GetId()
	_, err := db.Select(&userSettingsData, selectUserSettings, Id)
	if err != nil {
		return err, nil
	}

	var data map[string]*user_settings.UserSettingValue
	err = json.Unmarshal(userSettingsData.Settings, &data)
	if err != nil {
		return err, nil
	}
	settingsResponse := &user_settings.GetUserSettingsResponse{
		Id:       userSettingsData.UserName,
		Settings: data,
	}

	return nil, settingsResponse
}
