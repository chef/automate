package postgres

import (
	"encoding/json"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/user_settings"
)

const selectUserSettings = `
SELECT 
user_name,
connector,
settings
from user_settings
where user_name = $1
`

const deleteUserSettings = `
DELETE FROM user_settings
WHERE id = $1;
`

const upsertUserSettings = `
INSERT INTO user_settings(user_name, connector, settings)
VALUES ($1, $2, $3)
ON CONFLICT ON CONSTRAINT user_name_unique
DO UPDATE SET (EXCLUDED.user_name, EXCLUDED.connector, settings) = ($1,$2,$3);
`

type UserSettings struct {
	UserName  string          `db:"user_name"`
	Connector string          `db:"connector"`
	Settings  json.RawMessage `db:"settings"`
}

func (db *DB) GetUserSettings(id string) (*user_settings.GetUserSettingsResponse, error) {
	userSettingsData := UserSettings{}
	res, err := db.Select(&userSettingsData, selectUserSettings, id)
	if err != nil {
		return nil, err
	}
	logrus.Infof("res %v", res)

	var data map[string]*user_settings.UserSettingValue
	err = json.Unmarshal(userSettingsData.Settings, &data)
	if err != nil {
		return nil, err
	}
	settingsResponse := &user_settings.GetUserSettingsResponse{
		Id:       userSettingsData.UserName,
		Settings: data,
	}

	return settingsResponse, err
}

func (db *DB) PutUserSettings(inUserSettings *user_settings.PutUserSettingsRequest) (*user_settings.PutUserSettingsResponse, error) {
	logrus.Infof("MAKING IT SO")
	var userSettings map[string]*user_settings.UserSettingValue
	id := inUserSettings.GetId()
	userSettings = inUserSettings.GetSettings()

	jsonString, err := json.Marshal(userSettings)
	if err != nil {
		return nil, err
	}
	logrus.Infof("for id: %s, user settings: %s", id, jsonString)

	return &user_settings.PutUserSettingsResponse{Id: id}, nil
}

func (db *DB) DeleteUserSettings(id string) error {
	_, err := db.Exec(deleteUserSettings, id)
	if err != nil {
		return errors.Wrapf(err, "DeleteUserSettings unable to delete user settings for id: %s", id)
	}
	return nil
}
