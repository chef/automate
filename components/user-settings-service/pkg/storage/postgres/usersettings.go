package postgres

import (
	"encoding/json"
	"fmt"

	"github.com/chef/automate/api/external/lib/errorutils"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/user_settings"
)

const selectUserSettings = `
SELECT 
id,
user_name,
connector,
settings
from user_settings 
WHERE user_name = $1 and connector = $2;
`

const deleteUserSettings = `
DELETE FROM user_settings 
WHERE user_name = $1 and connector = $2;
`

const upsertUserSettings = `
INSERT INTO user_settings(user_name, connector, settings)
VALUES ($1, $2, $3)
ON CONFLICT ON CONSTRAINT user_settings_user_name_and_connector
DO UPDATE SET (user_name, connector, settings) = ($1,$2,$3);
`

type UserSettings struct {
	Id        int32           `db:"id, primarykey, autoincrement"`
	UserName  string          `db:"user_name"`
	Connector string          `db:"connector"`
	Settings  json.RawMessage `db:"settings"`
}

//GetUserSettings retrieves user settings from the database
func (db *DB) GetUserSettings(name string, connector string) (*user_settings.GetUserSettingsResponse, error) {
	userSettingsData := UserSettings{}
	settingsResponse := &user_settings.GetUserSettingsResponse{
		User: &user_settings.User{
			Name:      name,
			Connector: connector,
		},
	}
	err := db.SelectOne(&userSettingsData, selectUserSettings, name, connector)
	if err != nil {
		return settingsResponse, errorutils.ProcessSQLNotFound(err,
			fmt.Sprintf("connector: $s, user_name: %s", connector, name), "GetJob error")
	}
	logrus.Infof("userSettingsData.Settings %v", userSettingsData.Settings)

	var data map[string]*user_settings.UserSettingValue
	err = json.Unmarshal(userSettingsData.Settings, &data)
	if err != nil {
		return nil, err
	}
	settingsResponse.Settings = data

	return settingsResponse, err
}

func (db *DB) PutUserSettings(name string, connector string,
	settings map[string]*user_settings.UserSettingValue) (*user_settings.PutUserSettingsResponse, error) {
	jsonString, err := json.Marshal(settings)
	if err != nil {
		return nil, err
	}

	_, err = db.Exec(upsertUserSettings, name, connector, jsonString)
	if err != nil {
		return nil, errors.Wrap(err, "PutUserSettings unable to update user settings")
	}

	return &user_settings.PutUserSettingsResponse{
		User: &user_settings.User{
			Name:      name,
			Connector: connector,
		},
	}, nil
}

func (db *DB) DeleteUserSettings(name string, connector string) error {
	_, err := db.Exec(deleteUserSettings, name, connector)
	if err != nil {
		return errors.Wrapf(err, "DeleteUserSettings unable to delete user settings for user: %s connector: %s",
			name, connector)
	}
	return nil
}
