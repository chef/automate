CREATE TABLE IF NOT EXISTS user_settings
(
    id SERIAL PRIMARY KEY,
    user_name TEXT NOT NULL DEFAULT '',
    connector TEXT NOT NULL DEFAULT 'local',
    settings JSON  NOT NULL DEFAULT '[]'::json,
    CONSTRAINT user_settings_user_name_and_connector UNIQUE (user_name, connector)
);

INSERT INTO user_settings (user_name, connector, settings)
VALUES ('_default', 'local',
        '{
          "date_format": {
            "default_value": "ddd, DD MMM YYYY",
            "value": "ddd, DD MMM YYYY",
            "enabled": true,
            "valid_values": [
              "ddd, DD MMM YYYY HH:mm:ss [UTC]",
              "YYYY-M-D",
              "ddd, DD MMM YYYY",
              "DD MMM YYYY",
              "ddd, DD MMM",
              "YYYY-MM-DD"
            ]
          }
        }')
ON CONFLICT ON CONSTRAINT user_settings_user_name_and_connector
    DO UPDATE
    SET settings='{
      "date_format": {
        "default_value": "ddd, DD MMM YYYY",
        "value": "ddd, DD MMM YYYY",
        "enabled": true,
        "valid_values": [
          "ddd, DD MMM YYYY HH:mm:ss [UTC]",
          "YYYY-M-D",
          "ddd, DD MMM YYYY",
          "DD MMM YYYY",
          "ddd, DD MMM",
          "YYYY-MM-DD"
        ]
      }
    }';