CREATE TABLE IF NOT EXISTS user_settings (
  id          SERIAL PRIMARY KEY,
  user_name   TEXT   NOT NULL DEFAULT '',
  connector   TEXT   NOT NULL DEFAULT 'local',
  settings    JSON   NOT NULL DFAULT '[]'::json,
);
