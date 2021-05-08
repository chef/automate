CREATE TABLE IF NOT EXISTS user_settings
(
    id       text NOT NULL,
    settings json NOT NULL,
    primary key (id)
);
