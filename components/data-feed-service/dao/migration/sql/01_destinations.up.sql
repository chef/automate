CREATE TABLE IF NOT EXISTS destinations (
  id     SERIAL,
  name   TEXT NOT NULL UNIQUE,
  url    TEXT NOT NULL,
  secret TEXT,
  PRIMARY KEY (id)
);
