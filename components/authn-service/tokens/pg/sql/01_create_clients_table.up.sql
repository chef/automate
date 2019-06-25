CREATE TABLE IF NOT EXISTS chef_authn_clients (
                  id TEXT NOT NULL PRIMARY KEY,
                  token TEXT NOT NULL,
                  created TIMESTAMPTZ,
                  updated TIMESTAMPTZ
                );
