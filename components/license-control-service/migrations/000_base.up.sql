BEGIN;

CREATE TABLE IF NOT EXISTS license (
       -- TODO(ssd) 2019-07-29: This is a single-row table currently,
       -- do we want to keep history?
       current BOOL PRIMARY KEY DEFAULT TRUE,
       configured_at TIMESTAMP WITH TIMEZONE DEFAULT NOW(),
       data    TEXT
);

COMMIT;
