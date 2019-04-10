-- Deploy delivery:jobs_runners_table to pg

BEGIN;

CREATE TABLE IF NOT EXISTS jobs_runners(
    id UUID NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
    name TEXT NOT NULL UNIQUE, -- hostname
    private_key TEXT NOT NULL
);

COMMIT;
