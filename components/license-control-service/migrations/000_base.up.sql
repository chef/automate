BEGIN;

CREATE TABLE IF NOT EXISTS licenses (
       db_id          BIGSERIAL PRIMARY KEY,
       active         BOOL UNIQUE,
       configured_at  TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
       data           TEXT UNIQUE,

       -- A variation on the "one row table" trick, but uses the fact
       -- that we've allowed NULL for the active column to allow
       -- multiple "NULL" rows but only a single true row. The
       -- following check ensures we never put `false` into active as
       -- a reminder about how we are using this.
       --
       -- See also:
       --
       -- https://github.com/chef/automate/blob/9e0022b71c146794e594c4f598464e06f5c141f4/components/authz-service/storage/postgres/migration/sql/20_add_migration_status_table.up.sql#L44-L46
       -- https://dba.stackexchange.com/questions/197562/constraint-one-boolean-row-is-true-all-other-rows-false
       --
       CONSTRAINT licence_active_true_or_null CHECK (active)
);

CREATE OR REPLACE FUNCTION set_active_license_v1(_license_data TEXT)
RETURNS VOID
AS $$
BEGIN
        UPDATE licenses SET active = NULL;

        INSERT INTO licenses(data, active) VALUES (_license_data, TRUE)
        ON CONFLICT (data)
        DO UPDATE SET configured_at = NOW(), active = TRUE;
END
$$
LANGUAGE plpgsql;

COMMIT;
