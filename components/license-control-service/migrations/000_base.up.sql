BEGIN;

CREATE TABLE IF NOT EXISTS licenses (
       db_id          BIGSERIAL PRIMARY KEY,
       active         BOOL UNIQUE,
       configured_at  TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
       data           TEXT UNIQUE,

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
