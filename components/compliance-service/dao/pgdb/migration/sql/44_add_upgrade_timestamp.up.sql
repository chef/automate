ALTER TABLE IF EXISTS upgrade_flags
    ADD COLUMN IF NOT EXISTS upgrade_timestamp timestamp without time zone DEFAULT NULL;
