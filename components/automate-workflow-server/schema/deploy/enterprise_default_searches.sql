-- Deploy enterprise_default_searches

BEGIN;

  CREATE TABLE IF NOT EXISTS enterprise_default_searches(
    id BIGSERIAL,
    ent_id BIGINT UNIQUE NOT NULL REFERENCES enterprises(id) ON DELETE CASCADE,
    search TEXT NOT NULL
    );

COMMIT;
