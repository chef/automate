-- Deploy dependency_failures

BEGIN;

  CREATE TABLE IF NOT EXISTS dependency_failures(
    enterprise_id BIGINT NOT NULL REFERENCES enterprises(id) ON DELETE CASCADE,
    pipeline_id BIGINT NOT NULL REFERENCES pipelines(id),
    grouping_id BIGINT,
    status TEXT
    );

COMMIT;
