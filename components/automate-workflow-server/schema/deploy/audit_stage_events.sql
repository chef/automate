-- Deploy audit_stage_events

BEGIN;

  CREATE TABLE IF NOT EXISTS audit_stage_events(
    id UUID NOT NULL PRIMARY KEY,
    action TEXT,
    create_time cd_timestamp NOT NULL,
    status TEXT,
    change_id UUID,
    change_title TEXT,
    ent_id BIGINT NOT NULL REFERENCES enterprises(id) ON DELETE CASCADE,
    ent TEXT,
    org TEXT,
    pipe TEXT,
    proj TEXT,
    stage_name TEXT,
    submitted_by TEXT,
    submitted_at cd_timestamp,
    approved_by TEXT,
    delivered_by TEXT);

COMMIT;
