-- Deploy changesets
-- requires: pipelines
-- requires: cd_changeset_status

BEGIN;

CREATE TABLE IF NOT EXISTS changesets(
  id UUID NOT NULL PRIMARY KEY,
  pipeline_id BIGINT NOT NULL REFERENCES pipelines(id) ON DELETE CASCADE,
  status cd_changeset_status NOT NULL,
  -- both of these are NULL iff this is the on-going changeset
  -- for this pipeline
  delivered_at cd_timestamp DEFAULT NULL,
  -- same idea as the 'approved_by' fields in the changes' table
  -- this is more like audit data and we want to protect this data
  -- even if the user gets deleted
  delivered_by TEXT,
  CHECK (
    CASE status
    WHEN 'open' THEN
      delivered_at IS NULL
      AND delivered_by IS NULL
    WHEN 'closed' THEN
      delivered_at IS NOT NULL
      AND delivered_by IS NOT NULL
    ELSE FALSE
    END
  )
);

-- there's only one open changeset for a given pipeline
CREATE UNIQUE INDEX single_open_changeset_idx
ON changesets(pipeline_id)
WHERE status = 'open';

COMMIT;
