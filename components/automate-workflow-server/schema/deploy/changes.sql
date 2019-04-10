-- Deploy changes
-- requires: pipelines

BEGIN;

CREATE TABLE IF NOT EXISTS changes(
  id UUID NOT NULL PRIMARY KEY,
  pipeline_id BIGINT NOT NULL REFERENCES pipelines(id) ON DELETE CASCADE,
  feature_branch TEXT NOT NULL,
  -- null until the change gets merged, at which point this is set to the SHA
  -- of the last commit on the last patch set
  merge_sha TEXT
  -- TODO To be continued...
);

-- a change that hasn't passed review yet is uniquely identified by
-- its pipeline and feature branch
CREATE UNIQUE INDEX feature_branch_pipeline_id_not_merged_idx
ON changes(pipeline_id, feature_branch)
WHERE merge_sha IS NULL;

COMMIT;
