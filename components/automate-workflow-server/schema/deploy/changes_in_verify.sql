-- Deploy changes_in_verify

BEGIN;

CREATE OR REPLACE FUNCTION changes_in_verify(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE
)
RETURNS TABLE(
  project projects.name%TYPE,
  pipeline pipelines.name%TYPE,
  feature_branch changes.feature_branch%TYPE,
  change_id changes.id%TYPE,
  created_at patchsets.submitted_at%TYPE,
  last_code_activity_at patchsets.submitted_at%TYPE,
  status stage_runs.status%TYPE,
  finished stage_runs.finished%TYPE)
LANGUAGE SQL STABLE
AS $$
WITH
-- Retrieve all unmerged changes in all projects in the given
-- organization.
unmerged_changes_in_org AS (
  SELECT p.name AS project,
         pipe.name AS pipeline,
         c.id AS change_id,
         c.feature_branch,
         c.submitted_at AS created_at,
         c.latest_patchset,
         c.latest_patchset_status,
         patch.submitted_at AS latest_code_activity_at
  FROM enterprises AS e
  JOIN organizations AS o
    ON e.id = o.enterprise_id
  JOIN projects AS p
    ON o.id = p.organization_id
  JOIN pipelines AS pipe
    ON p.id = pipe.project_id
  JOIN changes AS c
    ON pipe.id = c.pipeline_id
  JOIN patchsets AS patch
    ON (c.id = patch.change_id AND c.latest_patchset = patch.sequence_number)
 WHERE e.name = p_enterprise_name
   AND o.name = p_organization_name
   AND c.merge_sha IS NULL
   AND c.latest_patchset_status = 'open'
),
-- We can run 'verify' multiple times for a given change (and even for
-- a single patchset). However, we only really care about the most
-- recent one for the change as a whole (as determined by the stage
-- ID, imperfect as that is)
latest_verify_stage_runs AS (
    SELECT change_id, status, finished
      FROM (
        SELECT s.id,
               s.change_id,
               s.stage,
               s.status,
               s.finished,
               max(s.id) OVER (partition BY s.change_id, s.stage) AS latest
          FROM stage_runs AS s
          JOIN unmerged_changes_in_org AS c
            ON c.change_id = s.change_id
         WHERE stage = 'verify' -- Should be redundant with the
                                -- unmerged changes, but better safe
                                -- than sorry, I suppose
       ) AS work
    WHERE work.id = latest
)
SELECT c.project,
       c.pipeline,
       c.feature_branch,
       c.change_id,
       c.created_at,
       c.latest_code_activity_at,
       s.status,
       s.finished
FROM unmerged_changes_in_org AS c
JOIN latest_verify_stage_runs AS s
  ON c.change_id = s.change_id
$$;

COMMENT ON FUNCTION changes_in_verify(
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE) IS
$$For a given organization, retrieve information on all changes that
are currently in the review stage.$$;

COMMIT;
