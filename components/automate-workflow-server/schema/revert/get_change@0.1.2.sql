-- Deploy get_change

BEGIN;
DROP FUNCTION get_change(uuid);
CREATE OR REPLACE FUNCTION get_change(p_change_id changes.id%TYPE)
RETURNS TABLE(
        id changes.id%TYPE,
        feature_branch changes.feature_branch%TYPE,
        pipeline pipelines.name%TYPE,
        status cd_patchset_status,
        submitted_at cd_timestamp,
        submitted_by users.name%TYPE)
LANGUAGE SQL STABLE
ROWS 1
AS $$
   WITH change_patchsets AS (
     SELECT change_id, sequence_number, status, submitted_at, submitter_id, max(l.sequence_number) OVER (PARTITION BY change_id) AS latest
     FROM patchsets AS l
     WHERE l.change_id = p_change_id
   )
   SELECT c.id,
          c.feature_branch,
          p.name AS pipeline,
          latest.status,
          first.submitted_at,
          u.name AS submitted_by
   FROM changes AS c
   JOIN pipelines AS p
     ON c.pipeline_id = p.id
   JOIN change_patchsets AS first
     ON first.change_id = c.id
    AND first.sequence_number = 1
   JOIN change_patchsets AS latest
     ON latest.latest = latest.sequence_number
   JOIN users AS u
     ON first.submitter_id = u.id
   WHERE c.id = p_change_id;
$$;

COMMIT;
