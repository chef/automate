-- Deploy get_change

BEGIN;
CREATE OR REPLACE FUNCTION get_change(p_change_id changes.id%TYPE)
RETURNS TABLE(
        id changes.id%TYPE,
        feature_branch changes.feature_branch%TYPE,
        pipeline pipelines.name%TYPE,
        status cd_patchset_status,
        submitted_at cd_timestamp,
        submitted_by users.name%TYPE,
        merge_sha changes.merge_sha%TYPE,
        approved_by changes.approved_by%TYPE,
        changeset_id changes.changeset_id%TYPE)
LANGUAGE SQL STABLE
ROWS 1
AS $$
   SELECT c.id,
          c.feature_branch,
          p.name AS pipeline,
          c.latest_patchset_status,
          c.submitted_at,
          c.submitted_by,
          c.merge_sha,
          c.approved_by,
          c.changeset_id
   FROM changes AS c
   JOIN pipelines AS p
     ON c.pipeline_id = p.id
   WHERE c.id = p_change_id;
$$;

COMMIT;
