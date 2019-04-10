-- Deploy open_changes_view
-- requires: most_recent_patchsets_view

BEGIN;

CREATE OR REPLACE VIEW open_changes AS
  SELECT o.enterprise_id AS enterprise_id,
         o.id AS organization_id,
         p.id AS project_id,
         c.id,
         c.pipeline_id,
         c.feature_branch,
         c.merge_sha,
         c.title,
         c.description,
         c.approved_by,
         c.changeset_id
    FROM changes AS c
    JOIN most_recent_patchsets AS patch
      ON patch.change_id = c.id
    JOIN pipelines AS pipe
      ON c.pipeline_id = pipe.id
    JOIN projects AS p
      ON pipe.project_id = p.id
    JOIN organizations AS o
      ON p.organization_id = o.id
   WHERE patch.status = 'open';

COMMENT ON VIEW open_changes IS
$$Shows only open changes. All closed and withdrawn changes are filtered
out. These are the *active* changes only!

This is the same as the changes table (modulo the above filtering) but
with enterprise, organization, and project IDs embedded in the
resultset. You can use these to filter (or not!) by enterprise,
organization, or project (and various subsets thereof).

$$;

COMMIT;
