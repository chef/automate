-- Deploy scoped_stage_runs_view
-- requires: organizations
-- requires: projects
-- requires: pipelines
-- requires: changes
-- requires: stage_runs

BEGIN;

CREATE OR REPLACE VIEW scoped_stage_runs AS
SELECT o.enterprise_id AS enterprise_id,
       o.id AS organization_id,
       p.id AS project_id,
       pipe.id AS pipeline_id,
       s.*
FROM stage_runs AS s
JOIN changes AS c
  ON s.change_id = c.id
JOIN pipelines AS pipe
  ON c.pipeline_id = pipe.id
JOIN projects AS p
  ON pipe.project_id = p.id
JOIN organizations AS o
  ON p.organization_id = o.id
;

COMMENT ON VIEW scoped_stage_runs IS
$$Provides a view of the `stage_runs` table with IDs for the enterprise,
organization, project and pipeline that the stage run is associated
with (through the specific change that triggered the run).

This is useful as a base for aggregating stage run information at
various levels, to help find out what the current state of a given
environment is, for instance.$$;

COMMIT;
