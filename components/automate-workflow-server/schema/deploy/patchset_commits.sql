-- Deploy patchset_commits

BEGIN;

CREATE OR REPLACE VIEW patchset_commits AS
  SELECT ppc.patchset_id,
         pc.sha,
         pc.subject,
         pc.body
    FROM patchset_project_commits AS ppc
    JOIN project_commits AS pc
      ON ppc.project_commit_id = pc.id
ORDER BY pc.id;

COMMIT;
