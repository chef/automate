-- Revert insert_patchset_commit

BEGIN;

DROP FUNCTION IF EXISTS insert_patchset_commit(
  p_project_id project_commits.project_id%TYPE,
  p_patchset_id patchset_project_commits.patchset_id%TYPE,
  p_sha project_commits.sha%TYPE,
  p_subject project_commits.subject%TYPE,
  p_body project_commits.body%TYPE
);

COMMIT;
