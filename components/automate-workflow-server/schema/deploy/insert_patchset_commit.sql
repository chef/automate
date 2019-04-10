-- Deploy insert_patchset_commit
-- requires: patchset_project_commits

-- Inserts a new patchset commit
-- Handles the nitty-gritty details of how the commits' info is actually stored

BEGIN;

CREATE OR REPLACE FUNCTION insert_patchset_commit(
  p_project_id project_commits.project_id%TYPE,
  p_patchset_id patchset_project_commits.patchset_id%TYPE,
  p_sha project_commits.sha%TYPE,
  p_subject project_commits.subject%TYPE,
  p_body project_commits.body%TYPE
)
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  v_project_commit_id project_commits.id%TYPE;
BEGIN

  SELECT id
  FROM project_commits
  WHERE project_id = p_project_id
  AND sha = p_sha
  INTO v_project_commit_id;

  IF NOT FOUND THEN
    INSERT INTO project_commits(project_id, sha, subject, body)
    VALUES (p_project_id, p_sha, p_subject, p_body)
    RETURNING id
    INTO v_project_commit_id;
  END IF;

  INSERT INTO patchset_project_commits(patchset_id, project_commit_id)
  VALUES (p_patchset_id, v_project_commit_id);

END;
$$;

COMMIT;
