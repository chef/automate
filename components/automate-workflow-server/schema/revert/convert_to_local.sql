-- Revert delivery:convert_to_local from pg

BEGIN;

DROP FUNCTION IF EXISTS convert_to_local(p_project_id projects.id%TYPE);

COMMIT;
