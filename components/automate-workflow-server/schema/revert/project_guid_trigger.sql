-- Revert project_guid_trigger

BEGIN;

DROP TRIGGER IF EXISTS update_project_guid
  ON projects;

COMMIT;
