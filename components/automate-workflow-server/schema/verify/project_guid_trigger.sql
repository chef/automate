-- Verify project_guid_trigger

BEGIN;

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'projects'
  AND trigger_name       = 'update_project_guid'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'INSERT'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE update_project_guid()';

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'projects'
  AND trigger_name       = 'update_project_guid'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'UPDATE'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE update_project_guid()';

ROLLBACK;
