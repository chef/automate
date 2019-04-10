-- Verify role_triggers

BEGIN;

 -- this will throw an exception if the trigger doesn't exist
SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'enterprise_user_roles'
  AND trigger_name       = 'ensure_membership'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'INSERT'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE ensure_role_membership_prerequisite()';

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'enterprise_user_roles'
  AND trigger_name       = 'ensure_membership'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'UPDATE'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE ensure_role_membership_prerequisite()';

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'organization_user_roles'
  AND trigger_name       = 'ensure_membership'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'INSERT'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE ensure_role_membership_prerequisite()';

SELECT 1/COUNT(*)
FROM information_schema.triggers
WHERE event_object_table = 'organization_user_roles'
  AND trigger_name       = 'ensure_membership'
  AND action_timing      = 'BEFORE'
  AND event_manipulation = 'UPDATE'
  AND action_orientation = 'ROW'
  AND action_statement   = 'EXECUTE PROCEDURE ensure_role_membership_prerequisite()';

ROLLBACK;
