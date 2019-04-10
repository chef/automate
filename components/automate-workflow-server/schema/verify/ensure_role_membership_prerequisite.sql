-- Verify ensure_role_membership_prerequisite

BEGIN;

SELECT has_function_privilege(
  'ensure_role_membership_prerequisite()',
  'execute');

ROLLBACK;
