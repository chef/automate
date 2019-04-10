-- Verify revoke_token

BEGIN;

SELECT has_function_privilege(
  'revoke_token(TEXT, TEXT, TEXT)',
  'execute');

ROLLBACK;
