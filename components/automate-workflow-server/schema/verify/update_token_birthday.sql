-- Verify update_token_birthday

BEGIN;

SELECT has_function_privilege(
  'update_token_birthday()',
  'execute');


ROLLBACK;
