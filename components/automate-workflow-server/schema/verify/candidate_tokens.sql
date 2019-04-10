-- Verify candidate_token

BEGIN;

SELECT has_function_privilege(
  'candidate_tokens(text, text, text)',
  'execute');

ROLLBACK;
