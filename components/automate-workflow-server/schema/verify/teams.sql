-- Verify users

BEGIN;

SELECT id,
       enterprise_id,
       name,
       description
FROM teams WHERE FALSE;

ROLLBACK;
