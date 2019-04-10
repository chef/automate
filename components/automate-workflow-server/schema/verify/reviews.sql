-- Verify reviews

BEGIN;

SELECT id, patchset_id, user_id, numeric_value
FROM reviews
WHERE FALSE;

ROLLBACK;
