-- Verify organizations

BEGIN;

SELECT id, enterprise_id, name FROM organizations WHERE FALSE;

ROLLBACK;
