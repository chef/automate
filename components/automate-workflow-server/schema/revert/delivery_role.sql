-- Revert delivery_role

BEGIN;

DROP TYPE IF EXISTS delivery_role;

COMMIT;
