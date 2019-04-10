-- Revert delivery_scope

BEGIN;

DROP TYPE IF EXISTS delivery_scope;

COMMIT;
