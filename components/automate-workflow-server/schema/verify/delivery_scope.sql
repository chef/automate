-- Verify delivery_scope

BEGIN;

-- If the type isn't there, this will fail horribly, as we want.
SELECT 'enterprise'::delivery_scope;

ROLLBACK;
