-- Verify delivery_role

BEGIN;

-- If the type isn't there, this will fail horribly, as we want.
SELECT 'admin'::delivery_role;

ROLLBACK;
