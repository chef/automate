-- Verify add_ent_id_to_external_basic_auth_applications

BEGIN;

SELECT ent_id
FROM external_basic_auth_applications
WHERE FALSE;

ROLLBACK;
