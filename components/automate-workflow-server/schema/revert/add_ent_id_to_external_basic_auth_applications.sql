-- Revert add_ent_id_to_external_basic_auth_applications

BEGIN;

  ALTER TABLE external_basic_auth_applications DROP COLUMN ent_id;

COMMIT;
