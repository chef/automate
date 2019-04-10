-- Deploy add_ent_id_to_external_basic_auth_applications

BEGIN;

  ALTER TABLE external_basic_auth_applications ADD COLUMN ent_id BIGINT;

  UPDATE external_basic_auth_applications
  SET ent_id = (SELECT id FROM enterprises LIMIT 1)
  FROM enterprises;

  ALTER TABLE external_basic_auth_applications ALTER ent_id SET NOT NULL;
  ALTER TABLE external_basic_auth_applications DROP COLUMN id;
  ALTER TABLE external_basic_auth_applications ADD PRIMARY KEY (name, ent_id);
  ALTER TABLE external_basic_auth_applications ADD CONSTRAINT ent_id_fk
    FOREIGN KEY (ent_id) REFERENCES enterprises(id) ON DELETE CASCADE;

COMMIT;
