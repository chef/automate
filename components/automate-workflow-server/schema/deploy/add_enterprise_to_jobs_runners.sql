-- Deploy delivery:add_enterprise_to_jobs_runners to pg
-- requires: jobs_runners_table
-- Adding the enterprise_id reference to the jobs_runners table

BEGIN;

ALTER TABLE jobs_runners ADD COLUMN enterprise_id BIGINT;

UPDATE jobs_runners
SET enterprise_id = (SELECT id FROM enterprises LIMIT 1)
FROM enterprises;

ALTER TABLE jobs_runners ALTER enterprise_id SET NOT NULL;
ALTER TABLE jobs_runners ADD CONSTRAINT enterprise_id_fk
  FOREIGN KEY (enterprise_id) REFERENCES enterprises(id) ON DELETE CASCADE;
ALTER TABLE jobs_runners DROP CONSTRAINT jobs_runners_name_key;
ALTER TABLE jobs_runners ADD CONSTRAINT name_enterprise_id_unique UNIQUE (name, enterprise_id);

COMMIT;
