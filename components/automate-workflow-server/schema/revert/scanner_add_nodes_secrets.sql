-- Revert delivery:scanner_add_nodes_secrets from pg

BEGIN;

DROP TABLE IF EXISTS nodes_secrets CASCADE;

ALTER TABLE jobs_profiles
  DROP CONSTRAINT IF EXISTS jobs_profiles_profile_id_fkey,
  ADD CONSTRAINT jobs_profiles_profile_id_fkey FOREIGN KEY (profile_id) REFERENCES profiles(id) ON DELETE CASCADE;

COMMIT;
