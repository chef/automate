DROP TABLE IF EXISTS nodes_secrets CASCADE;

ALTER TABLE IF EXISTS jobs_profiles
  DROP CONSTRAINT IF EXISTS jobs_profiles_profile_id_fkey,
  add constraint jobs_profiles_profile_id_fkey FOREIGN KEY (profile_id) REFERENCES profiles(id) ON DELETE CASCADE;
