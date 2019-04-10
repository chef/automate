CREATE TABLE IF NOT EXISTS nodes_secrets (
  node_id   text NOT NULL references nodes(id) ON DELETE CASCADE,
  secret_id text NOT NULL references secrets(id)
);

ALTER TABLE IF EXISTS jobs_profiles
  DROP CONSTRAINT IF EXISTS jobs_profiles_profile_id_fkey,
  add constraint jobs_profiles_profile_id_fkey FOREIGN KEY (profile_id) REFERENCES profiles(id);
