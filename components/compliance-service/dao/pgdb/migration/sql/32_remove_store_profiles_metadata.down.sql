ALTER TABLE IF EXISTS store_profiles
  ADD COLUMN IF NOT EXISTS metadata jsonb NOT NULL;

CREATE INDEX IF NOT EXISTS idxprofilename ON store_profiles ((metadata->>'name'));
CREATE INDEX IF NOT EXISTS idxprofileversion ON store_profiles ((metadata->>'version'));
