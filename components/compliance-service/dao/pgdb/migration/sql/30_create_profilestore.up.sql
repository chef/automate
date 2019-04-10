CREATE TABLE IF NOT EXISTS store_profiles (
  sha256       text NOT NULL UNIQUE,
  CHECK (sha256 <> ''),
  tar          BYTEA NOT NULL,
  info         JSONB NOT NULL,
  metadata     JSONB NOT NULL,
  primary key(sha256)
);

CREATE INDEX idxprofilename ON store_profiles ((metadata->>'name'));
CREATE INDEX idxprofileversion ON store_profiles ((metadata->>'version'));

CREATE TABLE IF NOT EXISTS store_market (
  sha256        text NOT NULL REFERENCES store_profiles (sha256),
  primary key(sha256)
);

CREATE TABLE IF NOT EXISTS store_namespace (
  owner         text NOT NULL,
  sha256        text NOT NULL REFERENCES store_profiles (sha256),
  primary key(owner, sha256)
);
