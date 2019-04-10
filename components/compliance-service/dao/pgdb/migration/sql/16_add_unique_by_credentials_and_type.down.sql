ALTER TABLE IF EXISTS node_managers
  ADD UNIQUE (credentials),
  DROP CONSTRAINT IF EXISTS "node_managers_credentials_type_key";
