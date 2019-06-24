ALTER TABLE chef_authn_clients ALTER COLUMN id TYPE uuid USING id::uuid;
