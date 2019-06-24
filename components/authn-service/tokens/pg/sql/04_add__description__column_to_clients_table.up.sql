ALTER TABLE chef_authn_clients ADD COLUMN description TEXT NOT NULL CHECK (description <> '');
