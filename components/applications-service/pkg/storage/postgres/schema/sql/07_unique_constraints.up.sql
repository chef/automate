ALTER TABLE deployment ADD CONSTRAINT deployment_unique UNIQUE (app_name, environment);
