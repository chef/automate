ALTER TABLE service_full ADD COLUMN service_group_id text;
CREATE INDEX service_full_service_group_id ON service_full (service_group_id);

