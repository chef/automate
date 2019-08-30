ALTER TABLE service_full ADD COLUMN disconnected BOOL NOT NULL DEFAULT false;
CREATE INDEX service_full_disconnected_idx ON service_full (disconnected);
