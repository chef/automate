-- Add last_event_occurred_at column to service table, this will be the timestamp of last
-- event received from habitat, the actual timestamp is sent from the habitat supervisor
ALTER TABLE service       ADD COLUMN last_event_occurred_at TIMESTAMP NOT NULL DEFAULT NOW();

-- Add created_at column to all tables in database
ALTER TABLE deployment    ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT NOW();
ALTER TABLE service_group ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT NOW();
ALTER TABLE supervisor    ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT NOW();
ALTER TABLE service       ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT NOW();

-- Add updated_at column to all tables in database
ALTER TABLE deployment    ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT NOW();
ALTER TABLE service_group ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT NOW();
ALTER TABLE supervisor    ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT NOW();
ALTER TABLE service       ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT NOW();

-- Add function to update timestamp of updated_at columns
CREATE OR REPLACE FUNCTION update_timestamp_updated_at_column()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$;

-- Add triggers to all updated_at columns in database
CREATE TRIGGER update_deployment_updated_at BEFORE UPDATE
ON deployment FOR EACH ROW EXECUTE PROCEDURE
update_timestamp_updated_at_column();

CREATE TRIGGER update_service_group_updated_at BEFORE UPDATE
ON service_group FOR EACH ROW EXECUTE PROCEDURE
update_timestamp_updated_at_column();

CREATE TRIGGER update_spervisor_updated_at BEFORE UPDATE
ON supervisor FOR EACH ROW EXECUTE PROCEDURE
update_timestamp_updated_at_column();

CREATE TRIGGER update_service_updated_at BEFORE UPDATE
ON service FOR EACH ROW EXECUTE PROCEDURE
update_timestamp_updated_at_column();
