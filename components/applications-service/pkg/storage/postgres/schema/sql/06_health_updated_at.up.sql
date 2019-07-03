-- Add health_updated_at column to the service table, the timewizard,
-- it will store the time when the health of a service changed.
--
-- NOTE @afiune this field will be updated from our ingester go package
ALTER TABLE service ADD COLUMN health_updated_at TIMESTAMP NOT NULL DEFAULT NOW();
