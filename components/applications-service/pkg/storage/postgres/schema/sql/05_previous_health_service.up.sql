-- Add previous_health column with the default HealthStatus 'NONE' that matches
-- with the protobuf definition => api/external/applications/applications.proto
ALTER TABLE service ADD COLUMN previous_health TEXT NOT NULL DEFAULT 'NONE';

-- (fix) drop the default value for the package_ident column
-- @afiune: we do not want to allow this field to be empty
ALTER TABLE service ALTER COLUMN package_ident DROP DEFAULT;
