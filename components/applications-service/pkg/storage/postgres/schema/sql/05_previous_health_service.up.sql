-- Add previous_health column
ALTER TABLE service ADD COLUMN previous_health TEXT NOT NULL DEFAULT '';

-- (fix) drop the default value for the package_ident column
-- @afiune: we do not want to allow this field to be empty
ALTER TABLE service ALTER COLUMN package_ident DROP DEFAULT;
