-- Insert the default project for existing teams but then do not allow empty project
-- arrays to be inserted. User must always specify projects to be inserted post-migration.
ALTER TABLE teams ADD COLUMN projects TEXT[] NOT NULL DEFAULT '{default}' CHECK (projects <> '{}');
ALTER TABLE teams ALTER COLUMN projects DROP DEFAULT;
