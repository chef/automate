ALTER TABLE orgs ADD COLUMN projects TEXT[] NOT NULL CHECK (projects <> '{}');
