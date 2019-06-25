-- This migration will add '{"default"}' project in everywhere, as old tokens need
-- to start with a project.
ALTER TABLE chef_authn_tokens ADD COLUMN project_ids TEXT[] NOT NULL DEFAULT '{default}' CHECK (project_ids <> '{}');
