-- This migration will remove adding '{"default"}' in, as we don't assume you want to add to the
-- default project. If someone tries to add a token without a project, they should fail based
-- on the above constraint. We have validation in the domain to prevent that from ever happening though.
ALTER TABLE chef_authn_tokens ALTER COLUMN project_ids DROP DEFAULT;
