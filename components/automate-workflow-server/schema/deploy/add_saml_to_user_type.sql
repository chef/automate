-- Deploy delivery:add_saml_to_user_type to pg
-- This migration will remove a view that is no longer used by references
-- the enum user_type which needs a new entry. Removing the view allows for
-- a cleaner migration path.

ALTER TYPE user_type ADD VALUE 'saml';

BEGIN;

  DROP VIEW IF EXISTS user_aliases;

COMMIT;
