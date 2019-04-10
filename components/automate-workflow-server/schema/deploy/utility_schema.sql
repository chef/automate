-- Deploy utility_schema

BEGIN;

CREATE SCHEMA utility;

COMMENT ON SCHEMA utility IS

$$Various utility procedures, views, etc. will live here. These are
not required for the proper functioning of the system, but can be
useful for troubleshooting, debugging, and performing other kinds of
maintenance.

Expert use only!$$;

COMMIT;
