-- Deploy delivery_scope

BEGIN;

CREATE TYPE delivery_scope AS ENUM('enterprise', 'organization', 'project', 'pipeline');

COMMIT;
