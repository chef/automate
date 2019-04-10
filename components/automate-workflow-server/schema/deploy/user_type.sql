-- Deploy user_type

BEGIN;

CREATE TYPE user_type AS ENUM ('internal', 'external');

COMMIT;
