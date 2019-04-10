-- Deploy hash_type

BEGIN;

CREATE TYPE password_hash_type AS ENUM ('bcrypt');

COMMIT;
