-- Revert extend_user_type

BEGIN;

-- types cannot be altered unless all values of the type to be dropped are deleted.
-- in this case we would have to delete all users of type a2 before proceeding.
-- ALTER TYPE user_type RENAME TO temp_user_type;
-- CREATE TYPE user_type AS ENUM ('internal', 'external');
-- DROP TYPE temp_user_type;
DROP TYPE user_type;

COMMIT;
