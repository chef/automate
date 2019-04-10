-- Deploy extend_user_type

-- Alter type add cannot run inside a transaction block
--BEGIN;

ALTER TYPE user_type ADD VALUE 'a2' AFTER 'external';

--COMMIT;
