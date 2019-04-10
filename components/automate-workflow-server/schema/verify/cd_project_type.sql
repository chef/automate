-- Verify delivery:cd_project_type on pg

BEGIN;

SELECT 'local'::cd_project_type;

ROLLBACK;
