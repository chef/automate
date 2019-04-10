-- Verify delivery:team_members on pg

BEGIN;

  SELECT team_id,
         user_id
  FROM team_members WHERE FALSE;

ROLLBACK;
