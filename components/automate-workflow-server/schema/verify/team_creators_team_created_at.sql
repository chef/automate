-- Verify team_creators_team_created_at

BEGIN;

SELECT creator_id,
       updater_id,
       created_at,
       updated_at
FROM teams WHERE FALSE;

ROLLBACK;
