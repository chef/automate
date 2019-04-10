ALTER TABLE teams DROP COLUMN deletable;
DROP TRIGGER IF EXISTS only_allow_delete_on_deletable_teams ON teams;
DROP FUNCTION IF EXISTS cannot_delete_team_error(text);
