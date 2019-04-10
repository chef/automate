-- Deploy project_guid_trigger
-- requires: update_project_guid

BEGIN;

CREATE TRIGGER update_project_guid
BEFORE INSERT OR UPDATE OF guid
ON projects
FOR EACH ROW
EXECUTE PROCEDURE update_project_guid();

COMMIT;
