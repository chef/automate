-- Deploy update_project_guid
-- requires: projects

BEGIN;

CREATE OR REPLACE FUNCTION update_project_guid()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.guid != OLD.guid THEN
    -- That field can never change!
    NEW.guid = OLD.guid;
  ELSIF TG_OP = 'INSERT' THEN
    -- Only create a new GUID if it's a new project
    NEW.guid = uuid_generate_v4();
  END IF;
  RETURN NEW;
END;
$$;

COMMIT;
