
BEGIN;

CREATE OR REPLACE FUNCTION update_rule(in_rule_id text, in_project_id text, in_name text, in_type text, in_deleted boolean, projects_filter TEXT[])
RETURNS TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  rule_db_id INTEGER;
BEGIN
    IF (EXISTS (SELECT id FROM iam_project_rules ipr WHERE id=in_rule_id AND projects_match_for_rule(ipr.project_id, projects_filter))) THEN -- cannot 'update' a non-existed applied rule
            INSERT INTO iam_staged_project_rules as ispr (id, project_id, name, type, deleted)
            VALUES (in_rule_id, in_project_id, in_name, in_type, false)
            ON CONFLICT (id) -- applied rule has already been updated, so update the staged version of it
                DO UPDATE
                        SET
                        id = in_rule_id,
                        project_id = in_project_id,
                        name          = in_name,
                        type   = in_type,
                        deleted    = in_deleted
            WHERE ispr.id = in_rule_id AND projects_match_for_rule(ispr.project_id, projects_filter)
        RETURNING ispr.db_id INTO rule_db_id;
        RETURN rule_db_id;
    -- TODO: add conditions
    ELSE
        RAISE EXCEPTION 'noooooot found %', in_rule_id USING -- do we need this? maybe we can just return null or something?
        ERRCODE='NOAPPLIEDRULE';
    END IF;
    RETURN rule_db_id;
END;
$$;

CREATE OR REPLACE FUNCTION fn_project_id_check() RETURNS TRIGGER AS $$
BEGIN 
    RAISE EXCEPTION 'cannot change project_id: % -> %', OLD.project_id, NEW.project_id USING
        ERRCODE='PROJID';
END$$ LANGUAGE plpgsql;

-- cannot update project it
CREATE TRIGGER verify_project_id BEFORE UPDATE OF project_id ON iam_staged_project_rules
FOR EACH ROW
WHEN (OLD.project_id != NEW.project_id) 
EXECUTE PROCEDURE fn_project_id_check();

CREATE OR REPLACE FUNCTION fn_deleted_rule_check() RETURNS TRIGGER AS $$
BEGIN 
    RAISE EXCEPTION 'rule with id % has been deleted', NEW.id USING
        ERRCODE='RULEDELETED'; 
END$$ LANGUAGE plpgsql;

-- cannot update a rule that is staged for deletion 
CREATE TRIGGER verify_not_deleted BEFORE UPDATE ON iam_staged_project_rules
FOR EACH ROW
WHEN (OLD.deleted = true)
EXECUTE PROCEDURE fn_deleted_rule_check();

COMMIT;
