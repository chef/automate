BEGIN;

CREATE OR REPLACE FUNCTION update_rule(_in_rule_id text, _in_project_id text, _in_name text, _in_type text, _in_deleted boolean, projects_filter TEXT[])
RETURNS TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  rule_db_id INTEGER;
BEGIN
    IF (NOT EXISTS (SELECT id FROM iam_project_rules WHERE id=_in_rule_id AND projects_match_for_rule(project_id, projects_filter))) AND
        (NOT EXISTS (SELECT id FROM iam_staged_project_rules WHERE id=_in_rule_id AND projects_match_for_rule(project_id, projects_filter))) THEN
            RAISE EXCEPTION 'not found %', _in_rule_id
            USING ERRCODE='case_not_found';
    ELSIF (NOT EXISTS (SELECT id, project_id FROM iam_project_rules WHERE id=_in_rule_id AND project_id=_in_project_id)) AND
        (NOT EXISTS (SELECT id, project_id FROM iam_staged_project_rules WHERE id=_in_rule_id AND project_id=_in_project_id)) THEN
            RAISE EXCEPTION 'incoming project does not match rule project %', _in_rule_id
            USING ERRCODE='PRJTR';
    ELSE
        INSERT INTO iam_staged_project_rules as ispr (id, project_id, name, type, deleted)
        VALUES (_in_rule_id, _in_project_id, _in_name, _in_type, false)
        ON CONFLICT (id) DO-- applied rule has already been updated, so update the staged version of it
            UPDATE SET
                id          = _in_rule_id,
                project_id  = _in_project_id,
                name        = _in_name,
                type        = _in_type,
                deleted     = _in_deleted
            WHERE ispr.id = _in_rule_id AND projects_match_for_rule(ispr.project_id, projects_filter)
        RETURNING ispr.db_id INTO rule_db_id;
    RETURN rule_db_id;
    END IF;
END;
$$;

CREATE OR REPLACE FUNCTION fn_deleted_rule_check() RETURNS TRIGGER AS $$
BEGIN 
    RAISE EXCEPTION 'rule with id % has been deleted', NEW.id USING
        ERRCODE='RDLTD'; 
END$$ LANGUAGE plpgsql;

-- cannot update a rule that is staged for deletion 
CREATE TRIGGER verify_not_deleted BEFORE UPDATE ON iam_staged_project_rules
FOR EACH ROW
WHEN (OLD.deleted = true)
EXECUTE PROCEDURE fn_deleted_rule_check();

COMMIT;
