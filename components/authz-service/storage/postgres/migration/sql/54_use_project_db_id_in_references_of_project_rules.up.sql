BEGIN;
-- break all references
ALTER TABLE iam_project_rules
    DROP CONSTRAINT iam_project_rules_project_id_fkey;
ALTER TABLE iam_staged_project_rules
    DROP CONSTRAINT iam_staged_project_rules_project_id_fkey;

-- When we're done fixing all references, we'll change the
-- primary key of iam_projects; now, we only add a uniqueness
-- constraint on what's going to be the new PK: db_id.
ALTER TABLE iam_projects
    ADD UNIQUE (db_id);

-- redo references (welcome to copy-paste hell)
ALTER TABLE iam_project_rules RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_project_rules
    ADD COLUMN project_id INTEGER REFERENCES iam_projects (db_id) ON DELETE CASCADE DEFERRABLE;
UPDATE
    iam_project_rules t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_project_rules
    DROP COLUMN project_temp_id;

ALTER TABLE iam_staged_project_rules RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_staged_project_rules
    ADD COLUMN project_id INTEGER REFERENCES iam_projects (db_id) ON DELETE CASCADE DEFERRABLE;
UPDATE
    iam_staged_project_rules t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_staged_project_rules
    DROP COLUMN project_temp_id;

-- fix functions
CREATE OR REPLACE FUNCTION
  query_rules_for_project(_project_id TEXT, _project_filter TEXT[])
  RETURNS setof json AS $$

  -- fetch staged rules
   SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc 
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE _project_id=p.id AND projects_match_for_rule(p.id, _project_filter) AND r.deleted=false
  GROUP BY r.id, p.id, r.name, r.type

  UNION ALL

  -- fetch applied rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE _project_id=p.id AND projects_match_for_rule(p.id, _project_filter)
    -- return applied rule only if no staged changes exist
    AND NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=r.id)
  GROUP BY r.id, p.id, r.name, r.type

 $$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_staged_and_applied_rules(_project_filter TEXT[])
  RETURNS setof json AS $$

  -- fetch staged rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'deleted', r.deleted,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rules
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type, r.deleted

  UNION ALL

  -- fetch applied rules
  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'deleted', false,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rules
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_rules(_project_filter TEXT[])
  RETURNS setof json AS $$

  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_staged_or_applied_rule(_rule_id TEXT, _project_filter TEXT[])
  RETURNS json AS $$

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'deleted', r.deleted,
    'status', 'staged',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_staged_project_rules AS r
  INNER JOIN iam_staged_rule_conditions AS rc
  ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE r.id=_rule_id AND projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type, r.deleted

  UNION ALL

  -- check for applied rule
  SELECT json_build_object(
    'id', r.id,
    'project_id', p.id,
    'name', r.name,
    'type', r.type,
    'deleted', false,
    'status', 'applied',
    'conditions', json_agg(
      json_build_object(
        'value', rc.value,
        'operator', rc.operator,
        'attribute', rc.attribute
      )
    )
  ) AS rule
  FROM iam_project_rules AS r
  INNER JOIN iam_rule_conditions AS rc
   ON rc.rule_db_id=r.db_id
  INNER JOIN iam_projects AS p
  ON r.project_id=p.db_id
  WHERE r.id=_rule_id AND projects_match_for_rule(p.id, _project_filter)
  GROUP BY r.id, p.id, r.name, r.type;

$$ LANGUAGE sql;

-- NEW: it takes a project's db_id as first argument
-- This way, we can keep some of the other functions unchanged.
CREATE OR REPLACE FUNCTION
  projects_match_for_rule(_project_db_id INTEGER, _projects_filter TEXT[])
  RETURNS BOOLEAN AS $$
    SELECT array_length(_projects_filter, 1) IS NULL
    OR EXISTS (SELECT 1 FROM iam_projects WHERE db_id=_project_db_id AND id=ANY(_projects_filter))
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION update_rule(_in_rule_id text, _in_project_id text, _in_name text, _in_type text, projects_filter TEXT[])
RETURNS TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  rule_db_id INTEGER;
BEGIN
    IF (NOT EXISTS (SELECT 1 FROM iam_project_rules WHERE id=_in_rule_id AND projects_match_for_rule(project_id, projects_filter))) AND
        (NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=_in_rule_id AND projects_match_for_rule(project_id, projects_filter))) THEN
            RAISE EXCEPTION 'not found %', _in_rule_id
            USING ERRCODE='case_not_found';
    ELSIF (NOT EXISTS (SELECT 1 FROM iam_project_rules WHERE id=_in_rule_id AND project_id=project_db_id(_in_project_id))) AND
        (NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=_in_rule_id AND project_id=project_db_id(_in_project_id))) THEN
            RAISE EXCEPTION 'incoming project does not match rule project %', _in_rule_id
            USING ERRCODE='PRJTR';
    ELSIF (NOT EXISTS (SELECT 1 FROM iam_project_rules WHERE id=_in_rule_id AND type=_in_type)) AND
        (NOT EXISTS (SELECT 1 FROM iam_staged_project_rules WHERE id=_in_rule_id AND type=_in_type)) THEN
            RAISE EXCEPTION 'incoming type does not match rule type %', _in_rule_id
            USING ERRCODE='RLTYP';
    ELSE
        INSERT INTO iam_staged_project_rules as ispr (id, project_id, name, type, deleted)
        VALUES (_in_rule_id, project_db_id(_in_project_id), _in_name, _in_type, false)
        ON CONFLICT (id) DO -- applied rule has already been updated, so update the staged version of it
            UPDATE SET
                id          = _in_rule_id,
                project_id  = project_db_id(_in_project_id),
                name        = _in_name,
                type        = _in_type
            WHERE ispr.id = _in_rule_id AND projects_match_for_rule(ispr.project_id, projects_filter)
        RETURNING ispr.db_id INTO rule_db_id;
    RETURN rule_db_id;
    END IF;
END;
$$;

-- helper functions
CREATE FUNCTION project_db_id (_id TEXT)
    RETURNS INTEGER
    AS $$
    SELECT
        db_id
    FROM
        iam_projects
    WHERE
        id = _id;
$$
LANGUAGE SQL;

CREATE FUNCTION project_id (_db_id INTEGER)
    RETURNS TEXT
    AS $$
    SELECT
        id
    FROM
        iam_projects
    WHERE
        db_id = _db_id;
$$
LANGUAGE SQL;
 
COMMIT;
