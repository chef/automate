BEGIN;
-- break all references
ALTER TABLE iam_project_rules
    DROP CONSTRAINT iam_project_rules_project_id_fkey;
ALTER TABLE iam_staged_project_rules
    DROP CONSTRAINT iam_staged_project_rules_project_id_fkey;
ALTER TABLE iam_role_projects
    DROP CONSTRAINT iam_role_projects_project_id_fkey;
ALTER TABLE iam_policy_projects
    DROP CONSTRAINT iam_policy_projects_project_id_fkey;
ALTER TABLE iam_statement_projects
    DROP CONSTRAINT iam_statement_projects_project_id_fkey;

-- change primary key of iam_projects
ALTER TABLE iam_projects
    DROP CONSTRAINT iam_projects_pkey;
ALTER TABLE iam_projects
    ADD PRIMARY KEY (db_id);
ALTER TABLE iam_projects
    ADD UNIQUE (id);

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

ALTER TABLE iam_role_projects RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_role_projects
    ADD COLUMN project_id INTEGER REFERENCES iam_projects (db_id) ON DELETE CASCADE DEFERRABLE;
UPDATE
    iam_role_projects t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_role_projects
    DROP COLUMN project_temp_id;

ALTER TABLE iam_policy_projects RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_policy_projects
    ADD COLUMN project_id INTEGER REFERENCES iam_projects (db_id) ON DELETE CASCADE DEFERRABLE;
UPDATE
    iam_policy_projects t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_policy_projects
    DROP COLUMN project_temp_id;

ALTER TABLE iam_statement_projects RENAME COLUMN project_id TO project_temp_id;
ALTER TABLE iam_statement_projects
    ADD COLUMN project_id INTEGER REFERENCES iam_projects (db_id) ON DELETE CASCADE DEFERRABLE;
UPDATE
    iam_statement_projects t
SET
    project_id = (
        SELECT
            db_id
        FROM
            iam_projects
        WHERE
            id = t.project_temp_id);
ALTER TABLE iam_statement_projects
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
 
COMMIT;
