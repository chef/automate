BEGIN;

-- Remove scope from statement
ALTER TABLE iam_statements DROP scope;

-- Add projects to statements with reference to iam_projects
ALTER TABLE iam_statements ADD PRIMARY KEY (id);

CREATE TABLE iam_statement_projects (
  statement_id UUID REFERENCES iam_statements ON DELETE CASCADE,
  project_id TEXT REFERENCES iam_projects ON DELETE CASCADE,
  PRIMARY KEY (statement_id, project_id)
);

DROP FUNCTION IF EXISTS insert_iam_statement_into_policy(_policy_id TEXT, _statement_id UUID, _statement_effect iam_effect, _statement_actions TEXT[],
  _statement_resources TEXT[], _statement_role TEXT, _statement_scope TEXT);

CREATE OR REPLACE FUNCTION
  insert_iam_statement_into_policy(_policy_id TEXT, _statement_id UUID, _statement_effect iam_effect, _statement_actions TEXT[],
  _statement_resources TEXT[], _statement_role TEXT, _statement_projects TEXT[])
  RETURNS void AS $$

    INSERT INTO iam_policy_statements (policy_id, statement_id)
      VALUES (_policy_id, _statement_id);

    INSERT INTO iam_statements (id, effect, actions, resources, role)
      VALUES (_statement_id, _statement_effect, _statement_actions, _statement_resources, _statement_role);
    
    INSERT INTO iam_statement_projects (statement_id, project_id) 
    SELECT _statement_id s_id, p_id
    FROM UNNEST(_statement_projects) p_id ON CONFLICT DO NOTHING

$$ LANGUAGE sql;

-- update query_policy(ies) so it returns each policy statement with projects
DROP FUNCTION IF EXISTS query_policy (_policy_id TEXT);

CREATE OR REPLACE FUNCTION
  query_policy(_policy_id TEXT)
  RETURNS json AS $$

  WITH temp AS
    (SELECT pol.id, pol.name, pol.type,
      -- get policy's statements using temporary table
      (WITH statement_rows AS
        (SELECT stmt.id, stmt.effect, stmt.actions, stmt.resources, stmt.role,
          -- get each statement's projects by cross-referencing iam_policy_statements and iam_statement_projects
          (SELECT          COALESCE(Json_agg(proj.id) filter (WHERE proj.id IS NOT NULL), '[]')
          FROM            iam_statement_projects AS stmt_projs
          LEFT OUTER JOIN iam_projects           AS proj
          ON              stmt_projs.statement_id=stmt.id
          WHERE           stmt_projs.project_id=proj.id) AS projects
      FROM iam_policy_statements AS pol_stmts LEFT OUTER JOIN iam_statements AS stmt 
      ON pol_stmts.statement_id=stmt.id WHERE pol_stmts.policy_id=pol.id
      GROUP BY stmt.id)
      SELECT COALESCE(json_agg(statement_rows) FILTER (WHERE statement_rows.id IS NOT NULL), '[]')
        AS statements FROM statement_rows),
      -- get policy members
      (SELECT COALESCE(json_agg(mem) FILTER (WHERE mem.id IS NOT NULL), '[]')
        FROM iam_policy_members AS pol_mems LEFT OUTER JOIN iam_members AS mem
        ON pol_mems.member_id=mem.id WHERE pol_mems.policy_id=pol.id) AS members

    FROM iam_policies as pol
    WHERE pol.id = _policy_id
    GROUP BY pol.id)
    SELECT row_to_json(temp) AS policy FROM temp;
$$ LANGUAGE sql;

DROP FUNCTION IF EXISTS query_policies ();

CREATE OR REPLACE FUNCTION
  query_policies()
  RETURNS setof json AS $$

  WITH temp AS
    (SELECT pol.id, pol.name, pol.type,
      (WITH statement_rows AS
        (SELECT stmt.id, stmt.effect, stmt.actions, stmt.resources, stmt.role,
          (SELECT           COALESCE(json_agg(proj.id) FILTER (WHERE proj.id IS NOT NULL), '[]')
            FROM            iam_statement_projects AS stmt_projs 
            LEFT OUTER JOIN iam_projects AS proj
            ON              stmt_projs.statement_id=stmt.id 
            WHERE           stmt_projs.project_id=proj.id) AS projects
      FROM iam_policy_statements AS pol_stmts LEFT OUTER JOIN iam_statements AS stmt 
      ON pol_stmts.statement_id=stmt.id WHERE pol_stmts.policy_id=pol.id
      GROUP BY stmt.id)
      SELECT COALESCE(json_agg(statement_rows) FILTER (WHERE statement_rows.id IS NOT NULL), '[]')
        AS statements FROM statement_rows),
      (SELECT COALESCE(json_agg(mem) FILTER (WHERE mem.id IS NOT NULL), '[]')
        FROM iam_policy_members AS pol_mems LEFT OUTER JOIN iam_members AS mem
        ON pol_mems.member_id=mem.id WHERE pol_mems.policy_id=pol.id) AS members

    FROM iam_policies as pol
    GROUP BY pol.id)
    SELECT row_to_json(temp) AS policy FROM temp;
$$ LANGUAGE sql;

COMMIT;
