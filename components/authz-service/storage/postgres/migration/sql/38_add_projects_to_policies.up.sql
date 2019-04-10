CREATE TABLE iam_policy_projects (
  policy_id TEXT REFERENCES iam_policies ON DELETE CASCADE,
  project_id TEXT REFERENCES iam_projects ON DELETE CASCADE,
  PRIMARY KEY (policy_id, project_id)
);

CREATE OR REPLACE FUNCTION
  query_policy(_policy_id TEXT)
  RETURNS json AS $$

  WITH temp AS (
    SELECT
      pol.id,
      pol.name,
      pol.type,
      -- get policy's statements using temporary table
      ( WITH statement_rows AS (
          SELECT
            stmt.id,
            stmt.effect,
            stmt.actions,
            stmt.resources,
            stmt.role,
            -- get each statement's projects by cross-referencing iam_policy_statements and iam_statement_projects
            (
              SELECT COALESCE(Json_agg(proj.id) FILTER ( WHERE proj.id IS NOT NULL), '[]')
              FROM iam_statement_projects AS stmt_projs
              LEFT OUTER JOIN iam_projects AS proj ON stmt_projs.statement_id = stmt.id
              WHERE stmt_projs.project_id = proj.id
            ) AS projects
          FROM iam_policy_statements AS pol_stmts
          LEFT OUTER JOIN iam_statements AS stmt ON pol_stmts.statement_id = stmt.id
          WHERE pol_stmts.policy_id = pol.id
          GROUP BY stmt.id
        )
        SELECT COALESCE(json_agg(statement_rows) FILTER ( WHERE statement_rows.id IS NOT NULL), '[]')
        FROM statement_rows
      ) AS statements,
      -- get policy members
      ( SELECT COALESCE(json_agg(mem) FILTER ( WHERE mem.id IS NOT NULL), '[]')
        FROM iam_policy_members AS pol_mems
        LEFT OUTER JOIN iam_members AS mem ON pol_mems.member_id = mem.id
        WHERE pol_mems.policy_id = pol.id
      ) AS members,
      -- get projects
      ( SELECT COALESCE(json_agg(proj.id) FILTER ( WHERE proj.id IS NOT NULL), '[]')
        FROM iam_policy_projects AS pol_projs
        LEFT OUTER JOIN iam_projects AS proj ON pol_projs.project_id = proj.id
        WHERE pol_projs.policy_id = pol.id
      ) AS projects
    FROM iam_policies as pol
    WHERE pol.id = _policy_id
    GROUP BY pol.id
  )
  SELECT row_to_json(temp) AS policy
  FROM temp;

$$ LANGUAGE sql;
