ALTER TABLE iam_stanzas RENAME TO iam_statements;

ALTER TABLE iam_policy_stanzas RENAME TO iam_policy_statements;

ALTER TABLE iam_policy_statements RENAME COLUMN stanza_id TO statement_id;

CREATE OR REPLACE FUNCTION
  query_policy(_policy_id UUID)
  RETURNS json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.description, p.type,
      (SELECT COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') FROM iam_policy_statements AS ps LEFT OUTER JOIN iam_statements
                AS s ON ps.statement_id=s.id WHERE ps.policy_id=p.id) AS statements,
      (SELECT COALESCE(json_agg(m) FILTER (WHERE m.id IS NOT NULL), '[]') FROM iam_policy_members AS pm LEFT OUTER JOIN iam_members
                AS m ON pm.member_id=m.id WHERE pm.policy_id=p.id) AS members
    FROM iam_policies as p
    WHERE p.id = _policy_id
    GROUP BY p.id)
  SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_policies()
  RETURNS setof json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.description, p.type,
      (SELECT COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') FROM iam_policy_statements AS ps LEFT OUTER JOIN iam_statements
                AS s ON ps.statement_id=s.id WHERE ps.policy_id=p.id) AS statements,
      (SELECT COALESCE(json_agg(m) FILTER (WHERE m.id IS NOT NULL), '[]') FROM iam_policy_members AS pm LEFT OUTER JOIN iam_members
                AS m ON pm.member_id=m.id WHERE pm.policy_id=p.id) AS members
    FROM iam_policies as p
    GROUP BY p.id)
  SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  insert_iam_statement_into_policy(_policy_id UUID, _statement_id UUID, _statement_effect iam_effect, _statement_actions TEXT[],
  _statement_resources TEXT[], _statement_role TEXT, _statement_scope TEXT)
  RETURNS void AS $$

    INSERT INTO iam_policy_statements (policy_id, statement_id)
      VALUES (_policy_id, _statement_id);

    INSERT INTO iam_statements (id, effect, actions, resources, role, scope)
      VALUES (_statement_id, _statement_effect, _statement_actions, _statement_resources, _statement_role, _statement_scope);

$$ LANGUAGE sql;