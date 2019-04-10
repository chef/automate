BEGIN;

-- Modifying a column type that is used as a pkey elsewhere is really complicated.
-- Much easier to just drop everything dependant and just re-add it.
-- Take advantage of the fact that no one is using this yet :)
DROP TABLE IF EXISTS
  iam_members,
  iam_policy_members,
  iam_policy_statements,
  iam_policies,
  iam_roles,
  iam_statement_roles, -- 24 before it was "cleaned up"
  iam_statements CASCADE;

DROP FUNCTION IF EXISTS insert_iam_policy (_id UUID, _name TEXT, _description TEXT, _type iam_policy_type);

DROP FUNCTION IF EXISTS query_policy (_policy_id UUID);

DROP FUNCTION IF EXISTS insert_iam_statement_into_policy(_policy_id UUID, _statement_id UUID, _statement_effect iam_effect, _statement_actions TEXT[],
  _statement_resources TEXT[], _statement_role TEXT, _statement_scope TEXT);

-- Removed description and changed id to TEXT from UUID.
-- Other table changes below are result of pkey changing to TEXT.
CREATE TABLE iam_policies (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  type iam_policy_type NOT NULL DEFAULT 'custom'
);

CREATE TABLE iam_members (
  id UUID PRIMARY KEY,
  name TEXT UNIQUE NOT NULL
);

CREATE TABLE iam_policy_members (
  policy_id TEXT REFERENCES iam_policies ON DELETE CASCADE,
  member_id UUID REFERENCES iam_members ON DELETE CASCADE,
  PRIMARY KEY (policy_id, member_id)
);

CREATE TABLE iam_policy_statements (
  policy_id TEXT REFERENCES iam_policies ON DELETE CASCADE,
  statement_id UUID UNIQUE,
  PRIMARY KEY (policy_id, statement_id)
);

CREATE TABLE iam_statements (
  id UUID REFERENCES iam_policy_statements(statement_id) ON DELETE CASCADE,
  effect iam_effect NOT NULL,
  actions TEXT[],
  resources TEXT[],
  role TEXT,
  scope TEXT
);

CREATE TABLE iam_roles (
  db_id SERIAL,
  id TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  type iam_policy_type NOT NULL DEFAULT 'custom',
  actions TEXT[]
);

-- Removed description from function.
CREATE OR REPLACE FUNCTION
  insert_iam_policy (_id TEXT, _name TEXT, _type iam_policy_type)
  RETURNS void AS $$

    INSERT INTO iam_policies (id, name, type)
      VALUES (_id, _name, _type);

$$ LANGUAGE sql;

-- Removed description from function and changed _policy_id to TEXT.
CREATE OR REPLACE FUNCTION
  query_policy(_policy_id TEXT)
  RETURNS json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type,
      (SELECT COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') FROM iam_policy_statements AS ps LEFT OUTER JOIN iam_statements
                AS s ON ps.statement_id=s.id WHERE ps.policy_id=p.id) AS statements,
      (SELECT COALESCE(json_agg(m) FILTER (WHERE m.id IS NOT NULL), '[]') FROM iam_policy_members AS pm LEFT OUTER JOIN iam_members
                AS m ON pm.member_id=m.id WHERE pm.policy_id=p.id) AS members
    FROM iam_policies as p
    WHERE p.id = _policy_id
    GROUP BY p.id)
  SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;

-- Removed description from function.
CREATE OR REPLACE FUNCTION
  query_policies()
  RETURNS setof json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.type,
      (SELECT COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') FROM iam_policy_statements AS ps LEFT OUTER JOIN iam_statements
                AS s ON ps.statement_id=s.id WHERE ps.policy_id=p.id) AS statements,
      (SELECT COALESCE(json_agg(m) FILTER (WHERE m.id IS NOT NULL), '[]') FROM iam_policy_members AS pm LEFT OUTER JOIN iam_members
                AS m ON pm.member_id=m.id WHERE pm.policy_id=p.id) AS members
    FROM iam_policies as p
    GROUP BY p.id)
  SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;

-- Simply changed policy_id to TEXT in this one.
CREATE OR REPLACE FUNCTION
  insert_iam_statement_into_policy(_policy_id TEXT, _statement_id UUID, _statement_effect iam_effect, _statement_actions TEXT[],
  _statement_resources TEXT[], _statement_role TEXT, _statement_scope TEXT)
  RETURNS void AS $$

    INSERT INTO iam_policy_statements (policy_id, statement_id)
      VALUES (_policy_id, _statement_id);

    INSERT INTO iam_statements (id, effect, actions, resources, role, scope)
      VALUES (_statement_id, _statement_effect, _statement_actions, _statement_resources, _statement_role, _statement_scope);

$$ LANGUAGE sql;

COMMIT;
