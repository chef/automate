CREATE TABLE iam_policies (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  description TEXT NOT NULL,
  subjects TEXT[]
);

CREATE TYPE iam_effect AS ENUM ('allow', 'deny');

CREATE TABLE iam_policy_stanzas (
  policy_id UUID REFERENCES iam_policies ON DELETE CASCADE,
  stanza_id UUID UNIQUE,
  PRIMARY KEY (policy_id, stanza_id)
);

-- We are putting in empty arrays and empty strings
-- everywhere because that's the interface we decided
-- on for our API. Nice to not have NULLs anyway.
CREATE TABLE iam_stanzas (
  id UUID REFERENCES iam_policy_stanzas(stanza_id) ON DELETE CASCADE,
  effect iam_effect NOT NULL,
  actions TEXT[],
  resources TEXT[],
  role TEXT,
  scope TEXT
);

-- No DEFAULTS are used in either of the insert functions so no need to return anything other than error.
CREATE OR REPLACE FUNCTION
  insert_iam_policy (_id UUID, _name TEXT, _description TEXT, _subjects TEXT[])
  RETURNS void AS $$

    INSERT INTO iam_policies (id, name, description, subjects)
      VALUES (_id, _name, _description, _subjects);

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  insert_iam_stanza_into_policy(_policy_id UUID, _stanza_id UUID, _stanza_effect iam_effect, _stanza_actions TEXT[],
  _stanza_resources TEXT[], _stanza_role TEXT, _stanza_scope TEXT)
  RETURNS void AS $$

    INSERT INTO iam_policy_stanzas (policy_id, stanza_id)
      VALUES (_policy_id, _stanza_id);

    INSERT INTO iam_stanzas (id, effect, actions, resources, role, scope)
      VALUES (_stanza_id, _stanza_effect, _stanza_actions, _stanza_resources, _stanza_role, _stanza_scope);

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_policy(_policy_id UUID)
  RETURNS json AS $$

    WITH t AS
      (SELECT p.id, p.name, p.description, p.subjects,  COALESCE(json_agg(s) FILTER (WHERE s IS NOT NULL), '[]')AS stanzas
      FROM iam_policies as p
      LEFT OUTER JOIN iam_policy_stanzas AS ps ON ps.policy_id=p.id
      LEFT OUTER JOIN iam_stanzas AS s ON ps.stanza_id=s.id
      WHERE p.id = _policy_id
      GROUP BY p.id)
    SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_policies()
  RETURNS setof json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.description, p.subjects,  COALESCE(json_agg(s) FILTER (WHERE s IS NOT NULL), '[]')AS stanzas
    FROM iam_policies as p
    LEFT OUTER JOIN iam_policy_stanzas AS ps ON ps.policy_id=p.id
    LEFT OUTER JOIN iam_stanzas AS s ON ps.stanza_id=s.id
    GROUP BY p.id)
  SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;
