CREATE TYPE iam_policy_type AS ENUM ('custom', 'chef-managed');

ALTER TABLE iam_policies
  ADD COLUMN type iam_policy_type NOT NULL DEFAULT 'custom';

CREATE OR REPLACE FUNCTION
  insert_iam_policy (_id UUID, _name TEXT, _description TEXT, _type iam_policy_type, _subjects TEXT[])
  RETURNS void AS $$

    INSERT INTO iam_policies (id, name, description, type, subjects)
      VALUES (_id, _name, _description, _type, _subjects);

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_policy(_policy_id UUID)
  RETURNS json AS $$

    WITH t AS
      (SELECT p.id, p.name, p.description, p.subjects, p.type, COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') AS stanzas
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
    (SELECT p.id, p.name, p.description, p.subjects, p.type, COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') AS stanzas
    FROM iam_policies as p
    LEFT OUTER JOIN iam_policy_stanzas AS ps ON ps.policy_id=p.id
    LEFT OUTER JOIN iam_stanzas AS s ON ps.stanza_id=s.id
    GROUP BY p.id)
  SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;
