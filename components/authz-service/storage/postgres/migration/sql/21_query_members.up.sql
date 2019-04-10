CREATE TABLE iam_policy_members (
  policy_id UUID REFERENCES iam_policies ON DELETE CASCADE,
  member_id UUID UNIQUE,
  PRIMARY KEY (policy_id, member_id)
);

CREATE TABLE iam_members (
  id UUID REFERENCES iam_policy_members(member_id) ON DELETE CASCADE,
  name TEXT NOT NULL
);

ALTER TABLE iam_policies
  DROP COLUMN subjects;

DROP FUNCTION insert_iam_policy (_id UUID, _name TEXT, _description TEXT, _type iam_policy_type, _subjects TEXT[]);

DROP FUNCTION insert_iam_policy (_id UUID, _name TEXT, _description TEXT, _subjects TEXT[]);

CREATE OR REPLACE FUNCTION
  insert_iam_policy (_id UUID, _name TEXT, _description TEXT, _type iam_policy_type)
  RETURNS void AS $$

    INSERT INTO iam_policies (id, name, description, type)
      VALUES (_id, _name, _description, _type);

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  insert_iam_policy_member (_policy_id UUID, _member_id UUID, _name TEXT)
  RETURNS void AS $$
    INSERT INTO iam_policy_members (policy_id, member_id)
      values(_policy_id, _member_id);

    INSERT INTO iam_members (id, name)
      values(_member_id, _name);
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_policy(_policy_id UUID)
  RETURNS json AS $$

  WITH t AS
    (SELECT p.id, p.name, p.description, p.type,
      (SELECT COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') FROM iam_policy_stanzas AS ps LEFT OUTER JOIN iam_stanzas 
                AS s ON ps.stanza_id=s.id WHERE ps.policy_id=p.id) AS stanzas,
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
      (SELECT COALESCE(json_agg(s) FILTER (WHERE s.id IS NOT NULL), '[]') FROM iam_policy_stanzas AS ps LEFT OUTER JOIN iam_stanzas 
                AS s ON ps.stanza_id=s.id WHERE ps.policy_id=p.id) AS stanzas,
      (SELECT COALESCE(json_agg(m) FILTER (WHERE m.id IS NOT NULL), '[]') FROM iam_policy_members AS pm LEFT OUTER JOIN iam_members
                AS m ON pm.member_id=m.id WHERE pm.policy_id=p.id) AS members
    FROM iam_policies as p
    GROUP BY p.id)
  SELECT row_to_json(t) AS policy FROM t;

$$ LANGUAGE sql;

