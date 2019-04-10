DROP FUNCTION insert_iam_policy_member (_policy_id UUID, _member_id UUID, _name TEXT);
 
CREATE TABLE iam_roles (
  db_id SERIAL,
  id TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  type iam_policy_type NOT NULL DEFAULT 'custom',
  actions TEXT[]
);

CREATE OR REPLACE FUNCTION
  query_role(_role_id TEXT)
  RETURNS json AS $$

  WITH t AS
    (SELECT r.id, r.name, r.type, r.actions
    FROM iam_roles r
    WHERE r.id = _role_id)
  SELECT row_to_json(t) AS role FROM t;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
  query_roles()
  RETURNS setof json AS $$

  WITH t AS
    (SELECT id, name, type, actions
    FROM iam_roles)
  SELECT row_to_json(t) AS role FROM t;

$$ LANGUAGE sql;

