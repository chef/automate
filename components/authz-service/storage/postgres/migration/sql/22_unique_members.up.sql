DROP TABLE iam_members;

DROP TABLE iam_policy_members;

CREATE TABLE iam_members (
  id UUID PRIMARY KEY,
  name TEXT UNIQUE NOT NULL
);

CREATE TABLE iam_policy_members (
  policy_id UUID REFERENCES iam_policies ON DELETE CASCADE,
  member_id UUID REFERENCES iam_members ON DELETE CASCADE,
  PRIMARY KEY (policy_id, member_id)
);
