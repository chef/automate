CREATE TABLE IF NOT EXISTS policy_rev (
  id                        TEXT    PRIMARY KEY,

  policy_name               TEXT    NOT NULL
);

CREATE INDEX IF NOT EXISTS policy_rev_id        ON policy_rev (id);


CREATE TABLE IF NOT EXISTS cookbook_lock (
  policy_id                 TEXT      PRIMARY KEY,
  policy_rev_id             TEXT      NOT NULL,
  cookbook_name             TEXT      NOT NULL
);

CREATE INDEX IF NOT EXISTS cook_lock_policy_rev_id     ON cookbook_lock (policy_rev_id);
