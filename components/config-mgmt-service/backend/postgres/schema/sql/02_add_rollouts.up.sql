CREATE TABLE IF NOT EXISTS rollouts (
  id                        SERIAL    PRIMARY KEY,

  policy_name               TEXT      NOT NULL,
  policy_node_group         TEXT      NOT NULL,
  policy_revision_id        TEXT      NOT NULL,
  policy_domain_url         TEXT      NOT NULL,
  scm_type                  TEXT      NOT NULL DEFAULT '',
  scm_web_type              TEXT      NOT NULL DEFAULT '',
  policy_scm_url            TEXT      NOT NULL DEFAULT '',
  policy_scm_web_url        TEXT      NOT NULL DEFAULT '',
  policy_scm_commit         TEXT      NOT NULL DEFAULT '',
  description               TEXT      NOT NULL DEFAULT '',
  ci_job_url                TEXT      NOT NULL DEFAULT '',
  ci_job_id                 TEXT      NOT NULL DEFAULT '',

  start_time                TIMESTAMP NOT NULL DEFAULT NOW(),
  end_time                  TIMESTAMP
);

CREATE INDEX policy_name_idx        ON rollouts (policy_name);
CREATE INDEX policy_node_group_idx  ON rollouts (policy_node_group);
CREATE INDEX policy_revision_id_idx ON rollouts (policy_revision_id);
CREATE INDEX policy_domain_url_idx  ON rollouts (policy_domain_url);
  
