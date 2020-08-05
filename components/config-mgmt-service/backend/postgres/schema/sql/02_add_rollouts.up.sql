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

CREATE INDEX IF NOT EXISTS policy_name_idx        ON rollouts (policy_name);
CREATE INDEX IF NOT EXISTS policy_node_group_idx  ON rollouts (policy_node_group);
CREATE INDEX IF NOT EXISTS policy_revision_id_idx ON rollouts (policy_revision_id);
CREATE INDEX IF NOT EXISTS policy_domain_url_idx  ON rollouts (policy_domain_url);


-- We call the policy name, policy node group, and policy domain URL the
-- "target node segment." Each target node segment has one (or none) rollout
-- for any given time, and we want to be able to find a past rollout by
-- querying for the target node segment and the (date)time.
-- To track the time when a rollout is/was active, we set the start_time when
-- we create a new one, and we set the end_time to NOW whenever the a rollout
-- is superseded.
CREATE OR REPLACE FUNCTION set_end_time_on_superseded()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
	UPDATE rollouts AS r
  SET end_time = NOW()
WHERE r.end_time IS NULL
  AND r.policy_name = NEW.policy_name
  AND r.policy_node_group = NEW.policy_node_group
	AND r.policy_domain_url = NEW.policy_domain_url;
  RETURN NEW;
END;
$$;

CREATE TRIGGER set_superseded_rollout_end_time BEFORE INSERT
ON rollouts FOR EACH ROW EXECUTE PROCEDURE
set_end_time_on_superseded();

-- Creating a new rollout with the same policy revision as the current rollout
-- for the given target node segment is not allowed; if the same nodes are
-- getting the same code, nothing is new or changed for us to track.
-- This restriction cannot be implemented via uniqueness constraint because a
-- user may push revision "A", then "B", then decide to go back to "A".
CREATE OR REPLACE FUNCTION disallow_redundant_rollout_insert()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  redundant_rollout_count integer;
BEGIN
  SELECT COUNT(*) INTO redundant_rollout_count
      FROM rollouts AS r
     WHERE r.end_time IS NULL
       AND r.policy_name = NEW.policy_name
       AND r.policy_node_group = NEW.policy_node_group
       AND r.policy_domain_url = NEW.policy_domain_url
       AND r.policy_revision_id = NEW.policy_revision_id;
  IF redundant_rollout_count != 0 THEN
    RAISE EXCEPTION 'current rollout exists for name=% group=% domain=% revision=%', NEW.policy_name, NEW.policy_node_group, NEW.policy_domain_url, NEW.policy_revision_id;
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER disallow_redundant_rollout_insert BEFORE INSERT
ON rollouts FOR EACH ROW EXECUTE PROCEDURE
disallow_redundant_rollout_insert();

