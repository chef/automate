-- Change the logic for how we detect conflicting rollout rows.
--
-- In our model, each rollout represents a code push to some set of nodes.
-- The most recent one is still ongoing or "current." A rollout that targets
-- the same set of nodes with the same code version as the current rollout is
-- redundant and it should be some kind of conflict error if you try to insert
-- one. Note that we need to support rollbacks, i.e., the user can push version
-- A, then B, then A again, then B again, etc. This makes it a little harder to
-- model redundant rollout conflicts in SQL (plain uniqueness constraint won't
-- work).
-- Prior to this migration, we used a trigger function that raises an exception
-- when trying to insert a redundant rollout record.. This approach works fine,
-- but we cannot use upsert with it, and now we need upserts. So we make
-- current_rollout a column in the database which lets us use an uniqueness
-- constraint to catch duplicate current rollouts.

-- Remove the old way of preventing redundant rollout records (see 02_add_rollouts.up.sql)
-- A new trigger will handle the requirement to set the end time on ending/superseded rollouts.
DROP TRIGGER IF EXISTS disallow_redundant_rollout_insert ON rollouts;
DROP TRIGGER IF EXISTS set_superseded_rollout_end_time ON rollouts;
DROP FUNCTION IF EXISTS disallow_redundant_rollout_insert();
DROP FUNCTION IF EXISTS set_end_time_on_superseded();

-- We use NULLs in this column to make the uniqueness constraint behave the way
-- we need it to. Because NULL is NOT equal to NULL for the purposes of the
-- constraint, this lets us have many columns with the same policy_name,
-- policy_node_group, policy_domain_url, and policy_revision_id, as long as
-- they all have current_rollout == NULL. Thus we can model our requirement
-- that CURRENT rollouts must be unique, but old ones do not have to be unique.
ALTER TABLE rollouts ADD COLUMN IF NOT EXISTS current_rollout BOOLEAN DEFAULT TRUE;
ALTER TABLE rollouts DROP CONSTRAINT IF EXISTS current_rollout_unique;
ALTER TABLE rollouts ADD CONSTRAINT current_rollout_unique UNIQUE (policy_name, policy_node_group, policy_domain_url, policy_revision_id, current_rollout);

-- This function is added in 02. The prior version set `end_time` to `NOW()`,
CREATE OR REPLACE FUNCTION current_rollout_ended()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
	UPDATE rollouts AS r
  SET end_time = NOW(),
      current_rollout = NULL
WHERE r.end_time IS NULL
  AND r.policy_name = NEW.policy_name
  AND r.policy_node_group = NEW.policy_node_group
	AND r.policy_domain_url = NEW.policy_domain_url
  -- exclude rows where the policy_revision_id is the same; this prevents us
  -- from changing rows that are supposed to hit the current_rollout_unique
  -- constraint
  AND r.policy_revision_id != NEW.policy_revision_id
  AND r.id != NEW.id;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS set_current_rollout ON rollouts;
CREATE TRIGGER set_current_rollout AFTER INSERT
ON rollouts FOR EACH ROW EXECUTE PROCEDURE
current_rollout_ended();


