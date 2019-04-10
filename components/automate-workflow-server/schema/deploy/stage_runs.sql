-- Deploy stage_runs
-- requires: changes

BEGIN;

CREATE TABLE stage_runs (
  -- should this be UUID? It's text in poc
  id BIGSERIAL PRIMARY KEY,
  change_id UUID NOT NULL REFERENCES changes(id),
  -- for now, skip enum or table on stages
  stage TEXT,
  status TEXT,
  finished BOOLEAN NOT NULL DEFAULT FALSE
);

-- we'll want to be able to find rows by change_id,
-- but a given change_id can have more than one run.
CREATE INDEX ON stage_runs(change_id);

COMMENT ON TABLE stage_runs IS
$$Track run of a change in through a stage
$$;

COMMIT;
