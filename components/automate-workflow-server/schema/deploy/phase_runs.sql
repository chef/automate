-- Deploy phase_runs
-- requires: stage_runs

BEGIN;

CREATE TABLE phase_runs (
  id BIGSERIAL PRIMARY KEY,     -- should this be UUID? It's text in poc
  stage_run_id BIGINT NOT NULL REFERENCES stage_runs(id) ON DELETE CASCADE,
  phase TEXT, -- for now skip enum on phases
  status TEXT, -- for now skip status enum
  finished BOOLEAN DEFAULT FALSE,
  run_success BOOLEAN DEFAULT FALSE, -- TODO not used in the code. get rid of that field
  run_log TEXT,
  run_status TEXT,
  build_node TEXT,
  search_query TEXT
);

CREATE INDEX ON phase_runs(stage_run_id);

COMMENT ON TABLE phase_runs IS
$$Track run of a phase within a stage for a change
$$;

COMMIT;
