-- Deploy delivery:add_description_to_phase_runs to pg

BEGIN;

ALTER TABLE phase_runs ADD COLUMN description TEXT;

COMMIT;

COMMENT ON COLUMN phase_runs.description IS
$$
A user-meaningful description of this particular phase run when a stage
consists of multiple copies of a given phase. For example, when running
multiple unit phases across different platforms.
$$;
