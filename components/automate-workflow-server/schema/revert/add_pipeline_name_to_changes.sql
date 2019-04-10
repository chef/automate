-- Revert add_pipeline_name_to_changes

BEGIN;

ALTER TABLE changes DROP COLUMN pipeline_name_at_creation;

COMMIT;
