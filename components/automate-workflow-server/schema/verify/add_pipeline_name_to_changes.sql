-- Verify add_pipeline_name_to_changes

BEGIN;

SELECT pipeline_name_at_creation
FROM changes
WHERE FALSE;

ROLLBACK;
