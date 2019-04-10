-- Deploy add_pipeline_name_to_changes

BEGIN;

ALTER TABLE changes ADD COLUMN pipeline_name_at_creation TEXT;

UPDATE changes
SET pipeline_name_at_creation = pipelines.name
FROM pipelines
WHERE changes.pipeline_id = pipelines.id;

ALTER TABLE changes ALTER COLUMN pipeline_name_at_creation SET NOT NULL;

COMMENT ON COLUMN changes.pipeline_name_at_creation IS
$$Pipeline_name is invariant after change creation to reflect state at
time of change creation. In the event of a pipeline name change, the id
should still be used to target the build. However, the change was
originally intended to target the pipeline at its creation as expressed
by the user who created the change. Much like the delivered_by,
created_by and approved_by being TEXT fields instead of the IDs of the
acting user. Pipeline_id is still present and the foreign key
constraint is still enforced. Deleting a pipeline will still delete all
changes associated with that pipeline irrespective of any name changes.
$$;

COMMIT;
