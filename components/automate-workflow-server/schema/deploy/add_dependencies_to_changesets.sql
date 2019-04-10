-- Deploy delivery:add_dependencies_to_changesets to pg

BEGIN;

ALTER TABLE changesets ADD COLUMN dependencies BIGINT[];

COMMENT ON COLUMN changesets.dependencies IS
$$
An array of pipeline uuids representing which pipelines this changeset
has a dependency on.
$$;

COMMIT;
