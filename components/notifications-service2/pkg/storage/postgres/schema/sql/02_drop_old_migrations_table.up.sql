-- This table tracked the migrations in the v1.0 (elixir) notifications service
-- the golang-migrate tooling we use now uses a table named `schema_migrations`
DROP TABLE IF EXISTS migrations;
