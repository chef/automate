BEGIN;

CREATE TABLE golang_migrations (
  only_one_row boolean PRIMARY KEY DEFAULT TRUE,
  last_migration_ran INTEGER NOT NULL,
  CONSTRAINT only_one_row_unique CHECK (only_one_row)
);

-- init state
INSERT INTO golang_migrations (last_migration_ran) VALUES (0);

COMMIT;
