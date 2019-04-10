-- Revert changes_add_title_and_description

BEGIN;

ALTER TABLE changes DROP COLUMN title;
ALTER TABLE changes DROP COLUMN description;

COMMIT;
