-- Deploy changes_add_title_and_description
-- requires: changes

BEGIN;

ALTER TABLE changes ADD COLUMN title TEXT;
ALTER TABLE changes ADD COLUMN description TEXT;

COMMIT;
