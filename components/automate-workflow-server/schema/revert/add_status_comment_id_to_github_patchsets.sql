-- Revert delivery:add_status_comment_id_to_github_patchsets from pg

BEGIN;

ALTER TABLE github_patchsets DROP COLUMN status_comment_id;

COMMIT;
