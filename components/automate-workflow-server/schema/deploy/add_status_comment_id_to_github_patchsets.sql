-- Deploy delivery:add_status_comment_id_to_github_patchsets to pg

BEGIN;

ALTER TABLE github_patchsets ADD COLUMN status_comment_id BIGINT;

COMMIT;
