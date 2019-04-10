-- Deploy delete_withdrawn_changes

BEGIN;

DELETE FROM changes USING patchsets where changes.id = patchsets.change_id and patchsets.status = 'withdrawn';

COMMIT;
