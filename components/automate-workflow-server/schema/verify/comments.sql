-- Verify comments

BEGIN;

SELECT id,
       patchset_id,
       submitter_id,
       content,
       type,
       status,
       last_modif_or_publication_timestamp,
       line_range,
       file_path,
       parent_id
FROM comments
WHERE FALSE;

ROLLBACK;
