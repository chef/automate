-- Deploy delivery:fix_change_patchset_info to pg
-- requires: add_patchset_info_to_changes

-- This change fixes an issue with add_patchset_info_to_changes where it was
-- not correctly identifying the most recent and the earliest patchset for
-- changes. This resulted in incorrect information being inserted into the
-- change table.

BEGIN;

-- Find the max patch number for all remaining changes and set status
-- accordingly
WITH last_patchsets AS (
  SELECT p.status,
         p.change_id,
         p.sequence_number,
         max(p.sequence_number) over (PARTITION BY p.change_id) AS max_sequence_number
    FROM patchsets AS p
    JOIN changes AS c
      ON c.id = p.change_id
)
UPDATE changes
SET latest_patchset_status = last_patchsets.status,
    latest_patchset = last_patchsets.sequence_number
  FROM last_patchsets
  WHERE changes.id = last_patchsets.change_id
    AND last_patchsets.sequence_number = last_patchsets.max_sequence_number
;
-- Find the min patch number for all changes and set the submitted_by and
-- submitted_at values. Note, we're setting the text of the submitter, not
-- their id. Ther is no cascade for this operation.
WITH first_patchsets AS (
  SELECT p.change_id,
         p.submitted_at,
         p.submitter_id,
         p.sequence_number,
         min(p.sequence_number) over (PARTITION BY p.change_id) AS min_sequence_number
    FROM patchsets AS p
    JOIN changes AS c
      ON c.id = p.change_id
)
UPDATE changes
SET submitted_at = first_patchsets.submitted_at,
    submitted_by = u.name
  FROM first_patchsets, users as u
  WHERE changes.id = first_patchsets.change_id
    AND first_patchsets.sequence_number = first_patchsets.min_sequence_number
    AND u.id = first_patchsets.submitter_id
;

COMMIT;
