-- Deploy add_latest_patchset_status_to_change

BEGIN;

ALTER TABLE changes ADD COLUMN latest_patchset_status cd_patchset_status;
ALTER TABLE changes ADD COLUMN latest_patchset SMALLINT;
ALTER TABLE changes ADD COLUMN submitted_at cd_timestamp DEFAULT NULL;
ALTER TABLE changes ADD COLUMN submitted_by TEXT;
-- We know all merged status will be the last status for that change
UPDATE changes
SET latest_patchset_status = p.status,
    latest_patchset = p.sequence_number
    FROM patchsets p
    WHERE changes.id = p.change_id
    AND p.status = 'merged';
-- Find the max patch number for all remaining changes and set status
-- accordingly
UPDATE changes
SET latest_patchset_status = work.status,
    latest_patchset = work.sequence_number
  FROM (
    SELECT
      p.status,
      p.change_id,
      p.sequence_number,
      max(p.sequence_number) over (PARTITION BY p.change_id)
    FROM patchsets AS p
    JOIN changes AS c
      ON c.id = p.change_id
      AND c.latest_patchset_status is NULL
    ) AS work
  WHERE changes.id = work.change_id;
-- Find the min patch number for all changes and set the submitted_by and
-- submitted_at values. Note, we're setting the text of the submitter, not
-- their id. Ther is no cascade for this operation.
UPDATE changes
SET submitted_at = work.submitted_at,
    submitted_by = u.name
  FROM (
    SELECT
      p.change_id,
      p.submitted_at,
      p.submitter_id,
      min(p.sequence_number) over (PARTITION by p.change_id)
    FROM patchsets AS p
    JOIN changes AS c
      ON c.id = p.change_id
    ) AS work, users AS u
  WHERE changes.id = work.change_id
  AND u.id = work.submitter_id;
-- now that all values are not NULL, change the new columns to not NULL
-- for any change we should know all of these values at any point in
-- time.
ALTER TABLE changes ALTER latest_patchset_status SET NOT NULL;
ALTER TABLE changes ALTER latest_patchset SET NOT NULL;
ALTER TABLE changes ALTER submitted_at SET NOT NULL;
ALTER TABLE changes ALTER submitted_by SET NOT NULL;
COMMIT;
