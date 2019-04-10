-- Deploy fix_spurious_withdrawn_patchsets

BEGIN;

-- So this is pretty weird. I have observed that there are a handful
-- of patchsets in our system that are technically superseded (i.e.,
-- their changes have additional patchsets beyond the patchsets in
-- question), but are not marked as such (they're actually
-- 'withdrawn'!).
--
-- Not quite sure how those "withdrawn" changes kept going (they all
-- eventually merged!); nearly all of them were for automated
-- omnibus-delivery "bump" changes, and from a few months ago, so I'm
-- comfortable chalking it up to "old code".
--
-- In any event, this sets all those should-be-superseded patchsets
-- back to superseded.
--
-- Here's the logic:
--
-- We have the following status codes for patchsets:

--    delivery=# \dT+ cd_patchset_status
--                                               List of data types
--     Schema |        Name        |   Internal name    | Size |  Elements  | Access privileges | Description
--    --------+--------------------+--------------------+------+------------+-------------------+-------------
--     public | cd_patchset_status | cd_patchset_status | 4    | open      +|                   |
--            |                    |                    |      | superseded+|                   |
--            |                    |                    |      | withdrawn +|                   |
--            |                    |                    |      | accepted  +|                   |
--            |                    |                    |      | merged     |                   |
--    (1 row)
--
--    delivery=# select distinct status from patchsets;
--       status
--    ------------
--     merged
--     withdrawn
--     open
--     superseded
--    (4 rows)
--
-- (Interestingly, we don't seem to use `accepted`; we should probably
-- remove that.)
--
-- As patchsets come in, they are superseded. The latest patchset for
-- any given change should therefore either be `merged`, `withdrawn`,
-- or `open`. Put another way, if you remove all the patchsets that
-- are not marked as superseded, those that remain should have the
-- highest sequence_number for their change. Any that *do not* are by
-- definition marked incorrectly; whatever they are, they *should* be
-- `superseded`.
--
-- This query performs the necessary update, and does it idempotently
-- and safely; if no offending patchsets are found, nothing is
-- changed.
--
-- Note that there is not really a viable revert action for this.
WITH non_superseded_patchsets AS (
  SELECT id
  FROM patchsets
  WHERE status != 'superseded'
),
latest_patchsets AS (
  SELECT work.id
  FROM
  (SELECT p.id, p.sequence_number, max(sequence_number) OVER (partition BY p.change_id)
   FROM patchsets AS p) AS work
  WHERE work.sequence_number = work.max
),
should_be_superseded AS (
  SELECT id FROM non_superseded_patchsets
  EXCEPT
  SELECT id FROM latest_patchsets
)
UPDATE patchsets
SET status = 'superseded'
FROM should_be_superseded
WHERE patchsets.id = should_be_superseded.id
RETURNING *;
;

COMMIT;
