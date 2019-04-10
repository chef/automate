-- Deploy patchset_changed_files
-- requires: patchsets

BEGIN;

CREATE TABLE IF NOT EXISTS patchset_changed_files(
  patchset_id BIGINT NOT NULL REFERENCES patchsets(id) ON UPDATE CASCADE ON DELETE CASCADE,
  -- one of "modified", "added", "deleted", or "none"
  -- "none" is a sepcial value to mark that said patchset actually didn't change any file,
  -- allowing for caching in that case too
  -- TODO: make that a proper enum?
  status TEXT NOT NULL,
  file TEXT,
  UNIQUE(patchset_id, file),

  -- the actual 'no changed file' marker is to have *both* status
  -- and file set to 'none';
  -- the reason why we do need both fields set to 'none' (as opposed
  -- to just status) is to be able to take advantage of the
  -- unique index on (patchset_id, file) to make sure that
  -- no race condition can result in having two 'no changed file'
  -- markers for a single patchset.
  --
  -- This whole idea is summed up in the following logical
  -- implication (which is *not* an equivalence)
  -- status = 'none' => file = 'none'
  CHECK(status != 'none' OR file = 'none')
);

COMMENT ON TABLE patchset_changed_files IS
$$This table caches the changed files for every patchset
(one row per file and per patchset)
$$;

COMMIT;
