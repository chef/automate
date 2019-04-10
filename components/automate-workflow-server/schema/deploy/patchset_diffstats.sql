-- Deploy patchset_diffstats
-- requires: patchsets

BEGIN;

CREATE TABLE IF NOT EXISTS patchset_diffstats(
  id BIGINT NOT NULL PRIMARY KEY REFERENCES patchsets(id) ON UPDATE CASCADE ON DELETE CASCADE,
  files_changed INTEGER NOT NULL,
  insertions INTEGER NOT NULL,
  deletions INTEGER NOT NULL
);

COMMENT ON TABLE patchset_diffstats IS
$$This table caches the git diffstats for every patchset.
$$;

COMMIT;
