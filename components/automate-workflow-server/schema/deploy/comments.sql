-- Deploy comments
-- requires: cd_comment_status, cd_comment_type, patchsets, users

-- Gathers the data for all types of comments
-- That might seem like a waste of space, but it makes dealing with
-- comments so much simpler, on all levels (and the waste is pretty
-- minor anyway)

BEGIN;

CREATE TABLE IF NOT EXISTS comments(
  -- *** Fields common to all types of comments ***
  id BIGSERIAL PRIMARY KEY,
  patchset_id BIGINT NOT NULL REFERENCES patchsets(id) ON DELETE CASCADE,
  -- TODO: here too, we want this notion of 'former users'
  submitter_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  content TEXT NOT NULL,
  type cd_comment_type NOT NULL,
  status cd_comment_status NOT NULL DEFAULT 'draft',
  -- for a draft comment, that's the last time it was modified,
  -- for a published comment, that's the publication date, and cannot change
  -- from then on (even if the content itself is modified)
  last_modif_or_publication_timestamp cd_timestamp NOT NULL,

  -- *** Field(s) specific to line comments ***
  line_range int4range DEFAULT NULL,
  -- file_path can be NULL for commit message comments
  file_path TEXT DEFAULT NULL,

  -- *** Field(s) specific to comment comments ***
  parent_id BIGINT DEFAULT NULL REFERENCES comments(id) ON DELETE CASCADE,

  -- *** and the checks to make sure the right rows define the right field
  CHECK (
    CASE type
    WHEN 'line' THEN
      line_range IS NOT NULL
      -- no constraint on file_path, since it can be null for commit message comments
      AND parent_id IS NULL
    WHEN 'patchset' THEN
      line_range IS NULL
      AND file_path IS NULL
      AND parent_id IS NULL
    WHEN 'comment' THEN
      line_range IS NULL
      AND file_path IS NULL
      AND parent_id IS NOT NULL
      -- this one gives some kind of protection against cycles
      -- with that, the only way to get a cycle would be to update a comment's parent_id
      -- after its creation, which is forbidden by the `update_comment` trigger
      AND parent_id != id
    END
  )
);

-- TODO we'll probably want an index on status, too

CREATE INDEX patchset_id_file_path_idx
ON comments(patchset_id, file_path)
WHERE type != 'comment';

CREATE INDEX parent_id_idx
ON comments(parent_id)
WHERE type = 'comment';

COMMIT;
