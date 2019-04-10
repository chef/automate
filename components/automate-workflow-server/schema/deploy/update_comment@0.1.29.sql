-- Deploy update_comment
-- requires: comments

-- This is meant to be used as an update trigger on comments
-- to implement the desired logic for comment updates.

BEGIN;

CREATE OR REPLACE FUNCTION update_comment()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
BEGIN
  IF OLD.status = 'draft' THEN
    -- clock_timestamp makes it easier to test than NOW!
    NEW.last_modif_or_publication_timestamp = clock_timestamp();
    -- the only 2 fields that can be updated are 'status' or 'content'
    -- any other updated is forbidden
    -- this is actually really important for comment comments: if we allowed
    -- updating the 'parent_id', that would mean we could have cycles
    IF (NEW.patchset_id, NEW.submitter_id, NEW.type, NEW.line_range, NEW.file_path, NEW.parent_id)
       IS DISTINCT FROM
       (OLD.patchset_id, OLD.submitter_id, OLD.type, OLD.line_range, OLD.file_path,  OLD.parent_id) THEN
      RAISE EXCEPTION
      USING ERRCODE = 'CD012',
            MESSAGE = 'Forbidden update on a draft comment',
            DETAIL  = 'You can only update the ''status'' and ''content'' fields',
            HINT    = 'Don''t make me say that again';
    END IF;
  ELSE
    -- you can't update a 'published' comment
     RAISE EXCEPTION
     USING ERRCODE = 'CD013',
           MESSAGE = 'Updating a published comment is forbidden',
           DETAIL  = 'Comment ID ' || OLD.id || ' has been published, it''s now impossible '
                     'to update its data',
           HINT    = 'Don''t make me say that again';
  END IF;

  RETURN NEW;
END;
$$;

COMMIT;
