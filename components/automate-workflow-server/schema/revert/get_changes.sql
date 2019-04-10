-- Deploy get_changes

BEGIN;
DROP FUNCTION get_changes(text,text,text,text,cd_patchset_status,uuid,boolean,integer);
CREATE OR REPLACE FUNCTION get_changes(
  -- The project changes pertain to (cannot be NULL)
  p_enterprise_name enterprises.name%TYPE,
  p_organization_name organizations.name%TYPE,
  p_project_name projects.name%TYPE,

  -- Filtering Parameters (all can be NULL)
  p_pipeline_name              pipelines.name%TYPE,
  p_state                      cd_patchset_status,
  p_last_change_id             changes.id%TYPE,
  p_reverse_chronological_sort BOOLEAN,
  p_limit                      INTEGER)
RETURNS TABLE(
        id changes.id%TYPE,
        feature_branch changes.feature_branch%TYPE,
        pipeline pipelines.name%TYPE,
        status cd_patchset_status,
        submitted_at cd_timestamp,
        submitted_by users.name%TYPE,
        merge_sha changes.merge_sha%TYPE,
        approved_by changes.approved_by%TYPE,
        changeset_id changes.changeset_id%TYPE)
LANGUAGE plpgsql STABLE
ROWS 100
AS $$
DECLARE

  -- If we're given a change ID, then that marks the last one the user
  -- has seen, so we'll need to fetch the "page" following it,
  -- according to whatever other sorting criteria we've been given.
  --
  -- Whether or not we're looking for things before or after this
  -- point in time is determined by the value of the
  -- `p_reverse_chronological_sort` parameter; we'll need this date
  -- regardless of which direction we're sorting, though.
  cutoff_timestamp cd_timestamp;

  -- We'll need to lookup the ID of the project we're dealing with, as
  -- well as the ID of any filtering pipeline we may have been
  -- given. (We don't need to keep track of an enterprise ID or
  -- organization ID, even though those names are given as arguments
  -- to this function, since they aren't required for this query.)
  v_project_id projects.id%TYPE;
  v_pipeline_id pipelines.id%TYPE;

  -- We may or may not be given a direction to sort change submission
  -- dates by. A reverse chronological sort corresponds to SQL's
  -- `ORDER BY <date> DESC` clause.
  v_sort_direction CONSTANT TEXT := CASE WHEN p_reverse_chronological_sort IS TRUE
                                         THEN 'DESC'
                                         ELSE 'ASC'
                                    END;

  -- We're supporting a paginated API here, and so we need to control
  -- the size of our pages. We default to 10 changes if the user does
  -- not provide a page size of their own.
  v_limit CONSTANT INTEGER := CASE WHEN p_limit IS NOT NULL
                                   THEN p_limit
                                   ELSE 10
                              END;

  -- A boolean flag indicating whether or not the `p_last_change_id`
  -- (if given) actually corresponds to a change that exists and is
  -- associated with this project (and pipeline, if so filtered).
  v_valid_change_id BOOLEAN DEFAULT FALSE;

  -- For now, we aren't taking a user name when dealing with changes,
  -- though we eventually will in order to filter by authorization
  -- roles. The `to_ids` function call below needs a user name
  -- argument, though, but it can be NULL.
  v_user_name CONSTANT users.name%TYPE = NULL;
BEGIN
  -- TODO: Restrict updating of user names if the name is 'admin'

  -- First, let's convert our names into IDs
  SELECT project_id, pipeline_id
  FROM to_ids(p_enterprise_name, v_user_name, p_organization_name, p_project_name, p_pipeline_name)
  INTO v_project_id, v_pipeline_id;

  -- Make sure we've got a valid limit
  IF v_limit < 0 THEN
    RAISE EXCEPTION
    USING ERRCODE = 'CD009',
          MESSAGE = 'Pagination limit cannot be negative',
          DETAIL  = 'The specified limit (' || p_limit || ') is negative',
          HINT    = 'Don''t do that';
  END IF;

  -- Make sure the change ID (if given) is legitimate. At the same time,
  -- grab the timestamp.
  IF p_last_change_id IS NOT NULL THEN
    EXECUTE 'SELECT TRUE, c.submitted_at
    FROM changes AS c
    JOIN pipelines AS pipe
      ON c.pipeline_id = pipe.id
    JOIN projects AS p
      ON pipe.project_id = p.id
    WHERE c.id = ' || quote_literal(p_last_change_id) ||
     ' AND p.id = ' || v_project_id ||
    CASE WHEN v_pipeline_id IS NOT NULL
         THEN ' AND pipe.id = ' || v_pipeline_id
         ELSE ''
    END INTO v_valid_change_id, cutoff_timestamp;

    IF v_valid_change_id IS NOT TRUE THEN
      RAISE EXCEPTION
      USING ERRCODE = 'CD010',
            MESSAGE = 'Invalid change ID given for paging',
            DETAIL  = 'The specified change ID (' || p_last_change_id || ') does not exist, or is not associated with the specified project and / or pipeline!',
            HINT    = 'Please specify an existing change within this project / pipeline';
    END IF;
  END IF;

  -- Make sure we've got a valid state (if so filtering)
  --
  -- NOTE: I'm not so hot on this, since we're conflating "change
  -- status" with "patchset status", which are similar but separate
  -- things. A "change status" would eliminate this special exception,
  -- but we'd need to match against the text version of the patchsets'
  -- status, and that's a bit gross too.
  --
  -- Meh.
  IF p_state IS NOT NULL AND
     NOT (p_state = 'open' OR
          p_state = 'merged') THEN
     RAISE EXCEPTION
     USING ERRCODE = 'CD011',
           MESSAGE = 'Invalid change state given',
           DETAIL  = 'The specified change state (' || p_state || ') is not a valid input',
           HINT    = 'Please use one of ''open'', ''merged'', or NULL';
  END IF;

  -- END INPUT VALIDATION
  ------------------------------------------------------------------------

  -- Grab that data!
  RETURN QUERY EXECUTE 'SELECT c.id,
             c.feature_branch,
             pipe.name AS pipeline,
             c.latest_patchset_status AS status,
             c.submitted_at,
             c.submitted_by,
             c.merge_sha,
             c.approved_by,
             c.changeset_id
        FROM changes AS c
        JOIN pipelines AS pipe
          ON c.pipeline_id = pipe.id
        JOIN projects AS proj
          ON pipe.project_id = proj.id
        WHERE proj.id = ' || v_project_id ||

  -- Narrow the results to the specified pipeline.
  CASE WHEN p_pipeline_name IS NOT NULL
    THEN
        ' AND pipe.id = ' || v_pipeline_id
    ELSE
        ''
  END ||

  -- If the user has requested only changes of a given status, we perform
  -- that filtering here.
  CASE WHEN p_state IS NOT NULL
    THEN
        ' AND c.latest_patchset_status = ' || quote_literal(p_state)
    ELSE
        ''
  END ||

  -- If we were given a change ID, then we're looking for changes
  -- submitted before / after it, according to our sort order, so we
  -- filter on the submission timestamp. We do this, rather than use
  -- the OFFSET operator, because the latter is inefficient when used
  -- with large values (the server still has to compute all the rows
  -- it will eventually skip). It's better to not even consider in the
  -- first place all the rows you'll skip. See
  -- http://use-the-index-luke.com/no-offset for a technical
  -- discussion of this approach.
  --
  -- Note that there is a vanishingly small chance that two changes
  -- may have been submitted across all the pipelines of a given
  -- project at the exact same moment in time (down to PostgreSQL's
  -- microsecond resolution, at least). In that case, they will have
  -- identical timestamps, and it is possible that one (or more, if
  -- you're just astoundingly fortunate!) change would never appear in
  -- a result set from this function, given a particular page size
  -- (i.e. the page cutoff occurs within the "block" of identical
  -- timestamps).
  --
  -- To mitigate this, we also add in an additional filter
  -- incorporating change ID itself. This will enable us to generate
  -- an internally-consistent and complete set ofpages. We'll also use
  -- this in our final LIMIT clause below.
  CASE WHEN p_last_change_id IS NOT NULL
    THEN
        ' AND c.submitted_at ' ||
        -- When doing a reverse chronological sort, we know that our "next
        -- page" will not be interested in any changes that were submitted
        -- after our `cutoff_timestamp`. Likewise, a chronological sort
        -- means we are uninterested in all changes submitted before
        -- `cutoff_timestamp`.
        --
        -- We use >= / <= as opposed to simply > / < for the timestamp in
        -- order to keep possible simultaneous events around; one's we've
        -- already seen will be further filtered next
        CASE WHEN v_sort_direction = 'ASC'
          THEN '>='
          ELSE '<='
        END || quote_literal(cutoff_timestamp) ||
        -- Any changes that come in at the exact same time as our cutoff
        -- timestamp must be removed if they sort before the change we saw
        -- last
        ' AND NOT (c.submitted_at = ' || quote_literal(cutoff_timestamp) ||
        ' AND c.id ' ||
        CASE WHEN v_sort_direction = 'ASC'
          THEN '<='
          ELSE '>='
        END || quote_literal(p_last_change_id) || ') '
    ELSE
        ''
  END ||

  -- For paging, we need to order our result set, taking into account
  -- the filtering we've already (potentially) done
  --
  -- We include the change ID in the ordering to reflect the same
  -- filtering we performed earlier, and to ensure that pages remain
  -- consistent in the face of exceedingly rare simultaneous change
  -- submissions.
        ' ORDER BY c.submitted_at ' || v_sort_direction || ', c.id ' ||
        v_sort_direction || ' LIMIT ' || v_limit
;

END;
$$;

COMMIT;
