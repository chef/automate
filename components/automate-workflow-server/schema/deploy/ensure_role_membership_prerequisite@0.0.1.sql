-- Deploy ensure_role_membership_prerequisite

BEGIN;

CREATE OR REPLACE FUNCTION ensure_role_membership_prerequisite()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
DECLARE
  -- All these variables are required for creating useful exception
  -- messages, mainly for getting the names of the entities in
  -- question, as opposed to their database IDs.  It takes a lot of
  -- work to be user-friendly :)

  -- We'll always need to know the name of the user being operated on.
  user_name users.name%TYPE;

  -- We'll also want to know the enterprise that the user actually
  -- does belong to
  user_enterprise_name enterprises.name%TYPE;

  -- When handling exceptions for granting roles at the enterprise
  -- level, we'll need the name of the enterprise we *thought* the
  -- user was in.
  --
  -- We'll also want a place to store the name of the enterprise that
  -- an organization, project, or pipeline belong to, as well.
  target_enterprise_name enterprises.name%TYPE;

  -- Likewise, this is for grabbing the name of the erroneous
  -- organization we're trying to grant a role in.
  target_organization_name organizations.name%TYPE;

  -- This function will eventually handle errors for role grants to
  -- projects and pipelines as well, but the schema of those tables
  -- doesn't exist yet.  When they do, then we'll need some variables
  -- to store the names for those as well.

  -- This will be error 001 for Chef Delivery.  Yay, custom error codes!
  error_code CONSTANT TEXT = 'CD001';

  -- This message will be shown to clients when an error has occurred.
  -- It will be the same regardless of which membership violation has
  -- occurred (user not in enterprise / user not in enterprise of
  -- the organization, project, or pipeline).  It will be further
  -- refined in each case with custom DETAIL and HINT attributes.
  message CONSTANT TEXT = 'Membership prerequisite for role granting not met';
BEGIN

  -- To ensure the proper contract is satisfied, we'll do a query that
  -- will only return a row if the user and the enterprise /
  -- organization / project / pipeline are compatible.  If a row is
  -- returned, then we're good; otherwise, we'll create an exception.
  CASE TG_TABLE_NAME
  WHEN 'enterprise_user_roles' THEN
    PERFORM *
    FROM users
    WHERE id            = NEW.user_id
      AND enterprise_id = NEW.enterprise_id;
  WHEN 'organization_user_roles' THEN
    PERFORM *
    FROM users AS u
    JOIN organizations AS o
      ON u.enterprise_id = o.enterprise_id
    WHERE o.id = NEW.organization_id
      AND u.id = NEW.user_id;
  ELSE
    -- TODO: Add cases for project and pipeline when we have those
    -- tables in place
    RAISE EXCEPTION 'not_implemented_yet';
  END CASE;

  IF NOT FOUND THEN
    -- If the above query found no rows, then we've got a problem;
    -- time to create an exception!  However, we're only going to
    -- throw an exception if the given user and enterprise /
    -- organization / project / pipeline are all actual entities in
    -- the database already.  If not (i.e., the user passed NULL or an
    -- otherwise invalid identifier), then we're not going to do
    -- anything, opting instead to let the schema-level constraints
    -- (NOT NULL, FKs, etc.) catch the error.  That way, we always get
    -- the most appropriate exception, and we don't need to have this
    -- code handling those things that the database is already really
    -- good at handling.

    -- For each case, we'll want to find the names of all the applicable
    -- entities in order to make an intelligible DETAIL message for our
    -- exception.

    -- We'll always need the name of the user, as well as the enterprise
    -- they're from.
    SELECT u.name, e.name
    FROM users AS u
    JOIN enterprises AS e
      ON u.enterprise_id = e.id
    WHERE u.id = NEW.user_id
    INTO user_name, user_enterprise_name;

    -- Only if that query returned something will we continue.
    -- Otherwise, the given user information isn't even valid, so
    -- there's no use doing any further checks.
    IF FOUND THEN
      CASE TG_TABLE_NAME
      WHEN 'enterprise_user_roles' THEN
        -- Now to figure out what enterprise we were trying to grant
        -- permissions in
        SELECT name FROM enterprises
        WHERE id = NEW.enterprise_id
        INTO target_enterprise_name;

        IF FOUND THEN
          -- Only raise the exception if the enterprise was actually
          -- valid (we already know the user is valid)
          RAISE EXCEPTION
          USING ERRCODE = error_code,
                MESSAGE = message,
                DETAIL  = 'User "' || user_name ||'" is a member of the enterprise "' ||
                          user_enterprise_name || '", not "' || target_enterprise_name || '"',
                HINT    = 'A user can only be granted a role at the enterprise scope for the enterprise it is a member of.';
        END IF;

      WHEN 'organization_user_roles' THEN
        -- Grab some information about the organization we're trying
        -- to get permissions in
        SELECT o.name, e.name
        FROM organizations AS o
        JOIN enterprises AS e
          ON o.enterprise_id = e.id
        WHERE o.id = NEW.organization_id
        INTO target_organization_name, target_enterprise_name;

        IF FOUND THEN
          -- Again, only if the organization is actually real
          RAISE EXCEPTION
          USING ERRCODE = error_code,
                MESSAGE = message,
                DETAIL  = 'User "' || user_name || '" from enterprise "' || user_enterprise_name ||
                          '" cannot have permissions in organization "' || target_organization_name ||
                          '" from enterprise "' || target_enterprise_name || '"',
                HINT    = 'A user can only be granted a role at the organization scope if both the user and organization belong to the same enterprise.';
        END IF;
      ELSE
        RAISE EXCEPTION 'not_implemented_yet';
      END CASE; -- TG_TABLE_NAME
    END IF; -- If the user actually exists
  END IF; -- If an error was detected

  -- If we've gotten this far, then we haven't thrown an exception.
  -- That either means that the tuple was totally valid, or it was
  -- invalid in a way that the schema-level constraints on the table
  -- in question will catch.
  RETURN NEW;
END;
$$;

COMMIT;
