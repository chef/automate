-- Deploy delivery_role

BEGIN;

-- There is no IF NOT EXISTS clause for type creation.  We could wrap
-- things in an anonymous DO block if you really wanted to...
CREATE TYPE delivery_role AS ENUM('admin',
                                  'committer',
                                  'reviewer',
                                  'shipper',
                                  'observer');

COMMIT;
