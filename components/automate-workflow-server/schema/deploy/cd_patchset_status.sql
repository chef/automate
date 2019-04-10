-- Deploy cd_patchset_status

BEGIN;

CREATE TYPE cd_patchset_status AS ENUM('open',
                                       -- superseded means that the patch set no longer is
                                       -- the latest one for that change
                                       'superseded',
                                       'withdrawn',
                                       'accepted',
                                       'merged');

COMMIT;
