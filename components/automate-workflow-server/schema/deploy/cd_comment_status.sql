-- Deploy cd_comment_status

BEGIN;

CREATE TYPE cd_comment_status AS ENUM('draft',
                                      'published');

COMMIT;
