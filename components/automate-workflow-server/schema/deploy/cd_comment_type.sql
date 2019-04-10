-- Deploy cd_comment_type

BEGIN;

CREATE TYPE cd_comment_type AS ENUM('patchset',
                                    'line',
                                    'comment');

COMMIT;
