-- Deploy cd_changeset_status

BEGIN;

CREATE TYPE cd_changeset_status AS ENUM('open',
                                        'closed');

COMMIT;
