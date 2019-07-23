BEGIN;

UPDATE teams
    SET
        description = 'admins'
    WHERE
        name = 'admins';

COMMIT;