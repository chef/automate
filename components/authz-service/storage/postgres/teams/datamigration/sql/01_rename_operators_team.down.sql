BEGIN;
UPDATE teams
    SET
        name = 'operators',
        description = 'Operators'
    WHERE
        name = 'editors';
COMMIT;