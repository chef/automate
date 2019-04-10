BEGIN;
UPDATE teams
    SET
        name = 'editors',
        description = 'Editors'
    WHERE
        name = 'operators'
    AND NOT EXISTS (select 1 from teams where name = 'editors');
COMMIT;
