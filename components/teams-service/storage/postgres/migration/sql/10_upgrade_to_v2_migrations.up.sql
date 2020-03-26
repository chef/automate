BEGIN;

UPDATE teams
    SET
        name = 'editors',
        description = 'Editors'
    WHERE
        name = 'operators'
    AND NOT EXISTS (select 1 from teams where name = 'editors');

INSERT INTO teams
    VALUES (uuid_generate_v4(), 'editors',
            'Editors',
            CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, TRUE)
    ON CONFLICT (name) DO NOTHING;

INSERT INTO teams
    VALUES (uuid_generate_v4(), 'viewers',
            'Viewers',
            CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, TRUE)
    ON CONFLICT (name) DO NOTHING;

COMMIT;
