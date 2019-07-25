BEGIN;

UPDATE iam_roles
    SET
        id = 'owner',
        name = 'Owner'
    WHERE
        id = 'all-actions';

COMMIT;
