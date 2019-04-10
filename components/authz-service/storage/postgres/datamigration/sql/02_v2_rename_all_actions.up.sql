BEGIN;

UPDATE iam_statements
    SET
        role = 'owner'
    WHERE
        role = 'all-actions';

UPDATE iam_roles
    SET
        id = 'owner',
        name = 'Owner'
    WHERE
        id = 'all-actions';

COMMIT;
