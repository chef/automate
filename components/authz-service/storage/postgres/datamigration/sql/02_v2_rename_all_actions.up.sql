BEGIN;

UPDATE iam_statements
    SET
        role_id = role_db_id('owner')
    WHERE
        role_id = role_db_id('all-actions');

UPDATE iam_roles
    SET
        id = 'owner',
        name = 'Owner'
    WHERE
        id = 'all-actions';

COMMIT;
