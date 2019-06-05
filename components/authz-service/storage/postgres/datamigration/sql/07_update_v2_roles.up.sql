BEGIN;

UPDATE iam_roles
    SET
        actions = actions || '{applications:*:list, applications:*:get}'
    WHERE
        id = 'viewer';

UPDATE iam_roles
    SET
        actions = actions || '{applications:*}'
    WHERE
        id = 'editor';

COMMIT;
