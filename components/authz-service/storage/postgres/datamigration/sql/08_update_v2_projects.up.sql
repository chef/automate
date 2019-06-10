BEGIN;

UPDATE iam_projects
    SET
        name = '(unassigned)'
    WHERE
        id = '(unassigned)';

COMMIT;
