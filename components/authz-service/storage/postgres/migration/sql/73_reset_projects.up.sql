BEGIN;

DELETE FROM iam_projects WHERE type='custom';
DELETE FROM iam_projects_graveyard;

COMMIT;
