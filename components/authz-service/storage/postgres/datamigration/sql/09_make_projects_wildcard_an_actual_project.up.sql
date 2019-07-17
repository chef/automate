BEGIN;

INSERT INTO iam_projects (id, name, type) VALUES ('*', 'Wildcard project', 'chef-managed');

COMMIT;
