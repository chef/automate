BEGIN;

INSERT INTO iam_projects (id, name, type, projects)
    VALUES ('(unassigned)', 'Unassigned', 'chef-managed', '{(unassigned)}')
    ON CONFLICT DO NOTHING;

/* Define the 'all projects' project */
INSERT INTO iam_projects (id, name, type, projects)
    VALUES ('~~ALL-PROJECTS~~', 'All Projects', 'chef-managed', '{~~ALL-PROJECTS~~}')
    ON CONFLICT DO NOTHING;

/* For any statements lacking projects, provide the default of 'all projects' */
INSERT INTO iam_statement_projects (statement_id, project_id)
    SELECT db_id, project_db_id('~~ALL-PROJECTS~~') FROM iam_statements
    WHERE db_id NOT IN (SELECT statement_id FROM iam_statement_projects);

COMMIT;
