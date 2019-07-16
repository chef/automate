BEGIN;

ALTER TABLE iam_projects
    DROP CONSTRAINT iam_projects_pkey; -- 'id' primary key
ALTER TABLE iam_projects
    ADD PRIMARY KEY (db_id); -- implies unique
ALTER TABLE iam_projects
    DROP CONSTRAINT iam_projects_db_id_key CASCADE; -- 'db_id' unique
ALTER TABLE iam_projects
    ADD UNIQUE (id);

-- recreate all links
-- Note: we've done this before; but in the separated steps, we've
-- relied on the unique
ALTER TABLE iam_project_rules
    ADD CONSTRAINT iam_projects_db_id_fkey FOREIGN KEY (project_id) REFERENCES iam_projects(db_id);
ALTER TABLE iam_staged_project_rules
    ADD CONSTRAINT iam_staged_project_rules_project_id_fkey FOREIGN KEY (project_id) REFERENCES iam_projects(db_id);
ALTER TABLE iam_role_projects
    ADD CONSTRAINT iam_role_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES iam_projects(db_id);
ALTER TABLE iam_policy_projects
    ADD CONSTRAINT iam_policy_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES iam_projects(db_id);
ALTER TABLE iam_statement_projects
    ADD CONSTRAINT iam_statement_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES iam_projects(db_id);

COMMIT;
