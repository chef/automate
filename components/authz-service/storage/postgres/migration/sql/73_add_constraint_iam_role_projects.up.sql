ALTER TABLE iam_role_projects
    ADD CONSTRAINT "iam_role_projects_role_id_project_id_unique" UNIQUE (role_id, project_id);