ALTER TABLE nodes_projects
ADD CONSTRAINT nodes_projects_fk
FOREIGN KEY (node_id)
REFERENCES nodes(id)
ON DELETE CASCADE;
