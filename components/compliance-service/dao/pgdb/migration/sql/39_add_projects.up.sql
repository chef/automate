CREATE TABLE IF NOT EXISTS projects (
  id          text NOT NULL,
  project_id  text NOT NULL UNIQUE,
  primary key(id),
  CHECK(length(project_id) > 0)
);

CREATE TABLE IF NOT EXISTS nodes_projects (
  node_id     text NOT NULL,
  project_id  text NOT NULL,
  CHECK(length(node_id) > 0),
  CHECK(length(project_id) > 0),
  UNIQUE(node_id, project_id)
);
