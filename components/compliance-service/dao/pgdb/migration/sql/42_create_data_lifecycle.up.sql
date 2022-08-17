
CREATE TABLE IF NOT EXISTS compliance_lifecycle (
  id BIGSERIAL PRIMARY KEY,
  policy_name TEXT NOT NULL,
  older_than_days INTEGER NOT NULL
);

INSERT INTO compliance_lifecycle (policy_name, older_than_days) VALUES ('unreachable-assets', 60);