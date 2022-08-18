
CREATE TABLE IF NOT EXISTS compliance_lifecycle (
  id BIGSERIAL PRIMARY KEY,
  policy_name TEXT NOT NULL,
  value INTEGER NOT NULL,
  timestamp timestamp default current_timestamp
);

INSERT INTO compliance_lifecycle (policy_name, value) VALUES ('unreachable_assets', 60);