CREATE TABLE policies (
  id uuid PRIMARY KEY,
  policy_data JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  version INT NOT NULL
);