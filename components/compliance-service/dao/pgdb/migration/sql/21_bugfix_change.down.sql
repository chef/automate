ALTER TABLE IF EXISTS nodes
  DROP COLUMN IF EXISTS connection_error;

ALTER TABLE IF EXISTS nodes
  DROP COLUMN IF EXISTS statechange_timestamp;