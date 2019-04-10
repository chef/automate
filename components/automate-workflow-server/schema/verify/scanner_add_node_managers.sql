-- Verify delivery:scanner_add_node_managers on pg

BEGIN;

SELECT
  id,
  name,
  type,
  credentials,
  status_type,
  status_message
FROM node_managers
WHERE FALSE;

SELECT
  node_selectors
FROM jobs WHERE FALSE;

ROLLBACK;
