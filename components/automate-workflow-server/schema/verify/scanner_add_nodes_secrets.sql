-- Verify delivery:scanner_add_nodes_secrets on pg

BEGIN;

SELECT
    node_id,
    secret_id
FROM nodes_secrets WHERE FALSE;

ROLLBACK;
