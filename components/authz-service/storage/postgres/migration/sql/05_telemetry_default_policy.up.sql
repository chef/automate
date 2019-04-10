BEGIN;

INSERT INTO policies
    VALUES ('f8d97cb1-a40e-46e4-bba8-a4ae8be51371',
            '{"action": "read", "effect": "allow", "resource": "telemetry:config", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "read", "effect": "allow", "resource": "telemetry:config", "subjects": ["user:*"]}',
        deletable=FALSE;

-- Remove default policy for service_info:telemetry
UPDATE policies SET deletable=TRUE WHERE id='44fa09fa-3c46-11e8-b467-0ed5f89f718b';
DELETE FROM policies WHERE id='44fa09fa-3c46-11e8-b467-0ed5f89f718b';

COMMIT;
