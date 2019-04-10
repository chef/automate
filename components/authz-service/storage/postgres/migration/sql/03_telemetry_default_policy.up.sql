BEGIN;

INSERT INTO policies
    VALUES ('44fa09fa-3c46-11e8-b467-0ed5f89f718b',
            '{"action": "*", "effect": "allow", "resource": "service_info:telemetry", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "service_info:telemetry", "subjects": ["user:*"]}',
        deletable=FALSE;

COMMIT;
