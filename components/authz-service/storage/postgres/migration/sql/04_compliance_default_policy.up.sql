BEGIN;

INSERT INTO policies
    (id, policy_data, created_at, version, deletable)
    VALUES ('3db8c368-9cb2-4839-9de4-57ded0de9a55',
            '{"action": "read", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["client:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "read", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["client:*"]}',
        deletable=TRUE;

INSERT INTO policies
    (id, policy_data, created_at, version, deletable)
    VALUES ('0d16e400-e6db-4694-a39c-a4d61f161bae',
            '{"action": "upload", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["client:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "upload", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["client:*"]}',
        deletable=TRUE;

INSERT INTO policies
    (id, policy_data, created_at, version, deletable)
    VALUES ('9fb73dc9-ef08-498a-af64-a8a17176f18f',
            '{"action": "search", "effect": "allow", "resource": "compliance:profiles", "subjects": ["client:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "search", "effect": "allow", "resource": "compliance:profiles", "subjects": ["client:*"]}',
        deletable=TRUE;

COMMIT;
