BEGIN;

INSERT INTO policies
    VALUES ('f9eb8c5a-3b8b-4695-ae39-ca434237f69b',
            '{"action": "*", "effect": "allow", "resource": "auth:users:${a2:username}", "subjects": ["user:local:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "auth:users:${a2:username}", "subjects": ["user:local:*"]}',
        deletable=TRUE;

END;
