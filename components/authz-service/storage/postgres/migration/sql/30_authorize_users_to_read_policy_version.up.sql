BEGIN;

INSERT INTO policies
    VALUES ('7c0f72b0-d706-424e-b6e8-22abaa37d5cc',
            '{"action": "read", "effect": "allow", "resource": "auth:policies", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "read", "effect": "allow", "resource": "auth:policies", "subjects": ["user:*"]}',
        deletable=FALSE;

END;
