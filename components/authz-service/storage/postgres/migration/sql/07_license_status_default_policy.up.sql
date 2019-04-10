BEGIN;

INSERT INTO policies
    VALUES ('ab2c89a9-a584-4501-b394-ec13d9991d61',
            '{"action": "read", "effect": "allow", "resource": "license:status", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "read", "effect": "allow", "resource": "license:status", "subjects": ["user:*"]}',
        deletable=FALSE;

COMMIT;
