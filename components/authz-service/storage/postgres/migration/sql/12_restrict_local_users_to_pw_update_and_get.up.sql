BEGIN;

-- If the user has already deleted the default policy regarding local users having access
-- to themselves, then do not update that policy. Else, update policy to be more restrictive
-- and also to reflect the fact that UpdateSelf is in a different resource namespace.
INSERT INTO policies
    (id, policy_data, created_at, version, deletable)
    SELECT 'f9eb8c5a-3b8b-4695-ae39-ca434237f69b',
           '{"action": "read", "effect": "allow", "resource": "auth:users:${a2:username}", "subjects": ["user:local:*"]}',
           CURRENT_TIMESTAMP,
           1,
           TRUE
    WHERE EXISTS ( select * from policies where id='f9eb8c5a-3b8b-4695-ae39-ca434237f69b' )
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "read", "effect": "allow", "resource": "auth:users:${a2:username}", "subjects": ["user:local:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('6ac3f062-0a84-437d-9f67-3d343fd8b0b8',
            '{"action": "*", "effect": "allow", "resource": "users:${a2:username}", "subjects": ["user:local:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "users:${a2:username}", "subjects": ["user:local:*"]}',
        deletable=TRUE;

END;
