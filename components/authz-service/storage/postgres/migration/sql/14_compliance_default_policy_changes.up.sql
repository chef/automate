BEGIN;

DELETE FROM policies WHERE id='73eb6ea3-55af-4eae-b7e9-4d89334087ed';

INSERT INTO policies
    VALUES ('54cf311b-a01e-4bde-bf54-5bf126e0adef',
            '{"action": "*", "effect": "allow", "resource": "compliance:reporting:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "compliance:reporting:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('06b712e5-eb69-41e6-b63f-7d13bebc33c3',
            '{"action": "*", "effect": "allow", "resource": "compliance:scanner:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "compliance:scanner:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('7015a011-d9e4-493c-a257-25c2a9407750',
            '{"action": "*", "effect": "allow", "resource": "compliance:profiles:market", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "compliance:profiles:market", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('34d5a221-b6a4-45aa-98d0-b57e2fed9454',
            '{"action": "*", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('95eaf07d-e1f6-4037-9ce9-7dbd99d6c947',
            '{"action": "*", "effect": "allow", "resource": "compliance:profiles", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "compliance:profiles", "subjects": ["user:*"]}',
        deletable=TRUE;


INSERT INTO policies
    VALUES ('b92de3b2-7620-4dac-b9eb-99dfe0db003f',
            '{"action": "delete", "effect": "allow", "resource": "compliance:profiles:${a2:username}", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "delete", "effect": "allow", "resource": "compliance:profiles:${a2:username}", "subjects": ["user:*"]}',
        deletable=TRUE;
END;
