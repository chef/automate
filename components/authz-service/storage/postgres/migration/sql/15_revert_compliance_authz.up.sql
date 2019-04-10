BEGIN;

DELETE FROM policies WHERE id='54cf311b-a01e-4bde-bf54-5bf126e0adef';

DELETE FROM policies WHERE id='06b712e5-eb69-41e6-b63f-7d13bebc33c3';

DELETE FROM policies WHERE id='7015a011-d9e4-493c-a257-25c2a9407750';

DELETE FROM policies WHERE id='34d5a221-b6a4-45aa-98d0-b57e2fed9454';

DELETE FROM policies WHERE id='95eaf07d-e1f6-4037-9ce9-7dbd99d6c947';

DELETE FROM policies WHERE id='b92de3b2-7620-4dac-b9eb-99dfe0db003f';

INSERT INTO policies
    VALUES ('73eb6ea3-55af-4eae-b7e9-4d89334087ed',
            '{"action": "*", "effect": "allow", "resource": "compliance:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data = '{"action": "*", "effect": "allow", "resource": "compliance:*", "subjects": ["user:*"]}',
        deletable=TRUE;

END;