BEGIN;

DELETE FROM policies WHERE id='cbdd1367-b731-4915-a97f-291199a63b62';
DELETE FROM policies WHERE id='4ac5f6cd-788b-4a09-bcdd-b4e2cf4395e1';

INSERT INTO policies
    VALUES ('42ca2668-511a-48e2-b5e2-553b26d34698',
            '{"action": "*", "effect": "allow", "resource": "cfgmgmt:stats:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "cfgmgmt:stats:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('76a38aae-a15e-463b-8c2b-4a09c1d62ef6',
            '{"action": "*", "effect": "allow", "resource": "cfgmgmt:nodes:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "cfgmgmt:nodes:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('ce23fd69-c4bd-4407-961b-60f9b26c4dfa',
            '{"action": "*", "effect": "allow", "resource": "cfgmgmt:nodes", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "cfgmgmt:nodes", "subjects": ["user:*"]}',
        deletable=TRUE;
END;
