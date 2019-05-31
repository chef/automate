BEGIN;

INSERT INTO policies
    VALUES ('aee14d59-da0b-4974-ba6d-1a018b024874',
            '{"action": "*", "effect": "allow", "resource": "service_groups", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "list", "effect": "allow", "resource": "service_groups", "subjects": ["user:*"]}',
        deletable=TRUE;

UPDATE iam_roles
    SET
        actions = actions || '{applications:*:list}'
    WHERE
        id = 'viewer';

UPDATE iam_roles
    SET
        actions = actions || '{applications:*}'
    WHERE
        id = 'editor';
END;
