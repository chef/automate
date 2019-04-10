BEGIN;

ALTER TABLE policies ADD COLUMN deletable BOOLEAN NOT NULL DEFAULT TRUE;

/* DRPPC = Drop Policy (error codes must be five characters). */
CREATE OR REPLACE FUNCTION cannot_delete_policy_error() RETURNS trigger AS $$
BEGIN
    RAISE EXCEPTION 'You cannot delete policy with id % as it is not marked as deletable.', OLD.id USING
        ERRCODE='DRPPC';
END$$ LANGUAGE plpgsql;

CREATE TRIGGER only_allow_delete_on_deletable_policies
    BEFORE DELETE ON policies
    FOR EACH ROW
    WHEN (OLD.deletable = FALSE)
    EXECUTE PROCEDURE cannot_delete_policy_error();

/* Non deletable policies. */
INSERT INTO policies
    VALUES ('b4b00330-22a4-4cd2-ac2a-820983c1c3b0',
            '{"action": "*", "effect": "allow", "resource": "*", "subjects": ["team:local:admins"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "*", "subjects": ["team:local:admins"]}',
        deletable=FALSE;

INSERT INTO policies
    VALUES ('0632b04d-9453-402e-9ebe-12d8e44711ac',
            '{"action": "*", "effect": "allow", "resource": "auth_introspection:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "auth_introspection:*", "subjects": ["user:*"]}',
        deletable=FALSE;

/* Deletable policies. */
INSERT INTO policies
    VALUES ('cbdd1367-b731-4915-a97f-291199a63b62',
            '{"action": "*", "effect": "allow", "resource": "cfgmgmt", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "cfgmgmt", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('4ac5f6cd-788b-4a09-bcdd-b4e2cf4395e1',
            '{"action": "*", "effect": "allow", "resource": "cfgmgmt:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "cfgmgmt:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('73eb6ea3-55af-4eae-b7e9-4d89334087ed',
            '{"action": "*", "effect": "allow", "resource": "compliance:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data = '{"action": "*", "effect": "allow", "resource": "compliance:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('e9b37b6a-cf30-4167-bfcb-dfd50e45926c',
            '{"action": "*", "effect": "allow", "resource": "service_info:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "service_info:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('3b8e626f-a46e-4f1c-8e84-d026111cd566',
            '{"action": "*", "effect": "allow", "resource": "events", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "events", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('fb1bf117-1941-4208-9771-b45bf288ff28',
            '{"action": "*", "effect": "allow", "resource": "events:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "events:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('d4167a66-823d-4cf5-a7bb-4702094ec38c',
            '{"action": "*", "effect": "allow", "resource": "ingest:*", "subjects": ["client:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "ingest:*", "subjects": ["client:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('3222e45f-7b05-4b49-9bf3-c4357d6c032f',
            '{"action": "*", "effect": "allow", "resource": "nodes", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "nodes", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('254fdd71-276e-4a21-a43a-5f59e2b09d6e',
            '{"action": "*", "effect": "allow", "resource": "nodes:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "nodes:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('ecf04384-05d6-41b4-9544-9d39a6d2892f',
            '{"action": "*", "effect": "allow", "resource": "nodemanagers", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "nodemanagers", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('167115ac-361b-448f-b28e-8ab5b16f3a67',
            '{"action": "*", "effect": "allow", "resource": "nodemanagers:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "nodemanagers:*", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('a6a0031a-409a-40b8-83f5-cd3a92e4c527',
            '{"action": "*", "effect": "allow", "resource": "secrets", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "secrets", "subjects": ["user:*"]}',
        deletable=TRUE;

INSERT INTO policies
    VALUES ('febfe2ed-be49-47d7-83f3-95b18a48328d',
            '{"action": "*", "effect": "allow", "resource": "secrets:*", "subjects": ["user:*"]}',
            CURRENT_TIMESTAMP,
            1,
            TRUE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "*", "effect": "allow", "resource": "secrets:*", "subjects": ["user:*"]}',
        deletable=TRUE;

/* Delete deprecated default policies. */
DELETE FROM policies where id='eb85eb89-97a6-46f4-bf6e-53994b8ca378';
DELETE FROM policies where id='8604fa54-4100-448c-8fa7-99309cacb24d';
DELETE FROM policies where id='e9059dae-4414-41e2-80b4-12c0c8c3ba9f';

COMMIT;
