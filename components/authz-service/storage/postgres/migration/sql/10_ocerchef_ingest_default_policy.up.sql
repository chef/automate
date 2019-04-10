BEGIN;

INSERT INTO policies
    VALUES ('8df0a55f-2a93-4119-b84f-f382d5019271',
            '{"action": "read", "effect": "allow", "resource": "ingest:status", "subjects": ["tls:service:automate-cs-oc-erchef:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "read", "effect": "allow", "resource": "ingest:status", "subjects": ["tls:service:automate-cs-oc-erchef:*"]}',
        deletable=FALSE;

INSERT INTO policies
    VALUES ('9d67b657-2d37-40f6-a11a-0b6c86aac1d9',
            '{"action": "create", "effect": "allow", "resource": "ingest:unified_events", "subjects": ["tls:service:automate-cs-oc-erchef:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "create", "effect": "allow", "resource": "ingest:unified_events", "subjects": ["tls:service:automate-cs-oc-erchef:*"]}',
        deletable=FALSE;
COMMIT;
