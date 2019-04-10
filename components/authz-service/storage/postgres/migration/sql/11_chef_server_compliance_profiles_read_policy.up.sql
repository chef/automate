BEGIN;

INSERT INTO policies
	VALUES ('9ed85167-a659-4415-b984-fde88d68c6f0',
			'{"action": "read", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["tls:service:automate-cs-nginx:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "read", "effect": "allow", "resource": "compliance:profiles:storage:*", "subjects": ["tls:service:automate-cs-nginx:*"]}',
        deletable=FALSE;

INSERT INTO policies
		VALUES ('6e792df9-e51f-4474-9539-40ca2a2b308c',
			'{"action": "create", "effect": "allow", "resource": "ingest:unified_events", "subjects": ["tls:service:automate-cs-nginx:*"]}',
            CURRENT_TIMESTAMP,
            1,
            FALSE)
    ON CONFLICT (id) DO UPDATE
    SET policy_data='{"action": "create", "effect": "allow", "resource": "ingest:unified_events", "subjects": ["tls:service:automate-cs-nginx:*"]}',
        deletable=FALSE;

COMMIT;
