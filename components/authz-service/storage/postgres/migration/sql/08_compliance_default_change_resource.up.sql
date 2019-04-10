BEGIN;

UPDATE policies
    SET policy_data='{"action": "read", "effect": "allow", "resource": "compliance:profiles:*", "subjects": ["token:*"]}'
    WHERE id='3db8c368-9cb2-4839-9de4-57ded0de9a55';

UPDATE policies
    SET policy_data='{"action": "upload", "effect": "allow", "resource": "compliance:profiles:*", "subjects": ["token:*"]}'
    WHERE id='0d16e400-e6db-4694-a39c-a4d61f161bae';

COMMIT;
