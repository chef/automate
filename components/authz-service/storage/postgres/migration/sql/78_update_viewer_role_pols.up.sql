-- replaces "infra:nodes:*" and "infra:nodeManagers:*"  with infra:*:get, infra:*:list
BEGIN;

UPDATE iam_roles
    SET
        actions = '{
            secrets:*:get,
            secrets:*:list,
            infra:*:get,
            infra:*:list,
            compliance:*:get,
            compliance:*:list,
            event:*:get,
            event:*:list,
            ingest:*:get,
            ingest:*:list,
            iam:projects:list,
            iam:projects:get,
            applications:*:get,
            applications:*:list
        }'
    WHERE
        id = 'viewer';
        
COMMIT;