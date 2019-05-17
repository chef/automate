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
            system:*:get,
            system:*:list,
            event:*:get,
            event:*:list,
            ingest:*:get,
            ingest:*:list,
            iam:projects:list,
            iam:projects:get
        }'
    WHERE
        id = 'viewer';

COMMIT;
