BEGIN;

UPDATE iam_roles
    SET
        actions = '{
            infra:*:list,
            infra:*:get,
            infra:*:create,
            infra:*:update,
            compliance:*,
            event:*,
            ingest:*,
            secrets:*,
            iam:projects:list,
            iam:projects:get,
            iam:projects:assign,
            applications:*
        }'
    WHERE
        id = 'editor';

COMMIT;