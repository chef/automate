BEGIN;

UPDATE iam_roles
    SET
        actions = '{
            infra:*:list,
            infra:*:get,
            infra:infraServersOrgsRoles:create,
            infra:infraServersOrgsRoles:update,
            infra:infraServersOrgsClient:create,
            infra:infraServersOrgsClient:update,
            infra:infraServersOrgsDataBags:create,
            infra:infraServersOrgsDataBagsItem:create,
            infra:infraServersOrgsDataBagsItem:update,
            infra:infraServersOrgsEnvironments:create,
            infra:infraServersOrgsEnvironments:update,
            infra:infraServersOrgsNodes:update,
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