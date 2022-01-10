BEGIN;

UPDATE iam_roles
    SET
        actions = '{
            infra:*:list,
            infra:*:get,
            infra:infraServersOrgsRoles:create,
            infra:infraServersOrgsRoles:update,
            infra:infraServersOrgsRoles:delete,
            infra:infraServersOrgsClient:create,
            infra:infraServersOrgsClient:update,
            infra:infraServersOrgsClient:delete,
            infra:infraServersOrgsDataBags:create,
            infra:infraServersOrgsDataBags:delete,
            infra:infraServersOrgsDataBagsItem:create,
            infra:infraServersOrgsDataBagsItem:update,
            infra:infraServersOrgsDataBagsItem:delete,
            infra:infraServersOrgsEnvironments:create,
            infra:infraServersOrgsEnvironments:update,
            infra:infraServersOrgsEnvironments:delete,
            infra:infraServersOrgsNodes:update,
            infra:infraServersOrgsNodes:delete,
            infra:infraServersOrgsPolicyFiles:delete,
            compliance:*,
            event:*,
            ingest:*,
            secrets:*,
            iam:projects:list,
            iam:projects:get,
            iam:projects:assign,
            iam:policies:list,
            iam:policies:get,
            iam:policyMembers:*,
            iam:teams:list,
            iam:teams:get,
            iam:teamUsers:*,
            iam:users:get,
            iam:users:list,
            applications:*
        }'
    WHERE
        id = 'project-owner';

COMMIT;