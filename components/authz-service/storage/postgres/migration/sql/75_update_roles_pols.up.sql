-- replaces "infra:*" with "infra:nodes:*" and "infra:nodeManagers:*"
-- removes incorrect action "telemetry:*"
-- removes system permissions
BEGIN;

UPDATE iam_roles
    SET
        actions = '{
            secrets:*:get,
            secrets:*:list,
            infra:nodes:get,
            infra:nodes:list,
            infra:nodeManagers:get,
            infra:nodeManagers:list,
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

UPDATE iam_roles
    SET
        actions = '{
            infra:nodes:*,
            infra:nodeManagers:*,
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

UPDATE iam_roles
    SET
        actions = '{
            infra:nodes:*,
            infra:nodeManagers:*,
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

UPDATE iam_statements
    SET
        actions = '{infra:nodes:*, infra:nodeManagers:*}'
    FROM
        iam_policies
    WHERE
        iam_statements.policy_id = iam_policies.db_id
    AND
        iam_policies.id = 'infrastructure-automation-access-legacy';

COMMIT;
