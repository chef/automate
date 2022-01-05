BEGIN;

UPDATE iam_roles
    SET
        actions = '{
            infra:*,
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