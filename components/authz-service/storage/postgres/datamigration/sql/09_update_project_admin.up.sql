UPDATE iam_roles
    SET
        actions = '{
          infra:*,
          compliance:*,
          system:*,
          event:*,
          ingest:*,
          secrets:*,
          telemetry:*,
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
          iam:users:list
          }'
    WHERE
        id = 'project-admin';

COMMIT;
