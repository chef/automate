BEGIN;

INSERT INTO iam_roles (id, name, type, actions)
  VALUES ('project-admin', 'Project Admin', 'chef-managed',
    '{infra:*,compliance:*,system:*,event:*,ingest:*,secrets:*,telemetry:*,iam:projects:list,iam:projects:get,iam:projects:assign,iam:policies:list,iam:policies:get,iam:policyMembers:*,iam:teams:list,iam:teams:get,iam:teamUsers:*}')
  ON CONFLICT (id) DO UPDATE
    SET name='Project Admin',
        type='chef-managed',
        actions='{infra:*,compliance:*,system:*,event:*,ingest:*,secrets:*,telemetry:*,iam:projects:list,iam:projects:get,iam:projects:assign,iam:policies:list,iam:policies:get,iam:policyMembers:*,iam:teams:list,iam:teams:get,iam:teamUsers:*}';

INSERT INTO iam_roles (id, name, type, actions)
  VALUES ('iam-members-viewer', 'IAM Members Viewer', 'chef-managed',
    '{iam:users:get, iam:users:list, iam:teams:get, iam:teams:list, iam:tokens:get, iam:tokens:list}')
  ON CONFLICT (id) DO UPDATE
    SET name='IAM Members Viewer',
        type='chef-managed',
        actions= '{iam:users:get, iam:users:list, iam:teams:get, iam:teams:list, iam:tokens:get, iam:tokens:list}';

UPDATE iam_roles
    SET
        actions = actions || '{iam:projects:list, iam:projects:get}'
    WHERE
        id = 'viewer';

UPDATE iam_roles
    SET
        actions = actions || '{iam:projects:list, iam:projects:get, iam:projects:assign}'
    WHERE
        id = 'editor';

COMMIT;
