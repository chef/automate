BEGIN;

SET CONSTRAINTS iam_policy_members_policy_id_fkey DEFERRED;

UPDATE iam_members
    SET
        name = 'team:local:editors'
    WHERE
        name = 'team:local:operators';

UPDATE iam_statements
    SET resources = array_replace(resources, 'team:local:operator', 'team:local:editor');

UPDATE iam_roles
    SET
        id = 'editor',
        name = 'Editor'
    WHERE
        id = 'operator';

UPDATE iam_policies SET id='editor-access', name='Editors' WHERE id='operator-access';

UPDATE iam_roles
    SET
        actions = actions || '{ secrets:*:get, secrets:*:list }'
    WHERE
        id = 'viewer';

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

UPDATE iam_roles
    SET
        id = 'owner',
        name = 'Owner'
    WHERE
        id = 'all-actions';

INSERT INTO iam_projects (id, name, type)
    VALUES ('(unassigned)', 'Unassigned', 'chef-managed')
    ON CONFLICT DO NOTHING;

-- Define the 'all projects' project
INSERT INTO iam_projects (id, name, type)
    VALUES ('~~ALL-PROJECTS~~', 'All Projects', 'chef-managed')
    ON CONFLICT DO NOTHING;

-- For any statements lacking projects, provide the default of 'all projects'
INSERT INTO iam_statement_projects (statement_id, project_id)
    SELECT db_id, project_db_id('~~ALL-PROJECTS~~') FROM iam_statements
    WHERE db_id NOT IN (SELECT statement_id FROM iam_statement_projects);

DELETE FROM iam_projects WHERE id='default';

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

COMMMIT;