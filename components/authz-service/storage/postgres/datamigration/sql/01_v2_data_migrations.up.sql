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

COMMIT;
