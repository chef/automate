BEGIN;

-- update admin policy to use role 'owner'
UPDATE iam_statements
    SET
        role_id = role_db_id('owner'), 
        actions = '{}'
    WHERE
        policy_id=policy_db_id('administrator-access')
    AND
        actions='{*}'
    AND
        effect='allow';

COMMIT;
