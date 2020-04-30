
-- append "infra:infraServers:list" & "infra:infraServers:get" with "infra:nodes:*" and "infra:nodeManagers:*"
-- for the editor role

UPDATE iam_roles
    SET actions = '{infra:infraServers:list,infra:infraServers:get}' || actions
    WHERE
        id = 'editor' AND
        NOT actions @> '{infra:infraServers:list,infra:infraServers:get}';


-- append "infra:infraServers:list" & "infra:infraServers:get" with "infra:nodes:*" and "infra:nodeManagers:*"
-- for the viewer role
UPDATE iam_roles
    SET actions = '{infra:infraServers:list,infra:infraServers:get}' || actions
    WHERE
        id = 'viewer' AND
        NOT actions @> '{infra:infraServers:list,infra:infraServers:get}';
