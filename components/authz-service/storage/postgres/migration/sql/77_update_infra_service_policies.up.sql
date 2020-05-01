
-- append "infra:infraServers:list" & "infra:infraServers:get" to the editor role's actions
UPDATE iam_roles
    SET actions = '{infra:infraServers:list,infra:infraServers:get}' || actions
    WHERE
        id = 'editor' AND
        NOT actions @> '{infra:infraServers:list,infra:infraServers:get}';


-- append "infra:infraServers:list" & "infra:infraServers:get" to the viewer role's actions
UPDATE iam_roles
    SET actions = '{infra:infraServers:list,infra:infraServers:get}' || actions
    WHERE
        id = 'viewer' AND
        NOT actions @> '{infra:infraServers:list,infra:infraServers:get}';
