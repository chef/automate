-- append "infra:infraServers:list" & "infra:infraServers:get" to the editor and viewer role's actions
UPDATE iam_roles
    SET actions = '{infra:infraServers:list,infra:infraServers:get}' || actions
    WHERE
        (id='editor' or id='viewer') AND
        NOT actions @> '{infra:infraServers:list,infra:infraServers:get}';
