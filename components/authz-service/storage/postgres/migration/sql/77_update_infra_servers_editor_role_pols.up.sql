-- append "infra:infraServers:*" with "infra:nodes:*" and "infra:nodeManagers:*"
-- for the editor role

UPDATE iam_roles
    SET actions = '{infra:infraServers:*}' || actions
    WHERE
        id = 'editor' AND
        NOT actions @> '{infra:infraServers:*}';
