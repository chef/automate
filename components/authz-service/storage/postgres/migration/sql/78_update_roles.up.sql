-- append "reportmanager:*" to the editor, viewer and project-owner role's actions
UPDATE iam_roles
    SET actions = '{reportmanager:*}' || actions
    WHERE
        (id='editor' or id='viewer' or id='project-owner') AND
        NOT actions @> '{reportmanager:*}';
