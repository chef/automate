UPDATE iam_roles
    SET actions = '{compliance:reports:export}' || actions
    WHERE
        (id='compliance-viewer' or id='viewer') AND
        NOT actions @> '{compliance:reports:export}';