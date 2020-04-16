DO $$
BEGIN
    IF
        (NOT EXISTS(SELECT id FROM iam_policies WHERE id IN ('compliance-viewer-access', 'compliance-editor-access')))
        AND (NOT EXISTS(SELECT id FROM iam_roles WHERE id IN ('compliance-viewer', 'compliance-editor')))
    THEN
        INSERT INTO iam_roles (id, name, type, actions)
            VALUES ('compliance-viewer', 'Compliance Viewer', 'custom', '{compliance:*:get, compliance:*:list}');

        INSERT INTO iam_roles (id, name, type, actions)
            VALUES ('compliance-editor', 'Compliance Editor', 'custom', '{compliance:*}');

        INSERT INTO iam_policies (id, name, type)
            VALUES ('compliance-viewer-access', 'Compliance Viewers', 'custom');

        INSERT INTO iam_policies (id, name, type)
            VALUES ('compliance-editor-access', 'Compliance Editors', 'custom');

        PERFORM insert_iam_statement_into_policy('compliance-viewer-access', 'allow', '{}', '{*}', 'compliance-viewer', '{~~ALL-PROJECTS~~}');

        PERFORM insert_iam_statement_into_policy('compliance-editor-access', 'allow', '{}', '{*}', 'compliance-editor', '{~~ALL-PROJECTS~~}');
    END IF;
END;
$$
LANGUAGE plpgsql;
