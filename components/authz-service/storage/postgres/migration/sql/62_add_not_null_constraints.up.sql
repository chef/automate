DELETE FROM iam_policy_members WHERE member_id IS NULL;
ALTER TABLE iam_policy_members ALTER COLUMN member_id SET NOT NULL;
DELETE FROM iam_policy_members WHERE policy_id IS NULL;
ALTER TABLE iam_policy_members ALTER COLUMN policy_id SET NOT NULL;

DELETE FROM iam_policy_projects WHERE policy_id IS NULL;
ALTER TABLE iam_policy_projects ALTER COLUMN policy_id SET NOT NULL;

DELETE FROM iam_project_rules WHERE project_id IS NULL;
ALTER TABLE iam_project_rules ALTER COLUMN project_id SET NOT NULL;

-- if any iam_projects are somehow missing themselves in their projects array,
-- update it to '{THEIR_ID}'.
UPDATE iam_projects p1
  SET projects = ARRAY[p2.id]
  FROM iam_projects p2
  WHERE p1.id = p2.id AND (p1.projects IS NULL OR p1.projects = '{}');
ALTER TABLE iam_projects ALTER COLUMN projects SET NOT NULL;

DELETE FROM iam_role_projects WHERE project_id IS NULL;
ALTER TABLE iam_role_projects ALTER COLUMN project_id SET NOT NULL;

-- we have validation here so nothing should be updated
DELETE FROM iam_roles WHERE actions IS NULL;
ALTER TABLE iam_roles ALTER COLUMN actions SET NOT NULL;

DELETE FROM iam_rule_conditions WHERE rule_db_id IS NULL;
ALTER TABLE iam_rule_conditions ALTER COLUMN rule_db_id SET NOT NULL;

DELETE FROM iam_staged_project_rules WHERE project_id IS NULL;
ALTER TABLE iam_staged_project_rules ALTER COLUMN project_id SET NOT NULL;

DELETE FROM iam_staged_rule_conditions WHERE rule_db_id IS NULL;
ALTER TABLE iam_staged_rule_conditions ALTER COLUMN rule_db_id SET NOT NULL;

DELETE FROM iam_statement_projects WHERE statement_id IS NULL;
ALTER TABLE iam_statement_projects ALTER COLUMN statement_id SET NOT NULL;
DELETE FROM iam_statement_projects WHERE project_id IS NULL;
ALTER TABLE iam_statement_projects ALTER COLUMN project_id SET NOT NULL;

CREATE OR REPLACE FUNCTION purge_statements_with_no_projects() RETURNS TRIGGER AS $$
  BEGIN
    -- delete all statements that now have no projects
    DELETE FROM iam_statements s
      WHERE NOT EXISTS (SELECT 1 FROM iam_statement_projects p WHERE s.db_id = p.statement_id AND p.project_id IS NOT NULL);

    DELETE FROM iam_policies p
        WHERE NOT EXISTS (SELECT 1 FROM iam_statements s WHERE p.db_id = s.policy_id)
        AND p.type != 'chef-managed';

    RETURN NULL;
  END
$$
LANGUAGE plpgsql;

-- run what is in our cleanup function once
DELETE FROM iam_statements s
  WHERE NOT EXISTS (SELECT 1 FROM iam_statement_projects p WHERE s.db_id = p.statement_id AND p.project_id IS NOT NULL);

DELETE FROM iam_policies p
    WHERE NOT EXISTS (SELECT 1 FROM iam_statements s WHERE p.db_id = s.policy_id)
    AND p.type != 'chef-managed';

CREATE TRIGGER on_project_deletion AFTER DELETE ON iam_projects
FOR EACH ROW
EXECUTE PROCEDURE purge_statements_with_no_projects();
