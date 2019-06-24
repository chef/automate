-- tokens no longer have to have projects
ALTER TABLE chef_authn_tokens DROP CONSTRAINT IF EXISTS chef_authn_tokens_project_ids_check;
