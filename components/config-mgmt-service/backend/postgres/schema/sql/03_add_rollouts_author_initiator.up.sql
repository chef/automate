ALTER TABLE rollouts ADD COLUMN scm_author_name TEXT NOT NULL DEFAULT '';
ALTER TABLE rollouts ADD COLUMN scm_author_email TEXT NOT NULL DEFAULT '';
ALTER TABLE rollouts ADD COLUMN policy_domain_username TEXT NOT NULL DEFAULT '';

