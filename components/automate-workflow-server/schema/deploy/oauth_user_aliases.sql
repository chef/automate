-- Deploy delivery:oauth_user_aliases to pg
-- requires: users
-- requires: external_oauth_applications

-- This table is used to track username aliases from external accounts. For example,
-- if you are using the Github integration but your Github username is different
-- from your Delivery username. This table will associate the two so that when
-- your Github username comes in on the Github Payload, we can associate it with
-- your Delivery user.

BEGIN;

CREATE TABLE oauth_user_aliases(
  user_id BIGINT REFERENCES users(id) ON DELETE CASCADE,
  oauth_app_id BIGINT REFERENCES external_oauth_applications(id) ON DELETE CASCADE,
  alias TEXT NOT NULL,
  UNIQUE(oauth_app_id, user_id), -- each user can only have 1 alias per app
  UNIQUE(oauth_app_id, alias) -- there can only be 1 alias per app
);

COMMENT ON TABLE oauth_user_aliases IS
'When integrating with OAuth applications, authorized users may have
 user accounts with those applications where their username differs
 from what they use on delivery. We refer to those usernames as "aliases" and
 this table associates those identities with their Delivery counterparts.';

COMMENT ON COLUMN oauth_user_aliases.user_id IS
'Foreign key to the Delivery user.';

COMMENT ON COLUMN oauth_user_aliases.oauth_app_id IS
'Foreign key to the OAuth application with which the alias is associated.';

COMMENT ON COLUMN oauth_user_aliases.alias IS
'The alias (username) from the OAuth application.';

CREATE OR REPLACE VIEW user_aliases AS
  SELECT u.id,
         u.enterprise_id,
         u.name,
         u.ssh_pub_key,
         u.first_name,
         u.last_name,
         u.email,
         u.user_type,
         o.oauth_app_id,
         o.alias
    FROM users AS u
    JOIN oauth_user_aliases AS o
      ON o.user_id = u.id;

COMMENT ON VIEW user_aliases IS
'A view of all users and their various OAuth aliases.';

COMMIT;
