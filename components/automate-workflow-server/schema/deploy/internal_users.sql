-- Deploy internal_users

BEGIN;

CREATE OR REPLACE VIEW internal_users AS
SELECT u.id,
       u.enterprise_id,
       u.name,
       u.ssh_pub_key,
       u.first_name,
       u.last_name,
       u.email,
       p.hashed_pass,
       p.hash_type
FROM users AS u
NATURAL LEFT OUTER JOIN user_passwords AS p
WHERE u.user_type = 'internal';

COMMIT;
