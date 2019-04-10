ALTER TABLE IF EXISTS profiles
  ADD COLUMN IF NOT EXISTS namespace text DEFAULT 'admin',
  ADD COLUMN IF NOT EXISTS name text DEFAULT 'none';

-- Handle URLs of the format 'compliance://mario/linux-patch-baseline#0.3.0'
UPDATE profiles
SET (namespace, name) = (
  SELECT m[1], m[2]
  FROM (
    SELECT regexp_matches(url, '^compliance://([^/]*)/([^/#]*)')
  ) matches(m)
)
WHERE url LIKE 'compliance://%' and namespace = 'admin' and name = 'none' ;

-- Handle URLs of the format 'https://github.com/dev-sec/apache-baseline' and
-- 'https://github.com/dev-sec/linux-baseline/archive/master.tar.gz'
UPDATE profiles
SET (namespace, name) = (
  SELECT m[1], m[2]
  FROM (
    SELECT regexp_matches(url, '^.*://[^/]*/([^/]*)/([^/#]*)')
  ) matches(m)
)
WHERE namespace = 'admin' and name = 'none' ;

UPDATE profiles
SET namespace = 'admin'
WHERE namespace = '' OR namespace is NULL;

UPDATE profiles
SET name = 'name'
WHERE name = '' OR name is NULL;

ALTER TABLE IF EXISTS profiles
  ALTER namespace SET DEFAULT 'admin',
  ALTER COLUMN namespace SET NOT NULL,
  DROP CONSTRAINT IF EXISTS namespace_not_empty,
  ADD CONSTRAINT namespace_not_empty CHECK (trim(namespace) <> ''),
  ALTER name SET DEFAULT 'none',
  ALTER COLUMN name SET NOT NULL,
  DROP CONSTRAINT IF EXISTS name_not_empty,
  ADD CONSTRAINT name_not_empty CHECK (trim(name) <> '');
