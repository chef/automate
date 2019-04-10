ALTER TABLE IF EXISTS profiles
  ADD COLUMN IF NOT EXISTS namespace text,
  ADD COLUMN IF NOT EXISTS name text;

-- Handle URLs of the format 'compliance://mario/linux-patch-baseline#0.3.0'
UPDATE profiles
SET (namespace, name) = (
  SELECT m[1], m[2]
  FROM (
    SELECT regexp_matches(url, '^compliance://([^/]*)/([^/#]*)')
  ) matches(m)
)
WHERE url LIKE 'compliance://%' ;

-- Handle URLs of the format 'https://github.com/dev-sec/apache-baseline' and
-- 'https://github.com/dev-sec/linux-baseline/archive/master.tar.gz'
UPDATE profiles
SET (namespace, name) = (
  SELECT m[1], m[2]
  FROM (
    SELECT regexp_matches(url, '^.*://[^/]*/([^/]*)/([^/#]*)')
  ) matches(m)
)
WHERE namespace IS NULL AND name IS NULL ;

ALTER TABLE IF EXISTS profiles
  ALTER COLUMN namespace SET NOT NULL,
  ADD CONSTRAINT namespace_not_empty CHECK (trim(namespace) <> ''),
  ALTER COLUMN name SET NOT NULL,
  ADD CONSTRAINT name_not_empty CHECK (trim(name) <> '');
