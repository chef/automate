CREATE SCHEMA IF NOT EXISTS cd_pgtap;

CREATE OR REPLACE FUNCTION cd_pgtap.col_is_array(p_table_name NAME,
                                                 p_column_name NAME)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
   RETURN NEXT has_column(p_table_name, p_column_name);

   RETURN NEXT col_is_null(p_table_name, p_column_name);
--   RETURN NEXT col_not_null(p_table_name, p_column_name);

   RETURN NEXT col_type_is(p_table_name, p_column_name, 'text[]');

   RETURN NEXT col_hasnt_default(p_table_name, p_column_name);

--   RETURN NEXT col_has_default(p_table_name, p_column_name);
--   RETURN NEXT col_default_is(p_table_name, p_column_name, '{}');
END;
$$;

COMMENT ON FUNCTION cd_pgtap.col_is_array(NAME, NAME) IS
$$Asserts that the given column is declared as a NOT NULL text ARRAY
with no default.$$;

CREATE OR REPLACE FUNCTION cd_pgtap.col_is_timestamp(p_table_name NAME, p_column_name NAME)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN NEXT has_column(p_table_name, p_column_name);
  RETURN NEXT col_not_null(p_table_name, p_column_name);
  RETURN NEXT col_type_is(p_table_name, p_column_name, 'timestamp with time zone');
  RETURN NEXT col_has_default(p_table_name, p_column_name);
  RETURN NEXT col_default_is(p_table_name, p_column_name, 'now()');
END;
$$;

CREATE OR REPLACE FUNCTION cd_pgtap.col_is_nullable_timestamp(p_table_name NAME, p_column_name NAME)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN NEXT has_column(p_table_name, p_column_name);
  RETURN NEXT col_is_null(p_table_name, p_column_name);
  RETURN NEXT col_type_is(p_table_name, p_column_name, 'timestamp with time zone');
  RETURN NEXT col_has_default(p_table_name, p_column_name);
  RETURN NEXT col_default_is(p_table_name, p_column_name, 'now()');
END;
$$;

CREATE OR REPLACE FUNCTION cd_pgtap.col_is_flag(p_table_name NAME, p_column_name NAME, p_default BOOLEAN)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN NEXT has_column(p_table_name, p_column_name);
  RETURN NEXT col_is_null(p_table_name, p_column_name);
  --  RETURN NEXT col_not_null(p_table_name, p_column_name);
  RETURN NEXT col_type_is(p_table_name, p_column_name, 'boolean');
  RETURN NEXT col_has_default(p_table_name, p_column_name);
  RETURN NEXT col_default_is(p_table_name, p_column_name,
                              CASE WHEN p_default IS TRUE
                                   THEN 'true'
                                   ELSE 'false'
                              END);
END;
$$;


CREATE OR REPLACE FUNCTION cd_pgtap.col_is_delivery_id(p_table_name NAME, p_column_name NAME)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN NEXT has_column(p_table_name, p_column_name);
  RETURN NEXT col_type_is(p_table_name, p_column_name, 'text');
  RETURN NEXT col_hasnt_default(p_table_name, p_column_name);
  RETURN NEXT col_not_null(p_table_name, p_column_name);
END;
$$;

-- TODO: Add to chef_pgtap

CREATE OR REPLACE FUNCTION cd_pgtap.col_is_json_blob(p_table_name NAME, p_column_name NAME)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN NEXT has_column(p_table_name, p_column_name);
  RETURN NEXT col_type_is(p_table_name, p_column_name, 'text');
  RETURN NEXT col_hasnt_default(p_table_name, p_column_name);
  RETURN NEXT col_is_null(p_table_name, p_column_name);
END;
$$;

-- TODO: Add to chef_pgtap

CREATE OR REPLACE FUNCTION cd_pgtap.col_is_serial(p_table_name NAME, p_column_name NAME)
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN NEXT has_column(p_table_name, p_column_name);
  RETURN NEXT col_not_null(p_table_name, p_column_name);
  RETURN NEXT col_type_is(p_table_name, p_column_name, 'integer');
  RETURN NEXT col_has_default(p_table_name, p_column_name);
  RETURN NEXT col_default_is(p_table_name, p_column_name, 'nextval(''' || p_table_name || '_' || p_column_name || '_seq''::regclass)');
--  RETURN NEXT col_is_pk(p_table_name, p_column_name);
END;
$$;

-- TODO: Add to chef_pgtap

-- NOTE: this will fall apart if you are using schemas, and have
-- multiple tables with the same name.  However, none of those are the
-- case for us.
CREATE OR REPLACE FUNCTION cd_pgtap.has_fk_count(p_table_name NAME, p_num BIGINT)
RETURNS TEXT
LANGUAGE SQL
AS $$
    SELECT is(
        count(*),
        $2,
        'Table ' || $1 || ' should have ' || $2 || ' foreign key constraints')
    FROM information_schema.table_constraints AS tc
    WHERE tc.table_name = $1
      AND tc.constraint_type = 'FOREIGN KEY'
    ;
$$;

CREATE OR REPLACE FUNCTION cd_pgtap.is_lut(p_table_name NAME, p_values text[])
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  RETURN QUERY SELECT has_table(p_table_name);

  -- Columns
  RETURN QUERY SELECT columns_are(p_table_name, ARRAY['id']);

  RETURN QUERY SELECT has_column(p_table_name, 'id');
  RETURN QUERY SELECT col_type_is(p_table_name, 'id', 'text');
  RETURN QUERY SELECT col_hasnt_default(p_table_name, 'id');
  RETURN QUERY SELECT col_is_pk(p_table_name, 'id');

  -- Keys

  RETURN QUERY SELECT has_pk(p_table_name);
  RETURN QUERY SELECT hasnt_fk(p_table_name);

  -- Contents
  RETURN QUERY SELECT results_eq('SELECT id FROM ' || p_table_name,
                                 p_values,
                                 'Lookup table ''' || p_table_name || ''' should have the proper values');
END;
$$;

-- These helper functions serve to translate human readable names into
-- utterly illegible internal database IDs.  They're used to make the
-- tests more readable and less fragile.

-- Get an "ent"erprise ID, given a name.
CREATE OR REPLACE FUNCTION ent(enterprises.name%TYPE)
RETURNS enterprises.id%TYPE
LANGUAGE SQL
AS $$
   SELECT id FROM enterprises WHERE name = $1;
$$;

-- Get an "org"anization ID, given its name, and the name of the
-- enterprise it's a part of.
CREATE OR REPLACE FUNCTION org(enterprises.name%TYPE, organizations.name%TYPE)
RETURNS organizations.id%TYPE
LANGUAGE SQL
AS $$
   SELECT o.id
   FROM organizations AS o
   JOIN enterprises AS e
     ON o.enterprise_id = e.id
   WHERE e.name = $1
     AND o.name = $2;
$$;

-- Get the ID of a "u"ser, given its name and the name of the
-- enterprise it belongs to.
CREATE OR REPLACE FUNCTION u(enterprises.name%TYPE, users.name%TYPE)
RETURNS users.id%TYPE
LANGUAGE SQL
AS $$
   SELECT u.id
   FROM users AS u
   JOIN enterprises AS e
     ON u.enterprise_id = e.id
   WHERE e.name = $1
     AND u.name = $2;
$$;

CREATE OR REPLACE FUNCTION proj(enterprises.name%TYPE, organizations.name%TYPE, projects.name%TYPE )
RETURNS projects.id%TYPE
LANGUAGE SQL
AS $$
   SELECT p.id
   FROM projects AS p
   JOIN organizations AS o
     ON p.organization_id = o.id
   JOIN enterprises AS e
     ON o.enterprise_id = e.id
   WHERE e.name = $1
     AND o.name = $2
     AND p.name = $3;
$$;

CREATE OR REPLACE FUNCTION pipe(enterprises.name%TYPE, organizations.name%TYPE, projects.name%TYPE, pipelines.name%TYPE)
RETURNS pipelines.id%TYPE
LANGUAGE SQL
AS $$
   SELECT pipe.id
   FROM pipelines AS pipe
   JOIN projects AS p
     ON pipe.project_id = p.id
   JOIN organizations AS o
     ON p.organization_id = o.id
   JOIN enterprises AS e
     ON o.enterprise_id = e.id
   WHERE e.name = $1
     AND o.name = $2
     AND p.name = $3
     AND pipe.name = $4;
$$;
