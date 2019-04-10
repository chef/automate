-- Test functions that use prepared statements (e.g., those that call
-- 'throws_ok' and friends) would either need to:
--
-- a) manually deallocate their prepared statements, or
--
-- b) make sure that their prepared statement names are globally
--    unique, forever and always
--
-- Both of those kind of suck, so instead we'll just issue a blanket
-- DEALLOCATE command before each test is run!
--
-- (NOTE: you can still manually deallocate within a test function if
-- you like, and if it helps keep you safe from dumb copy-paste
-- errors and the like, but it's not required.)
CREATE OR REPLACE FUNCTION setup_01_deallocate_prepared_statements()
RETURNS VOID
LANGUAGE SQL
AS $$
  DEALLOCATE ALL;
$$;
