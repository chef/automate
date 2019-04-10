-- Verify enterprises

BEGIN;

SELECT id,
       name
   FROM enterprises WHERE FALSE;

ROLLBACK;
