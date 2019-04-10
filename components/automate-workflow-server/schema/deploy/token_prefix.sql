-- Deploy token_prefix

BEGIN;

CREATE DOMAIN token_prefix
  AS CHAR(4)
  CONSTRAINT required_length CHECK (length(VALUE) = 4);
   -- TODO: additionally constrain to valid base64 characters?

COMMENT ON DOMAIN token_prefix IS
'Tokens are indexed by a prefix. This domain defines the length of the
prefix for the entire system; no other functions, either in the
database or the application, need know the exact length of the
prefix. Should this need to be changed in the future, this is the only
thing that needs to change.';

COMMIT;
