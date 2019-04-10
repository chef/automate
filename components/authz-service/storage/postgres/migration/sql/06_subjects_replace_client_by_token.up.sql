BEGIN;

-- When given an array, this function will replace `old` by `new`. See below for
-- a usage example.
-- Adapted from https://stackoverflow.com/a/42281999
CREATE OR REPLACE FUNCTION replace_array_elements(arr jsonb, old text, new text)
RETURNS jsonb LANGUAGE SQL AS $$
    SELECT jsonb_agg(replace(e, old, new))
    FROM jsonb_array_elements_text(arr) e(e)
$$;

-- For all policies in the database, replace "client:" by "token:" in subjects.
-- This should both affect customer policies and default policies shipped by us.
-- We can't assume that all default policies are still in the system, so this is
-- what we do: replace the string "client:" by "token:". This will also affect
-- wildcard policies: "client:*" becomes "token:*".
UPDATE policies
    SET policy_data=jsonb_set(policy_data,
                              '{subjects}',
                              replace_array_elements(policy_data->'subjects',
                                                     'client:',
                                                     'token:'));

COMMIT;
