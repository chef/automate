BEGIN;

CREATE OR REPLACE FUNCTION valid_migration_state_change(
  old_state migration_state, new_state migration_state
) RETURNS boolean
LANGUAGE sql AS
$$
  SELECT EXISTS(
    SELECT 1
    FROM (VALUES
      ('init',        'in-progress'),
      ('in-progress', 'failed'),
      ('in-progress', 'successful'),
      ('in-progress', 'successful-beta1'), -- if beta flag set while on v1
      ('successful', 'successful-beta1'), -- if beta flag set while on v2
      ('failed',      'in-progress'),
      ('failed',      'init'),
      ('successful',  'init'),
      ('successful',  'in-progress'),
      ('successful-beta1',  'init'), -- reset back to v1
      ('successful-beta1',  'successful'), -- reset back to regular v2
      ('successful-beta1',  'in-progress')
    ) AS valid(old, new)
    WHERE valid.old::migration_state=old_state
    AND valid.new::migration_state=new_state
  )
$$;

COMMIT;