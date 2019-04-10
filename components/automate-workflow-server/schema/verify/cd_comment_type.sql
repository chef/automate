-- Verify cd_comment_type

BEGIN;

-- If the type isn't there, this will fail horribly, as we want.
SELECT 'patchset'::cd_comment_type;

ROLLBACK;
