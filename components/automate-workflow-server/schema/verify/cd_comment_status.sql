-- Verify cd_comment_status

BEGIN;

-- If the type isn't there, this will fail horribly, as we want.
SELECT 'draft'::cd_comment_status;

ROLLBACK;
