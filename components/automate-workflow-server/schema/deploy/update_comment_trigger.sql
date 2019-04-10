-- Deploy update_comment_trigger
-- requires: update_comment, comments

BEGIN;

CREATE TRIGGER update_comment
BEFORE UPDATE
ON comments
FOR EACH ROW
EXECUTE PROCEDURE update_comment();

COMMIT;
