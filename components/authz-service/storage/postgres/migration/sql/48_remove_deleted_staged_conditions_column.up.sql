BEGIN;

ALTER TABLE iam_staged_rule_conditions DROP COLUMN deleted CASCADE;

COMMIT;