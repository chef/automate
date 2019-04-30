BEGIN;
CREATE TABLE policy_change_tracker (
  policy_change_id uuid DEFAULT uuid_generate_v4()
);
INSERT INTO policy_change_tracker VALUES( uuid_generate_v4() );
COMMIT;
