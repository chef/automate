-- Deploy stage_ordering

BEGIN;

CREATE TABLE stage_ordering(
  sequence_number INTEGER NOT NULL UNIQUE,
  stage TEXT NOT NULL,
  phase TEXT NOT NULL,
  PRIMARY KEY(stage, phase),
  parallel BOOLEAN DEFAULT FALSE
);

INSERT INTO stage_ordering(sequence_number, stage, phase, parallel)
VALUES (1, 'verify', 'unit', TRUE),
       (2, 'verify', 'lint', TRUE),
       (3, 'verify', 'syntax', TRUE),
       (4, 'build', 'unit', FALSE),
       (5, 'build', 'lint', FALSE),
       (6, 'build', 'build', FALSE),
       (7, 'build', 'repository', FALSE),
       (8, 'acceptance', 'provision', FALSE),
       (9, 'acceptance', 'deploy', FALSE),
       (10, 'acceptance', 'smoke', FALSE),
       (11, 'acceptance', 'functional', FALSE),
       (12, 'union', 'provision', FALSE),
       (13, 'union', 'deploy', FALSE),
       (14, 'union', 'smoke', FALSE),
       (15, 'rehearsal', 'provision', FALSE),
       (16, 'rehearsal', 'deploy', FALSE),
       (17, 'rehearsal', 'smoke', FALSE),
       (18, 'showtime', 'provision', FALSE),
       (19, 'showtime', 'deploy', FALSE),
       (20, 'showtime', 'smoke', FALSE);

COMMENT ON TABLE stage_ordering IS

$$This embeds stage and phase ordering in the database, allowing it to
be available for all queries and procedures. This obviates the need to
encode this in the application code.

This table is intentionally not linked by foreign key constraints to
anything else in the system. The information is also not structurally
encoded (as enums, for instance) to allow us to more easily modify
things as we build out the system. We do intend for the stage and
phase information to be "hardcoded", though; because this information
is in a table does not imply that users of the system will be allowed
to add their own stages and phases.
$$;
COMMIT;
