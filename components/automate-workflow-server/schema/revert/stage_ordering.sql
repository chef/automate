-- Deploy stage_ordering

BEGIN;

DELETE FROM stage_ordering;

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

COMMIT;
