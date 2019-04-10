-- Deploy stage_ordering

BEGIN;

-- We need to remove the existing ordering information, because it was
-- incorrect. Deleting is OK because no tables have any functional
-- dependencies on the contents of this table.
DELETE FROM stage_ordering;

INSERT INTO stage_ordering(sequence_number, stage, phase, parallel)
VALUES (1, 'verify', 'unit', TRUE),
       (2, 'verify', 'lint', TRUE),
       (3, 'verify', 'syntax', TRUE),

       (4, 'release', 'unit', FALSE),
       (5, 'release', 'lint', FALSE),
       (6, 'release', 'syntax', FALSE),
       (7, 'release', 'quality', FALSE),
       (8, 'release', 'security', FALSE),
       (9, 'release', 'release', FALSE),

       (10, 'acceptance', 'provision', FALSE),
       (11, 'acceptance', 'deploy', FALSE),
       (12, 'acceptance', 'smoke', FALSE),
       (13, 'acceptance', 'functional', FALSE),

       (14, 'union', 'provision', FALSE),
       (15, 'union', 'deploy', FALSE),
       (16, 'union', 'smoke', FALSE),
       (17, 'union', 'functional', FALSE),

       (18, 'rehearsal', 'provision', FALSE),
       (19, 'rehearsal', 'deploy', FALSE),
       (20, 'rehearsal', 'smoke', FALSE),
       (21, 'rehearsal', 'functional', FALSE),

       (22, 'production', 'provision', FALSE),
       (23, 'production', 'deploy', FALSE),
       (24, 'production', 'smoke', FALSE),
       (25, 'production', 'functional', FALSE);

COMMIT;
