-- So this is kinda cool, but also kinda horrible. In order to make it
-- easier to reconcile this set of data with the stuff loaded in from
-- our big dump file, we need to re-do the sequence identifiers, but
-- in a way that we can still sanely manage references to them.
--
-- Hopefully I've struck some kind of balance here using psql
-- variables (see all the `\set` calls throughout).
--
-- Down where we deal with stage and phase runs (whose only identity
-- is these ID numbers), I've taken to using some arithmetic, using a
-- variable for the "last ID" encountered in the dump file, and then
-- just adding onto it. This should make it a bit easier if we ever
-- need to update things in the future.

\set bigco_ent_id 2
\set smallco_ent_id 3
\set externalco_ent_id 4

INSERT INTO enterprises(id, name)
VALUES
  (:bigco_ent_id, 'BigCo'),
  (:smallco_ent_id, 'SmallCo'),
  (:externalco_ent_id, 'ExternalCo')
;

-- Set id sequence appropriately
SELECT setval('enterprises_id_seq', (SELECT max(id) FROM enterprises));

\set bigco_eng_org_id 12
\set smallco_eng_org_id 13

INSERT INTO organizations(id, enterprise_id, name)
VALUES
  (:bigco_eng_org_id, :bigco_ent_id, 'BigCo Engineering'),
  (:smallco_eng_org_id, :smallco_ent_id, 'SmallCo Engineering')
;

SELECT setval('organizations_id_seq', (SELECT max(id) FROM organizations));

\set bigco_eng_skunkworks_proj 34
\set smallco_eng_disruptotron_proj 35
\set bigco_eng_honeybadger_proj 36


INSERT INTO projects(id, organization_id, guid, name)
VALUES
  (:bigco_eng_skunkworks_proj, :bigco_eng_org_id, '2a2fab9b-dbe2-4021-a1eb-a1f593642bfc', 'skunkworks'),
  (:smallco_eng_disruptotron_proj, :smallco_eng_org_id, '45a1a513-48ca-4361-be7a-1838bd119eda', 'disrupt-o-tron'),
  (:bigco_eng_honeybadger_proj, :bigco_eng_org_id, 'f3f8bd73-7bdf-4d78-a7c9-e78197c0438f', 'honeybadger')
;

SELECT setval('projects_id_seq', (SELECT max(id) FROM projects));

\set bigco_eng_skunkworks_master_pipe 24
\set bigco_eng_skunkworks_v1_pipe 25
\set bigco_eng_skunkworks_legacy_pipe 26
\set smallco_eng_disruptotron_master_pipe 27
\set bigco_eng_honeybadger_master_pipe 28

INSERT INTO pipelines(id, project_id, name)
VALUES
  (:bigco_eng_skunkworks_master_pipe, :bigco_eng_skunkworks_proj, 'master'),
  (:bigco_eng_skunkworks_v1_pipe, :bigco_eng_skunkworks_proj, 'v1'),
  (:bigco_eng_skunkworks_legacy_pipe, :bigco_eng_skunkworks_proj, 'legacy'),
  (:smallco_eng_disruptotron_master_pipe, :smallco_eng_disruptotron_proj, 'master'),
  (:bigco_eng_honeybadger_master_pipe, :bigco_eng_honeybadger_proj, 'master')
;

SELECT setval('pipelines_id_seq', (SELECT max(id) FROM pipelines));

\set bigco_admin 54
\set bigco_user 55
\set bigco_chaos_monkey 56
\set smallco_user 57
\set smallco_chaos_monkey 58
\set externalco_user 59
\set bigco_builder 60
\set smallco_admin 61
\set smallco_builder 62

INSERT INTO users(id, enterprise_id, name, user_type)
VALUES
  (:bigco_admin, :bigco_ent_id, 'admin', 'internal'),
  (:bigco_user, :bigco_ent_id, 'BigCo User', 'internal'),
  (:bigco_chaos_monkey, :bigco_ent_id, 'BigCo Chaos Monkey', 'internal'),
  (:smallco_user, :smallco_ent_id, 'SmallCo User', 'internal'),
  (:smallco_chaos_monkey, :smallco_ent_id, 'SmallCo Chaos Monkey', 'internal'),
  (:externalco_user, :externalco_ent_id, 'external_user_is_external', 'external'),
  (:bigco_builder, :bigco_ent_id, 'builder', 'internal'),
  (:smallco_admin, :smallco_ent_id, 'admin', 'internal'),
  (:smallco_builder, :smallco_ent_id, 'builder', 'internal')
;

SELECT setval('users_id_seq', (SELECT max(id) FROM users));

INSERT INTO user_tokens(id, auth_token)
VALUES
  (:bigco_user, 'fake_bigco_token'),
  (:bigco_chaos_monkey, 'big_chaos_monkey_token'),
  (:bigco_chaos_monkey, 'another_big_chaos_monkey_token'),
  (:smallco_user, 'fake_smallco_token'),
  (:smallco_chaos_monkey, 'small_chaos_monkey_token')
;

\set patchset_id_last 1813

-- select '(' || concat_ws(', ', quote_literal(id), pipeline_id, quote_literal(feature_branch), quote_nullable(merge_sha)) || ')' FROM changes;
INSERT INTO changes(id, pipeline_id, feature_branch, merge_sha, title, description, approved_by, changeset_id, latest_patchset_status, latest_patchset, submitted_at, submitted_by, pipeline_name_at_creation)
VALUES
  ('396bd34b-c42a-465e-85bd-a2c49b9908a6', :bigco_eng_skunkworks_master_pipe, 'bcu/feature1', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.33469-07', 'BigCo User', 'master'),
  ('cb761c42-35e2-4e58-936e-27b381c01aab', :bigco_eng_skunkworks_master_pipe, 'bcu/feature2', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.339788-07', 'BigCo User', 'master'),
  ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654', :bigco_eng_skunkworks_master_pipe, 'bcu/feature3', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.339788-07', 'BigCo User', 'master'),
  ('d83f9edd-66b1-44ac-b617-ffefa07660af', :bigco_eng_skunkworks_master_pipe, 'bcu/feature4', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.34324-07', 'BigCo User', 'master'),
  ('1ebc2534-4a43-4efc-95d0-f797842c6a6e', :bigco_eng_skunkworks_master_pipe, 'bcu/feature5', NULL, NULL, NULL, NULL, NULL, 'withdrawn', 3, '2014-08-26 19:07:22.344772-07', 'BigCo User', 'master'),
  ('674802e5-4354-4306-865d-9b7e6c6de2b9', :bigco_eng_skunkworks_master_pipe, 'bcu/feature8', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.34932-07', 'BigCo User', 'master'),
  ('f211b17b-2ad7-49e6-a899-e59ca3d4d1e3', :bigco_eng_skunkworks_master_pipe, 'bcu/feature9', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.350268-07', 'BigCo User', 'master'),
  ('7e1d1db0-e313-4107-a07c-8f62eae5d23f', :bigco_eng_skunkworks_master_pipe, 'bcu/feature10', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.351063-07', 'BigCo User', 'master'),
  ('9e37222a-5159-4754-907d-ead631b9cda2', :bigco_eng_skunkworks_master_pipe, 'bcu/feature11', 'deadbeef111', NULL, NULL, 'admin', NULL, 'merged', 2, '2014-08-26 19:07:22.351843-07', 'BigCo User', 'master'),
  ('453c64fc-6939-4a59-862e-a31e4178a3c6', :bigco_eng_skunkworks_master_pipe, 'bcu/feature12', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.354288-07', 'BigCo User', 'master'),
  ('aa3917ef-71b7-4264-8525-16bc46e367dc', :bigco_eng_skunkworks_master_pipe, 'bcu/feature13', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.355038-07', 'BigCo User', 'master'),
  ('6edcad49-2a53-4db3-9761-fe73f1b72130', :bigco_eng_skunkworks_legacy_pipe, 'bcu/feature13', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.355798-07', 'BigCo User', 'legacy'),
  ('3a389fc9-fb03-4d86-8323-c0e4cf3b1537', :bigco_eng_skunkworks_legacy_pipe, 'bcu/feature14', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.361497-07', 'BigCo User', 'legacy'),
  ('296de263-6773-43ba-a002-bdb8def7db4e', :bigco_eng_skunkworks_legacy_pipe, 'bcu/feature15', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.362672-07', 'BigCo User', 'legacy'),
  ('72f13aef-4469-47dc-83d0-4bd09a98a554', :bigco_eng_skunkworks_v1_pipe, 'bcu/feature16', NULL, NULL, NULL, NULL, NULL, 'open', 1, '2014-08-26 19:07:22.363512-07', 'BigCo User', 'v1'),
  ('6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', :bigco_eng_honeybadger_master_pipe, 'bcu/monkeys', NULL, NULL, NULL, NULL, NULL, 'open', 2, '2014-08-27 18:27:32.02564-07', 'BigCo User', 'master')
;

-- select '(' || concat_ws(', ', id, quote_literal(change_id), sequence_number, quote_literal(submitted_at), quote_literal(sha), submitter_id, quote_nullable(verified_against_sha), is_verified, quote_nullable(status)) || ')' FROM patchsets;
INSERT INTO patchsets(id, change_id, sequence_number, submitted_at, sha, submitter_id, verified_against_sha, is_verified, status)
VALUES
  (:patchset_id_last + 1, '396bd34b-c42a-465e-85bd-a2c49b9908a6', 1, '2014-08-26 19:07:22.33469-07', 'deadbeef1', :bigco_user, NULL, FALSE, 'open'),

  (:patchset_id_last + 2, 'cb761c42-35e2-4e58-936e-27b381c01aab', 1, '2014-08-26 19:07:22.339788-07', 'deadbeef2', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 3, '7e1d8f8c-920f-4f6c-a87c-4ee3fea87654', 1, '2014-08-26 19:07:22.339788-07', 'deadbeef3', :bigco_user, NULL, FALSE, 'open'), -- PATHOLOGICAL CASE: exact same timestamp as the change above

  (:patchset_id_last + 4, 'd83f9edd-66b1-44ac-b617-ffefa07660af', 1, '2014-08-26 19:07:22.34324-07', 'deadbeef4', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 5, '1ebc2534-4a43-4efc-95d0-f797842c6a6e', 1, '2014-08-26 19:07:22.344772-07', 'deadbeef5', :bigco_user, NULL, FALSE, 'superseded'),
  (:patchset_id_last + 6, '1ebc2534-4a43-4efc-95d0-f797842c6a6e', 2, '2014-08-26 19:07:22.346248-07', 'deadbeef51', :bigco_user, NULL, FALSE, 'superseded'),
  (:patchset_id_last + 7, '1ebc2534-4a43-4efc-95d0-f797842c6a6e', 3, '2014-08-26 19:07:22.348075-07', 'deadbeef52', :bigco_user, NULL, FALSE, 'withdrawn'),
  (:patchset_id_last + 8, '674802e5-4354-4306-865d-9b7e6c6de2b9', 1, '2014-08-26 19:07:22.34932-07', 'deadbeef8', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 9, 'f211b17b-2ad7-49e6-a899-e59ca3d4d1e3', 1, '2014-08-26 19:07:22.350268-07', 'deadbeef9', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 10, '7e1d1db0-e313-4107-a07c-8f62eae5d23f', 1, '2014-08-26 19:07:22.351063-07', 'deadbeef10', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 11, '9e37222a-5159-4754-907d-ead631b9cda2', 1, '2014-08-26 19:07:22.351843-07', 'deadbeef11', :bigco_user, NULL, FALSE, 'superseded'),
  (:patchset_id_last + 12, '9e37222a-5159-4754-907d-ead631b9cda2', 2, '2014-08-26 19:07:22.352647-07', 'deadbeef111', :bigco_user, NULL, FALSE, 'merged'),
  (:patchset_id_last + 13, '453c64fc-6939-4a59-862e-a31e4178a3c6', 1, '2014-08-26 19:07:22.354288-07', 'deadbeef12', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 14, 'aa3917ef-71b7-4264-8525-16bc46e367dc', 1, '2014-08-26 19:07:22.355038-07', 'deadbeef13', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 15, '6edcad49-2a53-4db3-9761-fe73f1b72130', 1, '2014-08-26 19:07:22.355798-07', 'deadbeef13', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 16, '3a389fc9-fb03-4d86-8323-c0e4cf3b1537', 1, '2014-08-26 19:07:22.361497-07', 'deadbeef14', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 17, '296de263-6773-43ba-a002-bdb8def7db4e', 1, '2014-08-26 19:07:22.362672-07', 'deadbeef15', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 18, '72f13aef-4469-47dc-83d0-4bd09a98a554', 1, '2014-08-26 19:07:22.363512-07', 'deadbeef16', :bigco_user, NULL, FALSE, 'open'),
  (:patchset_id_last + 19, '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', 1, '2014-08-27 18:27:32.02564-07', 'beefbeef1', :bigco_user, NULL, FALSE, 'superseded'),
  (:patchset_id_last + 20, '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', 2, '2014-08-27 18:29:32.02564-07', 'beefbeef11', :bigco_user, NULL, FALSE, 'open')
  -- Insert a new patchset on the other project's pipeline
;
SELECT setval('patchsets_id_seq', (SELECT max(id) FROM patchsets));

-- Here, we add some dummy data for stage / phase runs for the
-- patchsets. All stages and phases are successful, and each patchset
-- has a corresponding verify run. The one merged change also has a
-- 'build' stage run (though the naming of this may change in the
-- future).
--
-- I didn't actually hand-enter all this information; I used the
-- following queries to populate the tables, based on the patchset
-- information already entered above. I've included them below for
-- posterity
------------------------------------------------------------------------
-- INSERT INTO stage_runs(change_id, stage, status, finished)
-- SELECT change_id, 'verify', 'finished', true
-- FROM patchsets;
--
-- INSERT INTO phase_runs(stage_run_id, phase, status, finished, run_success, run_log, run_status, build_node, search_query, search_description)
-- SELECT sr.id, phases.name, 'finished', TRUE, TRUE, 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1','BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'
-- FROM stage_runs AS sr,
-- (VALUES ('lint'), ('syntax'), ('unit')) AS phases (name);
--
-- INSERT INTO stage_runs(change_id, stage, status, finished)
-- SELECT change_id, 'build', 'finished', true
-- FROM patchsets WHERE status = 'merged';
--
-- INSERT INTO phase_runs(stage_run_id, phase, status, finished, run_success, run_log, run_status, build_node, search_query)
-- SELECT sr.id, phases.name, 'finished', TRUE, TRUE, 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1','BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'
-- FROM stage_runs AS sr,
-- (VALUES ('unit'), ('lint'), ('build'), ('repository')) AS phases (name)
-- WHERE sr.stage = 'build';
------------------------------------------------------------------------

-- select '(' || concat_ws(', ', id, quote_literal(change_id), quote_literal(stage), quote_literal(status), quote_literal(finished)) || '),' FROM stage_runs;
\set stage_runs_id_last 4740

INSERT INTO stage_runs(id, change_id, stage, status, finished)
VALUES
 (:stage_runs_id_last + 1, '396bd34b-c42a-465e-85bd-a2c49b9908a6', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 2, 'cb761c42-35e2-4e58-936e-27b381c01aab', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 3, '7e1d8f8c-920f-4f6c-a87c-4ee3fea87654', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 4, 'd83f9edd-66b1-44ac-b617-ffefa07660af', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 5, '1ebc2534-4a43-4efc-95d0-f797842c6a6e', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 6, '1ebc2534-4a43-4efc-95d0-f797842c6a6e', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 7, '1ebc2534-4a43-4efc-95d0-f797842c6a6e', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 8, '674802e5-4354-4306-865d-9b7e6c6de2b9', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 9, 'f211b17b-2ad7-49e6-a899-e59ca3d4d1e3', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 10, '7e1d1db0-e313-4107-a07c-8f62eae5d23f', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 11, '9e37222a-5159-4754-907d-ead631b9cda2', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 12, '9e37222a-5159-4754-907d-ead631b9cda2', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 13, '453c64fc-6939-4a59-862e-a31e4178a3c6', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 14, 'aa3917ef-71b7-4264-8525-16bc46e367dc', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 15, '6edcad49-2a53-4db3-9761-fe73f1b72130', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 16, '3a389fc9-fb03-4d86-8323-c0e4cf3b1537', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 17, '296de263-6773-43ba-a002-bdb8def7db4e', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 18, '72f13aef-4469-47dc-83d0-4bd09a98a554', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 19, '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 20, '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', 'verify', 'finished', 'true'),
 (:stage_runs_id_last + 21, '9e37222a-5159-4754-907d-ead631b9cda2', 'build', 'finished', 'true');

-- select '(' || concat_ws(', ', id, stage_run_id, quote_literal(phase), quote_literal(status), quote_literal(finished), quote_literal(run_success), quote_literal(run_log), quote_literal(run_status), quote_literal(build_node), quote_literal(search_query), quote_literal(search_description)) || '),' FROM phase_runs;

\set phase_runs_id_last 18010

INSERT INTO phase_runs(id, stage_run_id, phase, status, finished, run_success, run_log, run_status, build_node, search_query, search_description, description)
VALUES
 (:phase_runs_id_last + 1, :stage_runs_id_last + 1, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 2, :stage_runs_id_last + 1, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 3, :stage_runs_id_last + 1, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 4, :stage_runs_id_last + 2, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOND, A DESC)'),
 (:phase_runs_id_last + 5, :stage_runs_id_last + 2, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 6, :stage_runs_id_last + 2, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 7, :stage_runs_id_last + 3, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 8, :stage_runs_id_last + 3, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 9, :stage_runs_id_last + 3, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 10, :stage_runs_id_last + 4, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 11, :stage_runs_id_last + 4, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 12, :stage_runs_id_last + 4, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 13, :stage_runs_id_last + 5, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 14, :stage_runs_id_last + 5, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 15, :stage_runs_id_last + 5, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 16, :stage_runs_id_last + 6, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 17, :stage_runs_id_last + 6, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 18, :stage_runs_id_last + 6, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 19, :stage_runs_id_last + 7, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 20, :stage_runs_id_last + 7, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 21, :stage_runs_id_last + 7, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 22, :stage_runs_id_last + 8, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 23, :stage_runs_id_last + 8, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 24, :stage_runs_id_last + 8, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 25, :stage_runs_id_last + 9, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 26, :stage_runs_id_last + 9, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 27, :stage_runs_id_last + 9, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 28, :stage_runs_id_last + 10, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 29, :stage_runs_id_last + 10, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 30, :stage_runs_id_last + 10, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 31, :stage_runs_id_last + 11, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 32, :stage_runs_id_last + 11, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 33, :stage_runs_id_last + 11, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 34, :stage_runs_id_last + 12, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 35, :stage_runs_id_last + 12, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 36, :stage_runs_id_last + 12, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 37, :stage_runs_id_last + 13, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 38, :stage_runs_id_last + 13, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 39, :stage_runs_id_last + 13, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 40, :stage_runs_id_last + 14, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 41, :stage_runs_id_last + 14, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 42, :stage_runs_id_last + 14, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 43, :stage_runs_id_last + 15, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 44, :stage_runs_id_last + 15, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 45, :stage_runs_id_last + 15, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 46, :stage_runs_id_last + 16, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 47, :stage_runs_id_last + 16, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 48, :stage_runs_id_last + 16, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 49, :stage_runs_id_last + 17, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 50, :stage_runs_id_last + 17, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 51, :stage_runs_id_last + 17, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 52, :stage_runs_id_last + 18, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 53, :stage_runs_id_last + 18, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 54, :stage_runs_id_last + 18, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 55, :stage_runs_id_last + 19, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 56, :stage_runs_id_last + 19, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 57, :stage_runs_id_last + 19, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 58, :stage_runs_id_last + 20, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 59, :stage_runs_id_last + 20, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 60, :stage_runs_id_last + 20, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),

 (:phase_runs_id_last + 61, :stage_runs_id_last + 21, 'unit', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 62, :stage_runs_id_last + 21, 'lint', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 63, :stage_runs_id_last + 21, 'syntax', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 64, :stage_runs_id_last + 21, 'quality', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 65, :stage_runs_id_last + 21, 'security', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
 (:phase_runs_id_last + 66, :stage_runs_id_last + 21, 'publish', 'finished', 'true', 'true', 'BEHOLD, A RUN LOG!', 'BEHOLD, A RUN STATUS!', 'buildnode1', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)');

SELECT setval('stage_runs_id_seq', (SELECT max(id) FROM stage_runs));
SELECT setval('phase_runs_id_seq', (SELECT max(id) FROM phase_runs));
