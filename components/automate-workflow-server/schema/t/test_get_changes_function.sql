CREATE OR REPLACE FUNCTION test_get_changes_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  -- All the tests are working against the same project. We'll just
  -- embed all that information here in a prepared statement; all
  -- parameters are for optional filtering / sorting
  PREPARE get_skunkworks_changes AS
    SELECT id,feature_branch,pipeline_name_at_creation,latest_patchset_status,submitted_at,submitted_by,merge_sha
         FROM get_changes('BigCo', 'BigCo Engineering', 'skunkworks', $1, $2, $3, $4, $5);

  -- Now, we'll prepare a bunch of queries representing the expected
  -- results we should see in our tests. Some of these get reused in
  -- several places. In any event, it's much nicer to declare them as
  -- prepared statements than to have them inline with the tests as
  -- strings... blech.
  PREPARE first_page_no_filters AS
  VALUES
    ('396bd34b-c42a-465e-85bd-a2c49b9908a6'::uuid, 'bcu/feature1', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.33469-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654'::uuid, 'bcu/feature3', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('cb761c42-35e2-4e58-936e-27b381c01aab'::uuid, 'bcu/feature2', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('d83f9edd-66b1-44ac-b617-ffefa07660af'::uuid, 'bcu/feature4', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34324-07'::cd_timestamp, 'BigCo User', NULL),
    ('1ebc2534-4a43-4efc-95d0-f797842c6a6e'::uuid, 'bcu/feature5', 'master', 'withdrawn'::cd_patchset_status, '2014-08-26 19:07:22.344772-07'::cd_timestamp, 'BigCo User', NULL),
    ('674802e5-4354-4306-865d-9b7e6c6de2b9'::uuid, 'bcu/feature8', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34932-07'::cd_timestamp, 'BigCo User', NULL),
    ('f211b17b-2ad7-49e6-a899-e59ca3d4d1e3'::uuid, 'bcu/feature9', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.350268-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d1db0-e313-4107-a07c-8f62eae5d23f'::uuid, 'bcu/feature10', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.351063-07'::cd_timestamp, 'BigCo User', NULL),
    ('9e37222a-5159-4754-907d-ead631b9cda2'::uuid, 'bcu/feature11', 'master', 'merged'::cd_patchset_status, '2014-08-26 19:07:22.351843-07'::cd_timestamp, 'BigCo User','deadbeef111'),
    ('453c64fc-6939-4a59-862e-a31e4178a3c6'::uuid, 'bcu/feature12', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.354288-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE second_page_no_filters AS
  VALUES
    ('aa3917ef-71b7-4264-8525-16bc46e367dc'::uuid, 'bcu/feature13', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355038-07'::cd_timestamp, 'BigCo User', NULL),
    ('6edcad49-2a53-4db3-9761-fe73f1b72130'::uuid, 'bcu/feature13', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355798-07'::cd_timestamp, 'BigCo User', NULL),
    ('3a389fc9-fb03-4d86-8323-c0e4cf3b1537'::uuid, 'bcu/feature14', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.361497-07'::cd_timestamp, 'BigCo User', NULL),
    ('296de263-6773-43ba-a002-bdb8def7db4e'::uuid, 'bcu/feature15', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.362672-07'::cd_timestamp, 'BigCo User', NULL),
    ('72f13aef-4469-47dc-83d0-4bd09a98a554'::uuid, 'bcu/feature16', 'v1', 'open'::cd_patchset_status, '2014-08-26 19:07:22.363512-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE first_page_of_five AS
  VALUES
    ('396bd34b-c42a-465e-85bd-a2c49b9908a6'::uuid, 'bcu/feature1', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.33469-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654'::uuid, 'bcu/feature3', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('cb761c42-35e2-4e58-936e-27b381c01aab'::uuid, 'bcu/feature2', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('d83f9edd-66b1-44ac-b617-ffefa07660af'::uuid, 'bcu/feature4', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34324-07'::cd_timestamp, 'BigCo User', NULL),
    ('1ebc2534-4a43-4efc-95d0-f797842c6a6e'::uuid, 'bcu/feature5', 'master', 'withdrawn'::cd_patchset_status, '2014-08-26 19:07:22.344772-07'::cd_timestamp, 'BigCo User', NULL)
  ;

 PREPARE first_page_of_twelve AS
  VALUES
    ('396bd34b-c42a-465e-85bd-a2c49b9908a6'::uuid, 'bcu/feature1', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.33469-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654'::uuid, 'bcu/feature3', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('cb761c42-35e2-4e58-936e-27b381c01aab'::uuid, 'bcu/feature2', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('d83f9edd-66b1-44ac-b617-ffefa07660af'::uuid, 'bcu/feature4', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34324-07'::cd_timestamp, 'BigCo User', NULL),
    ('1ebc2534-4a43-4efc-95d0-f797842c6a6e'::uuid, 'bcu/feature5', 'master', 'withdrawn'::cd_patchset_status, '2014-08-26 19:07:22.344772-07'::cd_timestamp, 'BigCo User', NULL),
    ('674802e5-4354-4306-865d-9b7e6c6de2b9'::uuid, 'bcu/feature8', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34932-07'::cd_timestamp, 'BigCo User', NULL),
    ('f211b17b-2ad7-49e6-a899-e59ca3d4d1e3'::uuid, 'bcu/feature9', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.350268-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d1db0-e313-4107-a07c-8f62eae5d23f'::uuid, 'bcu/feature10', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.351063-07'::cd_timestamp, 'BigCo User', NULL),
    ('9e37222a-5159-4754-907d-ead631b9cda2'::uuid, 'bcu/feature11', 'master', 'merged'::cd_patchset_status, '2014-08-26 19:07:22.351843-07'::cd_timestamp, 'BigCo User', 'deadbeef111'),
    ('453c64fc-6939-4a59-862e-a31e4178a3c6'::uuid, 'bcu/feature12', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.354288-07'::cd_timestamp, 'BigCo User', NULL),
    ('aa3917ef-71b7-4264-8525-16bc46e367dc'::uuid, 'bcu/feature13', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355038-07'::cd_timestamp, 'BigCo User', NULL),
    ('6edcad49-2a53-4db3-9761-fe73f1b72130'::uuid, 'bcu/feature13', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355798-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE first_page_reverse_chronological AS
  VALUES
    ('72f13aef-4469-47dc-83d0-4bd09a98a554'::uuid, 'bcu/feature16', 'v1', 'open'::cd_patchset_status, '2014-08-26 19:07:22.363512-07'::cd_timestamp, 'BigCo User', NULL),
    ('296de263-6773-43ba-a002-bdb8def7db4e'::uuid, 'bcu/feature15', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.362672-07'::cd_timestamp, 'BigCo User', NULL),
    ('3a389fc9-fb03-4d86-8323-c0e4cf3b1537'::uuid, 'bcu/feature14', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.361497-07'::cd_timestamp, 'BigCo User', NULL),
    ('6edcad49-2a53-4db3-9761-fe73f1b72130'::uuid, 'bcu/feature13', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355798-07'::cd_timestamp, 'BigCo User', NULL),
    ('aa3917ef-71b7-4264-8525-16bc46e367dc'::uuid, 'bcu/feature13', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355038-07'::cd_timestamp, 'BigCo User', NULL),
    ('453c64fc-6939-4a59-862e-a31e4178a3c6'::uuid, 'bcu/feature12', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.354288-07'::cd_timestamp, 'BigCo User', NULL),
    ('9e37222a-5159-4754-907d-ead631b9cda2'::uuid, 'bcu/feature11', 'master', 'merged'::cd_patchset_status, '2014-08-26 19:07:22.351843-07'::cd_timestamp, 'BigCo User', 'deadbeef111'),
    ('7e1d1db0-e313-4107-a07c-8f62eae5d23f'::uuid, 'bcu/feature10', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.351063-07'::cd_timestamp, 'BigCo User', NULL),
    ('f211b17b-2ad7-49e6-a899-e59ca3d4d1e3'::uuid, 'bcu/feature9', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.350268-07'::cd_timestamp, 'BigCo User', NULL),
    ('674802e5-4354-4306-865d-9b7e6c6de2b9'::uuid, 'bcu/feature8', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34932-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE all_v1_changes AS
  VALUES
    ('72f13aef-4469-47dc-83d0-4bd09a98a554'::uuid, 'bcu/feature16', 'v1', 'open'::cd_patchset_status, '2014-08-26 19:07:22.363512-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE all_legacy_changes AS
  VALUES
    ('6edcad49-2a53-4db3-9761-fe73f1b72130'::uuid, 'bcu/feature13', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355798-07'::cd_timestamp, 'BigCo User', NULL),
    ('3a389fc9-fb03-4d86-8323-c0e4cf3b1537'::uuid, 'bcu/feature14', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.361497-07'::cd_timestamp, 'BigCo User', NULL),
    ('296de263-6773-43ba-a002-bdb8def7db4e'::uuid, 'bcu/feature15', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.362672-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE open_legacy_chronological_page_1_size_2 AS
  VALUES
    ('6edcad49-2a53-4db3-9761-fe73f1b72130'::uuid, 'bcu/feature13', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355798-07'::cd_timestamp, 'BigCo User', NULL),
    ('3a389fc9-fb03-4d86-8323-c0e4cf3b1537'::uuid, 'bcu/feature14', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.361497-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE open_legacy_chronological_page_2_size_2 AS
  VALUES
    ('296de263-6773-43ba-a002-bdb8def7db4e'::uuid, 'bcu/feature15', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.362672-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  PREPARE merged_master_changes AS
  VALUES
    ('9e37222a-5159-4754-907d-ead631b9cda2'::uuid, 'bcu/feature11', 'master', 'merged'::cd_patchset_status, '2014-08-26 19:07:22.351843-07'::cd_timestamp, 'BigCo User', 'deadbeef111')
  ;

  PREPARE withdrawn_master_changes AS
  VALUES
    ('1ebc2534-4a43-4efc-95d0-f797842c6a6e'::uuid, 'bcu/feature5', 'master', 'withdrawn'::cd_patchset_status, '2014-08-26 19:07:22.344772-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  -- sorted in chronological order
  PREPARE everything AS
  VALUES
    ('396bd34b-c42a-465e-85bd-a2c49b9908a6'::uuid, 'bcu/feature1', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.33469-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654'::uuid, 'bcu/feature3', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('cb761c42-35e2-4e58-936e-27b381c01aab'::uuid, 'bcu/feature2', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('d83f9edd-66b1-44ac-b617-ffefa07660af'::uuid, 'bcu/feature4', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34324-07'::cd_timestamp, 'BigCo User', NULL),
    ('1ebc2534-4a43-4efc-95d0-f797842c6a6e'::uuid, 'bcu/feature5', 'master', 'withdrawn'::cd_patchset_status, '2014-08-26 19:07:22.344772-07'::cd_timestamp, 'BigCo User', NULL),
    ('674802e5-4354-4306-865d-9b7e6c6de2b9'::uuid, 'bcu/feature8', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34932-07'::cd_timestamp, 'BigCo User', NULL),
    ('f211b17b-2ad7-49e6-a899-e59ca3d4d1e3'::uuid, 'bcu/feature9', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.350268-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d1db0-e313-4107-a07c-8f62eae5d23f'::uuid, 'bcu/feature10', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.351063-07'::cd_timestamp, 'BigCo User', NULL),
    ('9e37222a-5159-4754-907d-ead631b9cda2'::uuid, 'bcu/feature11', 'master', 'merged'::cd_patchset_status, '2014-08-26 19:07:22.351843-07'::cd_timestamp, 'BigCo User', 'deadbeef111'),
    ('453c64fc-6939-4a59-862e-a31e4178a3c6'::uuid, 'bcu/feature12', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.354288-07'::cd_timestamp, 'BigCo User', NULL),
    ('aa3917ef-71b7-4264-8525-16bc46e367dc'::uuid, 'bcu/feature13', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355038-07'::cd_timestamp, 'BigCo User', NULL),
    ('6edcad49-2a53-4db3-9761-fe73f1b72130'::uuid, 'bcu/feature13', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.355798-07'::cd_timestamp, 'BigCo User', NULL),
    ('3a389fc9-fb03-4d86-8323-c0e4cf3b1537'::uuid, 'bcu/feature14', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.361497-07'::cd_timestamp, 'BigCo User', NULL),
    ('296de263-6773-43ba-a002-bdb8def7db4e'::uuid, 'bcu/feature15', 'legacy', 'open'::cd_patchset_status, '2014-08-26 19:07:22.362672-07'::cd_timestamp, 'BigCo User', NULL),
    ('72f13aef-4469-47dc-83d0-4bd09a98a554'::uuid, 'bcu/feature16', 'v1', 'open'::cd_patchset_status, '2014-08-26 19:07:22.363512-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  -- Now, the tests!
  ------------------------------------------------------------------------

  -- Filtering by pipeline and / or state

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, NULL),
   'first_page_no_filters',
   'When called with no filtering parameters, returns the first 10 changes, regardless of status');

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'v1', NULL, NULL, NULL, NULL),
   'all_v1_changes',
   'When filtering on the "v1" pipeline, shows only "v1" changes'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'legacy', NULL, NULL, NULL, NULL),
   'all_legacy_changes',
   'When filtering on the "legacy" pipeline, shows only "legacy" changes'
  );

  RETURN QUERY SELECT is_empty(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'legacy', 'merged', NULL, NULL, NULL),
   'When filtering for "withdrawn legacy" changes, returns no rows'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'master', 'merged', NULL, NULL, NULL),
   'merged_master_changes',
   'When filtering for "merged master" changes, shows only those changes'
  );

  -- Sorting
  ------------------------------------------------------------------------

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, NULL),
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, FALSE, NULL),
   'A chronological sort is the default'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, TRUE, NULL),
   'first_page_reverse_chronological',
   'A reverse-chronological sort can be requested'
  );

  -- Pagination
  ------------------------------------------------------------------------

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, NULL),
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, 10),
   'The default page size is 10 (if you do not specify a page size, this is what you get)'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, 5),
   'first_page_of_five',
   'You can request less than the default page size (here, 5)'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, 12),
   'first_page_of_twelve',
   'You can request more than the default page size (here, 12)'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, 1000),
   'everything',
   'Requesting a page size larger than all the data will just get you all the data'
  );

  RETURN QUERY SELECT is_empty(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, 0),
   'You can even request a page size of 0, though I am not sure why you would want to'
  );

  -- Pages beyond the first
  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, '453c64fc-6939-4a59-862e-a31e4178a3c6', NULL, NULL),
   'second_page_no_filters',
   'Passing a change ID will retrieve the page following'
  );

  RETURN QUERY SELECT is_empty(
    format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, '72f13aef-4469-47dc-83d0-4bd09a98a554', NULL, NULL),
    'Retrieving a page after the last visible change returns nothing'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'legacy', 'open', NULL, NULL, 2),
   'open_legacy_chronological_page_1_size_2',
   'Ensure pagination plays well with other filtering parameters: 2 open changes for "legacy" with a page size of 2'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'legacy', 'open', '3a389fc9-fb03-4d86-8323-c0e4cf3b1537', NULL, 2),
   'open_legacy_chronological_page_2_size_2',
   'Ensure pagination plays well with other filtering parameters: 1 open change for "legacy" on page 2 with a size of 2 (i.e., show the 3rd one)'
  );

  -- Pathological Case That We'll Probably Never See
  ------------------------------------------------------------------------

  PREPARE pathological_date_results_first_page AS
  VALUES
    ('396bd34b-c42a-465e-85bd-a2c49b9908a6'::uuid, 'bcu/feature1', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.33469-07'::cd_timestamp, 'BigCo User', NULL),
    ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654'::uuid, 'bcu/feature3', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL)
  ;
  PREPARE pathological_date_results_second_page AS
  VALUES
    ('cb761c42-35e2-4e58-936e-27b381c01aab'::uuid, 'bcu/feature2', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('d83f9edd-66b1-44ac-b617-ffefa07660af'::uuid, 'bcu/feature4', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.34324-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'master', 'open', NULL, NULL, 2),
   'pathological_date_results_first_page',
   'Ensure that pagination works properly when the "last change" falls in a block of simultaneous changes; this is page 1'
  );

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'master', 'open', '7e1d8f8c-920f-4f6c-a87c-4ee3fea87654', NULL, 2),
   'pathological_date_results_second_page',
   'Ensure that pagination works properly when the "last change" falls in a block of simultaneous changes; this is page 2'
  );

  -- Now to ensure this works in reverse-chronological order, too!
  PREPARE pathological_date_results_second_page_reverse AS
  VALUES
    ('7e1d8f8c-920f-4f6c-a87c-4ee3fea87654'::uuid, 'bcu/feature3', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'BigCo User', NULL),
    ('396bd34b-c42a-465e-85bd-a2c49b9908a6'::uuid, 'bcu/feature1', 'master', 'open'::cd_patchset_status, '2014-08-26 19:07:22.33469-07'::cd_timestamp, 'BigCo User', NULL)
  ;

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'master', 'open', 'cb761c42-35e2-4e58-936e-27b381c01aab', TRUE, 2),
   'pathological_date_results_second_page_reverse',
   'Ensure that pagination works properly when the "last change" falls in a block of simultaneous changes; this is the second page, going in reverse'
  );

  -- Error Cases
  ------------------------------------------------------------------------

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, NULL, NULL, -10),
   'CD009',
   'Pagination limit cannot be negative',
   'A limit cannot be negative, just like the exception says'
  );

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee', NULL, NULL),
   'CD010',
   'Invalid change ID given for paging',
   'Specifying a non-existent change ID for paging is a no-no'
  );

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee', NULL, NULL),
   'CD010',
   'Invalid change ID given for paging',
   'Specifying a non-existent change ID for paging is a no-no'
  );

  -- Show that an existing UUID needs to be from the proper project

  RETURN NEXT set_has(
    'SELECT c.id FROM changes AS c
                 JOIN pipelines AS pipe ON c.pipeline_id = pipe.id
                 JOIN projects AS p ON pipe.project_id = p.id
                 WHERE p.name = ''honeybadger''',
    'VALUES (''6cd05c0d-dcbf-45aa-b22f-750ae1f971b7''::uuid)',
    'Assert that a particular UUID exists on a different project...');

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, NULL, '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', NULL, NULL),
   'CD010',
   'Invalid change ID given for paging',
   'Specifying an existing change ID that does not belong to this project is a no-no'
  );

  -- If you filter by pipeline, the given change ID better be associated with it!

  RETURN NEXT set_has(
    'SELECT c.id FROM changes AS c
                 JOIN pipelines AS pipe ON c.pipeline_id = pipe.id
                 JOIN projects AS p ON pipe.project_id = p.id
                 WHERE p.name = ''skunkworks''
                 AND pipe.name = ''legacy''',
    'VALUES (''6edcad49-2a53-4db3-9761-fe73f1b72130''::uuid)',
    'Assert that a particular UUID exists on the same project, but the "legacy" pipeline...');

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'master', NULL, '6edcad49-2a53-4db3-9761-fe73f1b72130', NULL, NULL),
   'CD010',
   'Invalid change ID given for paging',
   'Specifying an existing change ID that does not belong to this project is a no-no'
  );

  -- Non-existent pipeline

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', 'no_pipeline_is_named_this', NULL, NULL, NULL, NULL),
   'CD007',
   'Pipeline not found',
   'Filtering on a non-existent pipeline is an error'
  );

  -- Filtering by invalid change status

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, 'superseded', NULL, NULL, NULL),
   'CD011',
   'Invalid change state given',
   '"superseded" is not a valid state for a change'
  );

  RETURN QUERY SELECT throws_ok(
   format('EXECUTE get_skunkworks_changes(%L, %L, %L, %L, %L)', NULL, 'accepted', NULL, NULL, NULL),
   'CD011',
   'Invalid change state given',
   '"accepted" is not a valid state for a change'
  );

END;
$$;
