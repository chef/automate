CREATE OR REPLACE FUNCTION test_get_latest_patchset_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
DECLARE

  -- So, these mirror the data in the delivery_test_data file, but
  -- aren't formally used in this function... I can't seem to get
  -- plpgsql variables to be used in prepared statements. :(
  --
  -- Consider them reference. I've tried making the places where
  -- they're used below as obvious and easy-to-work-with as possible,
  -- given this constraint.
  patchset_id_last BIGINT := 1813;
  bigco_user_id BIGINT = u('BigCo', 'BigCo User');  -- 55
BEGIN

  PREPARE merged_change AS
  -- Yes, all these typecasts are required :(
  VALUES(1825::bigint /* patchset_id_last + 12 */,'9e37222a-5159-4754-907d-ead631b9cda2'::uuid, 2::smallint, '2014-08-26 19:07:22.352647-07'::cd_timestamp, 'deadbeef111', 55::bigint, NULL, FALSE, 'merged'::cd_patchset_status);

  PREPARE open_change AS
  VALUES(1817::bigint /* patchset_id_last + 4 */, 'd83f9edd-66b1-44ac-b617-ffefa07660af'::uuid, 1::smallint, '2014-08-26 19:07:22.34324-07'::cd_timestamp, 'deadbeef4', 55::bigint, NULL, FALSE, 'open'::cd_patchset_status);

  PREPARE withdrawn_change AS
  VALUES (1820::bigint /* patchset_id_last +7 */, '1ebc2534-4a43-4efc-95d0-f797842c6a6e'::uuid, 3::smallint, '2014-08-26 19:07:22.348075-07'::cd_timestamp, 'deadbeef52', 55::bigint, NULL, FALSE, 'withdrawn'::cd_patchset_status);

  PREPARE no_change AS
  SELECT * FROM patchsets WHERE FALSE;

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM get_latest_patchset(%L)', '9e37222a-5159-4754-907d-ead631b9cda2'),
    'merged_change',
    'Returns the latest patchset for a merged change');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM get_latest_patchset(%L)', 'd83f9edd-66b1-44ac-b617-ffefa07660af'),
    'open_change',
    'Returns the latest patchset for an open change');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM get_latest_patchset(%L)', 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'),
    'no_change',
    'Returns nothing for a non-existent change');

END;
$$;
