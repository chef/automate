CREATE OR REPLACE FUNCTION test_get_patchsets_for_change_function()
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
  VALUES (1825::bigint /* patchset_id_last + 12 */, '9e37222a-5159-4754-907d-ead631b9cda2'::uuid, 2::smallint, '2014-08-26 19:07:22.352647-07'::cd_timestamp, 'deadbeef111', 55::bigint, NULL, FALSE, 'merged'::cd_patchset_status);

  PREPARE open_change AS
  VALUES
  (1833::bigint /* patchset_id_last + 20 */, '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7'::uuid, 2::smallint, '2014-08-27 18:29:32.02564-07'::cd_timestamp, 'beefbeef11', 55::bigint, NULL, FALSE, 'open'::cd_patchset_status),
  (1832::bigint /* patchset_id_last + 19 */, '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7'::uuid, 1::smallint, '2014-08-27 18:27:32.02564-07'::cd_timestamp, 'beefbeef1', 55::bigint, NULL, FALSE, 'superseded'::cd_patchset_status);

  PREPARE withdrawn_change AS
  VALUES
  (1820::bigint /* patchset_id_last + 7 */, '1ebc2534-4a43-4efc-95d0-f797842c6a6e'::uuid, 3::smallint, '2014-08-26 19:07:22.348075-07'::cd_timestamp, 'deadbeef52', 55::bigint, NULL, FALSE, 'withdrawn'::cd_patchset_status);

  PREPARE no_change AS
  SELECT * FROM patchsets WHERE FALSE;

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM get_patchsets_for_change(%L)', '9e37222a-5159-4754-907d-ead631b9cda2'),
    'merged_change',
    'Returns only the final patchset for a merged change');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM get_patchsets_for_change(%L)', '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7'),
    'open_change',
    'Returns multiple patchsets for an open change, in reverse sequence-number order');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM get_patchsets_for_change(%L)', 'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee'),
    'no_change',
    'Returns nothing for a non-existent change');

END;
$$;
