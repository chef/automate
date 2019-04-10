CREATE OR REPLACE FUNCTION test_merge_change()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE get_merged_change AS
    SELECT id, merge_sha, latest_patchset, latest_patchset_status, approved_by
    FROM merge_change($1, $2, $3);

  PREPARE merged_change AS
    VALUES ('6cd05c0d-dcbf-45aa-b22f-750ae1f971b7'::uuid, 'deadbeef100', 2::smallint, 'merged'::cd_patchset_status, 'admin');

  RETURN QUERY SELECT results_eq(
    format('EXECUTE get_merged_change(%L, %L, %L)', '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', 'deadbeef100', 'admin'),
    'merged_change',
    'Marks the change as merged and records merge_sha.'
  );

  PREPARE merged_patchset AS
    VALUES (1833::bigint, 2::smallint, 'merged'::cd_patchset_status);

  PREPARE get_merged_patchset AS
    SELECT id, sequence_number, status
      FROM patchsets
     WHERE change_id = $1
       AND sequence_number = $2;

  RETURN QUERY SELECT results_eq(
    format('EXECUTE get_merged_patchset(%L, %L)', '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7', 2),
    'merged_patchset',
    'Marks the patchset as merged.'
  );

END;
$$;
