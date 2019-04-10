CREATE OR REPLACE FUNCTION test_changes_in_verify_function()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN

  PREPARE get_changes_in_verify AS
    SELECT project, feature_branch, change_id, created_at, last_code_activity_at, status, finished
    FROM changes_in_verify($1, $2)
    ORDER BY project, pipeline, feature_branch;

  PREPARE big_co_engineering AS
  VALUES
    ('honeybadger', 'bcu/monkeys', '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7'::uuid, '2014-08-27 18:27:32.02564-07'::cd_timestamp, '2014-08-27 18:29:32.02564-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature13', '6edcad49-2a53-4db3-9761-fe73f1b72130'::uuid, '2014-08-26 19:07:22.355798-07'::cd_timestamp, '2014-08-26 19:07:22.355798-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature14', '3a389fc9-fb03-4d86-8323-c0e4cf3b1537'::uuid, '2014-08-26 19:07:22.361497-07'::cd_timestamp, '2014-08-26 19:07:22.361497-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature15', '296de263-6773-43ba-a002-bdb8def7db4e'::uuid, '2014-08-26 19:07:22.362672-07'::cd_timestamp, '2014-08-26 19:07:22.362672-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature1', '396bd34b-c42a-465e-85bd-a2c49b9908a6'::uuid, '2014-08-26 19:07:22.33469-07'::cd_timestamp, '2014-08-26 19:07:22.33469-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature10', '7e1d1db0-e313-4107-a07c-8f62eae5d23f'::uuid, '2014-08-26 19:07:22.351063-07'::cd_timestamp, '2014-08-26 19:07:22.351063-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature12', '453c64fc-6939-4a59-862e-a31e4178a3c6'::uuid, '2014-08-26 19:07:22.354288-07'::cd_timestamp, '2014-08-26 19:07:22.354288-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature13', 'aa3917ef-71b7-4264-8525-16bc46e367dc'::uuid, '2014-08-26 19:07:22.355038-07'::cd_timestamp, '2014-08-26 19:07:22.355038-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature2', 'cb761c42-35e2-4e58-936e-27b381c01aab'::uuid, '2014-08-26 19:07:22.339788-07'::cd_timestamp, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature3', '7e1d8f8c-920f-4f6c-a87c-4ee3fea87654'::uuid, '2014-08-26 19:07:22.339788-07'::cd_timestamp, '2014-08-26 19:07:22.339788-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature4', 'd83f9edd-66b1-44ac-b617-ffefa07660af'::uuid, '2014-08-26 19:07:22.34324-07'::cd_timestamp, '2014-08-26 19:07:22.34324-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature8', '674802e5-4354-4306-865d-9b7e6c6de2b9'::uuid, '2014-08-26 19:07:22.34932-07'::cd_timestamp, '2014-08-26 19:07:22.34932-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature9', 'f211b17b-2ad7-49e6-a899-e59ca3d4d1e3'::uuid, '2014-08-26 19:07:22.350268-07'::cd_timestamp, '2014-08-26 19:07:22.350268-07'::cd_timestamp, 'finished', true),
    ('skunkworks', 'bcu/feature16', '72f13aef-4469-47dc-83d0-4bd09a98a554'::uuid, '2014-08-26 19:07:22.363512-07'::cd_timestamp, '2014-08-26 19:07:22.363512-07'::cd_timestamp, 'finished', true)
  ;

  RETURN QUERY SELECT results_eq(
   format('EXECUTE get_changes_in_verify(%L, %L)', 'BigCo', 'BigCo Engineering'),
   'big_co_engineering',
   'Returns the changes for BigCo Engineering');

END;
$$;
