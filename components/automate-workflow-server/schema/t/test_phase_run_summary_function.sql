CREATE OR REPLACE FUNCTION test_phase_run_summary_function()
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
  stage_runs_id_last BIGINT = 4740;
  phase_runs_id_last BIGINT = 18010;
BEGIN

  PREPARE multiple_patchsets AS
  VALUES
    (4760::bigint /* stage_runs_last_id + 20 */,'verify','finished',18070::bigint /* phase_runs_id_last + 60 */,'unit','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),
    (4760::bigint /* stage_runs_last_id + 20 */,'verify','finished',18068::bigint /* phase_runs_id_last + 58 */,'lint','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)'),
    (4760::bigint /* stage_runs_last_id + 20 */,'verify','finished',18069::bigint /* phase_runs_id_last + 59 */,'syntax','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'honeybadger (BEHOLD, A DESC)');

  PREPARE all_the_things AS
  VALUES
    (4752::bigint /* stage_runs_last_id + 12 */,'verify','finished',18046::bigint /* phase_runs_id_last + 36 */,'unit','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
    (4752::bigint /* stage_runs_last_id + 12 */,'verify','finished',18044::bigint /* phase_runs_id_last + 34 */,'lint','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
    (4752::bigint /* stage_runs_last_id + 12 */,'verify','finished',18045::bigint /* phase_runs_id_last + 35 */,'syntax','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),

    (4761::bigint /* stage_runs_last_id + 21 */,'build','finished',18071::bigint /* phase_runs_id_last + 61 */,'unit','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
    (4761::bigint /* stage_runs_last_id + 21 */,'build','finished',18072::bigint /* phase_runs_id_last + 62 */,'lint','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
    (4761::bigint /* stage_runs_last_id + 21 */,'build','finished',18073::bigint /* phase_runs_id_last + 63 */,'syntax','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
    (4761::bigint /* stage_runs_last_id + 21 */,'build','finished',18074::bigint /* phase_runs_id_last + 64 */,'quality','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
    (4761::bigint /* stage_runs_last_id + 21 */,'build','finished',18075::bigint /* phase_runs_id_last + 65 */,'security','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)'),
    (4761::bigint /* stage_runs_last_id + 21 */,'build','finished',18076::bigint /* phase_runs_id_last + 66 */,'publish','finished', 'BEHOLD, A QUERY', 'BEHOLD, A DESC', 'skunkworks (BEHOLD, A DESC)')
  ;

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phase_run_summary(%L)', '9e37222a-5159-4754-907d-ead631b9cda2'),
    'all_the_things',
    'A phase summary for a change with multiple patchsets and multiple stage runs returns the phase runs from the merged patchset and subsequent stages');

  RETURN QUERY SELECT results_eq(
    format('SELECT * FROM phase_run_summary(%L)', '6cd05c0d-dcbf-45aa-b22f-750ae1f971b7'),
    'multiple_patchsets',
    'A phase summary for a change with multiple patchsets (but which is still open) returns the phase runs for the most recent stage');

END;
$$;
