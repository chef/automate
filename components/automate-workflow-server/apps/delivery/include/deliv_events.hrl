-record(stage_event, {
          action     :: started | running | finished,
          create_time :: calendar:datetime(),
          stage_name :: deliv_stage_name(),
          status     :: binary(),
          scope      :: d_common_scope(),
          stage_run  :: d_stage_run(),
          change     :: d_change()
         }).

-record(phase_event, {
          phase_name      :: deliv_phase_name(),
          stage_run       :: d_stage_run(),
          phase_run       :: d_phase_run(),
          change          :: d_change(),
          patchset        :: d_patchset(),
          status          :: idle | running | failed | passed | skipped,
          status_reason   :: waiting_for_worker | no_workers | job_canceled,
          job_status      :: binary()
         }).
