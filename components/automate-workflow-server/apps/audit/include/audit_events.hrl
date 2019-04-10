-record(audit_stage_event, {
          action :: started | finished,
          create_time :: calendar:datetime(),
          status :: binary(),
          change_id :: binary(),
          change_title :: binary(),
          ent :: binary(),
          org :: binary(),
          pipe :: binary(),
          proj :: binary(),
          stage_name :: atom(),
          submitted_at :: calendar:datetime(),
          submitted_by :: binary(),
          approved_by :: binary() | null,
          delivered_by :: binary() | null
         }).
