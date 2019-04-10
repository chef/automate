%% Chef API Result records
-record(search, {
        total,
        start,
        rows
        }).

-record(push_job_status, {
        id,
        command,
        run_timeout,
        status,
        created_at,
        updated_at,
        total_nodes,
        nodes
        }).

-record(job_event, {source,
                    seq,
                    time,
                    type,
                    msg}).

-record(deliverance_stage_queue, {queues=orddict:new(),
                      running=orddict:new()}).

-record(state, {
          phase_pid,
          node_name,
          node_search_query,
          command = <<"deliverance">>,
          quorum = 1,
          status,
          finished = false,
          consecutive_status_errors = 0, %% Related to rest api
          times_started = 0
         }).

%%-record(user, {id,
%%               userid,
%%               password,
%%               created_on}).

%%-record(project, {id,
%%                  name,
%%                  project_type,
%%                  unit_slave=[],
%%                  lint_slave=[],
%%                  syntax_slave=[],
%%                  security_slave=[],
%%                  quality_slave=[],
%%                  smoke_slave=[],
%%                  functional_slave=[],
%%                  provision_slave=[],
%%                  publish_slave=[],
%%                  deploy_slave=[],
%%                  deploy_pattern=[],
%%                  assets=[],
%%                  created_on}).

%%-record(pipeline, {id,
%%                   project_id,
%%                   name,
%%                   production=true,
%%                   created_on}).

%%-record(change, {id, data}).

%%-define(EMPTY_HASH, <<"{}">>).
%%-define(EMPTY_HASH_LIST, <<"[{}]">>).
%%-define(OK_RESPONSE, <<"{\"result\": \"ok\"}">>).
