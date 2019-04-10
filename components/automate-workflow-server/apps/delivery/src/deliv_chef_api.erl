-module(deliv_chef_api).

-include("deliverance_types.hrl").
-include("deliv_types.hrl").

-export([
         create_environment/1,
         get_client/1,
         pub_key_for_client/1,
         push_job_start/3,
         push_job_status/1,
         push_node_states/0,
         search/3
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%% TODO: management of Chef policy for environments and cookbook
%% versions needs to move here.

-define(RUN_TIMEOUT, delivery_app:get_env(push_jobs_run_timeout)).
-define(APP_ENV_CONFIG_KEY, chef_req_config).

%% @private
%% @doc Returns the chef_req configuration record
%% We cache it in the application's env config
%% Returns a #chef_req_config{} record
-spec get_config() -> tuple().
get_config() ->
    case delivery_app:get_env(?APP_ENV_CONFIG_KEY, undefined) of
        undefined ->
            %% TODO: PORT: Change this to read config directly from delivery attrs
            %% in sys.config. Instead of form a separate file. Eventually we probably
            %% want to read these from the db since we will need to support multiple
            %% chef servers.
            ConfigFile = delivery_app:get_env(deliv_chef_config),
            Config = chef_req:load_config(ConfigFile),
            application:set_env(delivery, ?APP_ENV_CONFIG_KEY, Config),
            Config;
        Config ->
            Config
    end.

%% @doc Return the public key for the client identified by `Name'.
-spec pub_key_for_client(str_or_binary()) -> {ok, binary()} | {error, _}.
pub_key_for_client(Name) ->
    case get_client(Name) of
        {ok, ClientJson} ->
            case ej:get([<<"public_key">>], ClientJson) of
                Key when is_binary(Key) ->
                    {ok, Key};
                undefined ->
                    {error, {missing_public_key, ClientJson}}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Fetch data for a client (node identity) in the Chef Server.
-spec get_client(str_or_binary()) -> {ok, json()} | {error, _}.
get_client(Name) ->
    Conf = get_config(),
    Path = "/clients/" ++ chef_utils:to_str(Name),
    case chef_req:request(get, Path, Conf) of
        {ok, "200", _Headers, Json} ->
            Client = chef_json:decode(Json),
            chef_log:debug("chef_api:get_client 200: ~p", [Client]),
            {ok, Client};
        Other ->
            chef_log:error("chef_api:get_client got bad response: ~p~n",
                            [Other]),
            {error, Other}
    end.

%% @doc Start a Chef Push job. Run `Command' on `Nodes' with the
%% specified `Quorum'.
-spec push_job_start(binary(), [binary()], non_neg_integer()) ->
                            {ok, JobId :: binary()} |
                            {error, _}.
push_job_start(Command, Nodes, Quorum) when erlang:is_binary(Command),
                                            erlang:is_list(Nodes),
                                            erlang:is_integer(Quorum) ->
    Conf = get_config(),
    ReqBody = jiffy:encode({[{command, Command},
                             {run_timeout, ?RUN_TIMEOUT},
                             {nodes, Nodes},
                             {quorum, Quorum}]}),
    case chef_req:request(post, "/pushy/jobs", ReqBody, Conf) of
        {ok, "201", _Headers, Json} ->
            {[{<<"uri">>, Uri}]} = jiffy:decode(Json),
            {ok, erlang:binary_to_list(lists:last(binary:split(Uri, <<"/">>,
                                                        [global])))};
        Other ->
            chef_log:info("chef_api:do_push_job_start got bad response: ~p~n",
                       [Other]),
            {error, Other}
    end.

%% @doc Get the status of the Chef Push job specified by `JobId'.
-spec push_job_status(binary() | string()) -> {ok, #push_job_status{}} |
                                              {error, _}.
push_job_status(JobId) ->
    JobIdStr = chef_utils:to_str(JobId),
    Conf = get_config(),
    case chef_req:request(get, "/pushy/jobs/" ++ JobIdStr, Conf) of
        {ok, "200", _Headers, Json} ->
            chef_log:debug("chef_api:do_push_job_status 200"),
            {StatusPlist} = jiffy:decode(Json),
            {NodePlist} = proplists:get_value(<<"nodes">>, StatusPlist),
            {ok, #push_job_status{id=proplists:get_value(<<"id">>, StatusPlist),
                                  command=proplists:get_value(<<"command">>, StatusPlist),
                                  status=proplists:get_value(<<"status">>, StatusPlist),
                                  run_timeout=proplists:get_value(<<"run_timeout">>, StatusPlist),
                                  created_at=proplists:get_value(<<"created_at">>, StatusPlist),
                                  updated_at=proplists:get_value(<<"updated_at">>, StatusPlist),
                                  total_nodes=count_nodes(NodePlist),
                                  nodes=NodePlist}};
        Other ->
            chef_log:error("chef_api:do_push_job_status got bad response: ~p~n", [Other]),
            {error, Other}
    end.

%% @doc Execute a partial search against the Chef Server. I think
%% what's happening is that `Attrs' is JSON binary literal describing
%% the partial search items to extract. Here's an example call:
%% ```
%% {ok, SearchResult} = chef_api:search("node", NodeSearchQuery,
%%                                      <<"{\"name\":[\"name\"]}">>),
%% '''
-spec search(string(), string(), binary()) -> {ok, #search{}} |
                                              {error, _}.
search(Index, Query, Attrs) when erlang:is_list(Index),
                                 erlang:is_list(Query),
                                 erlang:is_binary(Attrs) ->
    Conf = get_config(),
    Path = "/search/" ++ Index ++ "?q=" ++ deliv_web_utils:encode_url(Query),
    case chef_req:request(post, Path, Attrs, Conf) of
        {ok, "200", _Headers, Json} ->
            chef_log:debug("chef_api:do_search 200"),
            {ResultPlist} = jiffy:decode(Json),
            {ok, #search{total=proplists:get_value(<<"total">>, ResultPlist),
                         start=proplists:get_value(<<"start">>, ResultPlist),
                         rows=proplists:get_value(<<"rows">>, ResultPlist)}};
        Other ->
            chef_log:error("chef_api:search got bad response: ~p~n", [Other]),
            {error, Other}
    end.

%% @doc Fetch node states for all nodes that Chef Push knows about.
-spec push_node_states() -> {ok, [{_, _}]} | {error, _}.
push_node_states() ->
    Conf = get_config(),
    case chef_req:request(get, "/pushy/node_states", Conf) of
        {ok, "200", _Headers, Json} ->
            chef_log:debug("chef_api:do_push_client_status 200"),
            StatePlist = jiffy:decode(Json),
            {ok, StatePlist};
        Other ->
            chef_log:error("chef_api:do_push_client_status got bad response: ~p~n", [Other]),
            {error, Other}
    end.

%% @doc Create an environment object on the Chef Server named `Name'.
-spec create_environment(string()) -> {ok, string()} | {error, _}.
create_environment(Name) ->
    Conf = get_config(),
    ReqBody = jiffy:encode({[{name, erlang:list_to_binary(Name)},
                             {json_class, <<"Chef::Environment">>},
                             {description, erlang:list_to_binary(Name ++ " created by Deliverance")},
                             {cookbook_versions, {[]}},
                             {chef_type, <<"environment">>}]}),
    case chef_req:request(post, "/environments", ReqBody, Conf) of
        {ok, "201", _Headers, Json} ->
            {[{<<"uri">>, Uri}]} = jiffy:decode(Json),
            {ok, erlang:binary_to_list(lists:last(binary:split(Uri, <<"/">>, [global])))};
        {ok, "409", _Headers, _Body} ->
            {ok, Name}
    end.

count_nodes(Nodes) ->
    lists:sum([ length(proplists:get_value(K, Nodes))
                || K <- proplists:get_keys(Nodes) ]).
