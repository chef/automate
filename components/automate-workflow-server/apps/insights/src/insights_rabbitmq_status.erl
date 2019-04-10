-module(insights_rabbitmq_status).

-include_lib("delivery/include/deliv_types.hrl").

-export([
         ping/0
        ]).

-spec ping() -> #status_metadata{}.
ping() ->
    {Status, Attributes} = rabbitmq_health(),
    #status_metadata{service = <<"rabbitmq">>, status = Status, additional_attributes = Attributes}.

-spec rabbitmq_health() -> {pong | pang | not_running, [{binary(), binary()}]}.
rabbitmq_health() ->
    case {insights_enabled(), rabbitmq_management_enabled()} of
        {true, true} ->
            CheckResults = run_rabbitmq_health_checks(),
            Status = determine_status(CheckResults),
            Attributes = [CheckAttrs || {_CheckStatus, CheckAttrs} <- CheckResults],
            {Status, lists:flatten(Attributes)};
        {true, false} ->
            {not_running, [{<<"rabbitmq_management">>, {[{<<"status">>, <<"not_running">>},
                                                         {<<"description">>, <<"rabbitmq_management plugin is disabled">>}]}}]};
        {_, _} ->
            {not_running, [{<<"rabbitmq">>, {[{<<"status">>, <<"not_running">>},
                                                         {<<"description">>, <<"RabbitMQ is disabled">>}]}}]}
    end.

%% Here's we'll iterate over our desired rabbitmq_management health checks. Any
%% requests paths that contain the vhost should be URI encoded because the vhost
%% probably contains a leading slash in the resource name, eg: "/insights".
%% Ibrowse has a bad time parsing those extra slashes in resource names.
-spec run_rabbitmq_health_checks() -> [{pong | pang, [{binary(), binary()}]}].
run_rabbitmq_health_checks() ->
    [rabbitmq_check(Name, Path) || {Name, Path} <- [{"vhost_aliveness", "/api/aliveness-test/" ++ http_uri:encode(chef_utils:to_str(rabbitmq_vhost()))},
                                                    {"node_health", "/api/healthchecks/node"}
                                                   ]].

-spec rabbitmq_check(string() | binary(), string()) -> {pong | pang, [{binary(), binary()}]}.
rabbitmq_check(CheckName, Path) when is_list(CheckName) ->
    rabbitmq_check(chef_utils:to_bin(CheckName), Path);
rabbitmq_check(CheckName, Path) ->
    URL = rabbitmq_management_root_url() ++ Path,
    Username = chef_utils:to_str(rabbitmq_management_user()),
    Password = chef_utils:to_str(rabbitmq_management_password()),
    Options = [{basic_auth, {Username, Password}},
               {ssl_options, [{verify, verify_none}]}],
    case deliv_http:req(get, URL, <<>>, [], Options) of
        {ok, ResCode, _ResHeaders, _ResBody} when ResCode >= 200, ResCode < 300 ->
            {pong, [{CheckName, {[{<<"status">>, <<"pong">>}]}}]};
        {ok, ResCode, _ResHeaders, ResBody} ->
            chef_log:error("Failed ~s check at ~s with ~s, ~s",
                           [CheckName, URL, ResCode, ResBody]),
            {pang, [{CheckName, {[{<<"status">>, <<"fail">>},
                                  {<<"description">>, ResBody}]}}]};
        {error, _Why} ->
            chef_log:error("Failed ~s check at ~s: unreachable",
                           [CheckName, URL]),
            {pang, [{CheckName, {[{<<"status">>, <<"fail">>},
                                  {<<"description">>, <<"The host was unreachable">>}]}}]}
    end.

-spec determine_status([{pong | pang, [{binary(), binary()}]}]) -> pong | pang.
determine_status(CheckResults) ->
    Statuses = [CheckStatus || {CheckStatus, _CheckAttrs} <- CheckResults],
    case lists:all(fun(Status) -> Status == pong end, Statuses) of
        true ->
            pong;
        false ->
            fail
    end.

-spec rabbitmq_management_root_url() -> string().
rabbitmq_management_root_url() ->
    Host = envy:get(insights, rabbitmq_host, undef, string),
    Schema = case envy:get(insights, rabbitmq_ssl, false, boolean) of
                 true -> "https";
                 false -> "http"
             end,
    Port = chef_utils:to_str(envy:get(insights, rabbitmq_management_port, undef, integer)),
    lists:append([Schema, "://", Host, ":", Port]).

-spec rabbitmq_management_user() -> binary().
rabbitmq_management_user() ->
    envy:get(insights, rabbitmq_management_user, undef, binary).

-spec rabbitmq_management_password() -> binary().
rabbitmq_management_password() ->
    envy:get(insights, rabbitmq_management_password, undef, binary).

-spec rabbitmq_vhost() -> binary().
rabbitmq_vhost() ->
    envy:get(insights, rabbitmq_vhost, undef, binary).

-spec rabbitmq_management_enabled() -> true | false.
rabbitmq_management_enabled() ->
    envy:get(insights, rabbitmq_management_enabled, false, atom).

-spec insights_enabled() -> true | false.
insights_enabled() ->
    envy:get(insights, enabled, false, atom).
