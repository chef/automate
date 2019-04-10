%% @doc Module for one-off github repository actions
-module(github_repo).

-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-export([
         add_builder_pub_key/1,
         add_user_pub_key/3,
         create_webhook/2,
         configure/2,
         webhook_callback_url/1
        ]).

-spec add_builder_pub_key(binary()) -> {ok, non_neg_integer()} | {error, term()}.
add_builder_pub_key(EntName) ->
    case deliv_intern_user:fetch(EntName, <<"builder">>) of
        {ok, User} ->
            Key = deliv_intern_user:getval(ssh_pub_key, User),
            add_user_pub_key(EntName, key_name(EntName), Key);
        {error, _} = Error ->
            Error
    end.

%% @private
key_name(EntName) ->
    chef_utils:to_bin(["Delivery Builder (", EntName, ")"]).

-spec configure({ok, d_project()} | {error, any()}, verify_ssl | no_verify_ssl) -> {ok, non_neg_integer()} | {error, term()}.
configure({ok, Project}, VerifySsl) ->
    create_webhook(deliv_project:to_coords(Project), VerifySsl);
configure({error, _Why} = Error, _) ->
    Error.

%% @doc Add a public key to the github user that generated the token for the specified
%% enterprise. Returns the Github UID for that key.
-spec add_user_pub_key(binary(), binary(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
add_user_pub_key(EntName, Title, Key) ->
    Body = {[{<<"title">>, Title}, {<<"key">>, Key}]},
    case deliv_github_client:user_req(EntName, post, ["/keys"], Body) of
        {ok, 201, _Headers, ResponseJson} ->
            {ok, ej:get({<<"id">>}, chef_json:decode(ResponseJson))};
        {ok, Status, Headers, ResponseJson} ->
            Reason = deliv_web_utils:status_to_atom(Status),
            chef_log:error("failed_call={~s:add_user_pub_key, ~p}; reason=~s; "
                            "http_response_headers=~p; http_response_body=~s",
                            [?MODULE, [EntName, Title, Key], Reason, Headers, ResponseJson]),
            {error, Reason};
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, add_user_to_pub_key, [EntName, Title, Key], Why),
            Err
    end.

%% @doc Create a project webhook
%% The verify_ssl and no_verify_ssl indicate whether we want SSL validation enabled for
%% the webhook. Returns the unique id for the webhook on github.
-spec create_webhook(#proj_coordinates{}, verify_ssl | no_verify_ssl) -> {ok, non_neg_integer()} | {error, term()}.
create_webhook(Coords, verify_ssl) ->
    do_create_webhook(Coords, webhook_request_body(Coords, <<"0">>));
create_webhook(Coords, no_verify_ssl) ->
    do_create_webhook(Coords, webhook_request_body(Coords, <<"1">>)).

webhook_callback_url(#proj_coordinates{ent_name = EntName,
                                       org_name = OrgName,
                                       proj_name = ProjName}) ->
    chef_utils:to_bin([deliv_web_utils:make_api_url_prefix(), EntName,
                        "/orgs/", OrgName, "/projects/", ProjName,
                        "/github-webhook"]).

%% @private
do_create_webhook(Coords, Body) ->
    case deliv_github_client:req(Coords, post, ["/hooks"], Body, []) of
        {ok, 201, _Headers, ResponseJson} ->
            {ok, ej:get({<<"id">>}, chef_json:decode(ResponseJson))};
        {ok, Status, Headers, ResponseJson} ->
            Reason = deliv_web_utils:status_to_atom(Status),
            chef_log:error("failed_call={~s:do_create_webhook, ~p}; "
                            "reason=~s; http_response_headers=~p; "
                            "http_response_body=~s",
                            [?MODULE, [Coords, Body], Reason, Headers, ResponseJson]),
            {error, Reason};
        {error, Why} = Err ->
            chef_log:failed_call(?MODULE, do_create_webhook, [Coords, Body], Why),
            Err
    end.

%% @private
webhook_request_body(Coords, SslEnabled) ->
    {[{<<"name">>, <<"web">>},
      {<<"config">>, {[{<<"url">>, webhook_callback_url(Coords)},
                       {<<"content_type">>, <<"json">>},
                       {<<"insecure_ssl">>, SslEnabled}
                      ]}},
      {<<"events">>, [<<"pull_request">>, <<"issue_comment">>]},
      {<<"active">>, true}
     ]}.


