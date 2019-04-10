-module(deliv_hand_change_description).
-behaviour(deliv_rest).

-include("deliv_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

-mixin([{deliv_token, [is_authorized/2]},
        {deliv_authz, [{forbidden_for_change_action/2, forbidden}]}]).

init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req, State) ->
    {ChangeId, Req1} = cowboy_req:binding(change_id, Req),

    case deliv_change:fetch_by_id(ChangeId) of
        {ok, Change} ->
            {Method, Req2} = cowboy_req:method(Req1),

            case Method of
                <<"GET">> ->
                    handle_get(Change, Req2, State);
                <<"PUT">> ->
                    handle_json_input(
                      deliv_web_utils:parse_json_req(Req2, update_change_jesse_spec()),
                      Change, State)
            end;
        {error, not_found} ->
            deliv_web_utils:error_response(404, not_found, <<>>, Req1, State);
        {error, Reason} ->
            chef_log:log(error, "Could not retrieve change ~s: ~p", [ChangeId, Reason]),
            deliv_web_utils:error_response(500, system_error, <<>>, Req1, State)
    end.

handle_get(Change, Req, State) ->
    Title = deliv_change:getval(title, Change),
    Description = deliv_change:getval(description, Change),

    Body = {[
             {<<"title">>, Title},
             {<<"description">>, Description}
            ]},

    deliv_web_utils:content(Body, Req, State).

handle_json_input({{error, _Why}, Req}, _Change, State) ->
    deliv_web_utils:error_response(400, bad_request, Req, State);
handle_json_input({Json, Req}, Change, State) ->
    NewTitle = ej:get([<<"title">>], Json),
    NewDescription = ej:get([<<"description">>], Json),
    NewChange = deliv_change:setvals([{title, NewTitle},
                                      {description, NewDescription}], Change),
    case deliv_change:update(NewChange) of
        {ok, _NewChange} ->
            {true, Req, State};
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_server_error, Req, State)
    end.

update_change_jesse_spec() ->
    chef_json:rigid_object_spec([{<<"title">>, <<"string">>},
                                  {<<"description">>, <<"string">>}]).
