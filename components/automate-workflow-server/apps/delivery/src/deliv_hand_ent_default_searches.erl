-module(deliv_hand_ent_default_searches).
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
        {deliv_authz, [forbidden/2]}]).


init(_Transport, Req, State) ->
    {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

to_json(Req, State) ->
    {EntName, Req1} = cowboy_req:binding(ent_name, Req),
    {Method, Req2} = cowboy_req:method(Req1),
    EnterpriseTuple = deliv_enterprise:fetch(EntName),
    case {Method, EnterpriseTuple} of
      {<<"GET">>, {ok, Enterprise}} ->
            DefaultSearch = deliv_enterprise_default_search:fetch(deliv_enterprise:getval(id, Enterprise)),
            Body = { [ {<<"search">>, Val} || #deliv_enterprise_default_search{search = Val} <- DefaultSearch ] },
            deliv_web_utils:content(Body, Req2, State);
      {<<"PUT">>, {ok, Enterprise}} ->
            {EjsonOrErrorTuple, Req3} =deliv_web_utils:parse_json_req(Req2,
                                             default_search_jesse_spec()),
            update_search(EjsonOrErrorTuple, Req3, Enterprise, State);
      {_, {error, Reason}} ->
            chef_log:failed_call(?MODULE, to_json, [EntName], Reason),
            deliv_web_utils:error_response(404, ent_not_found, Req2, State)
    end.

default_search_jesse_spec() ->
    chef_json:rigid_object_spec([{<<"search">>, <<"string">>}]).

update_search({error, Reason}, Req, _Enterprise, State) ->
    chef_log:failed_call(?MODULE, update_search, [], Reason),
    deliv_web_utils:error_response(409, must_include_search, Req, State);
update_search(Ejson, Req, Enterprise, State) ->
    Search = ej:get([<<"search">>], Ejson),
    EntId = deliv_enterprise:getval(id, Enterprise),
    upsert_or_delete(Search, EntId, Req, State).

upsert_or_delete(<<"">>, EntId, Req, State) ->
    deliv_enterprise_default_search:delete(EntId),
    {true, Req, State};
upsert_or_delete(Search, EntId, Req, State) ->
    deliv_enterprise_default_search:upsert(EntId, Search),
    {true, Req, State}.

