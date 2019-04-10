%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
-module(deliv_hand_changes).
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
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {deliv_web_utils:content_type_json_or_any_map(to_json), Req, State}.

content_types_accepted(Req, State) ->
    {deliv_web_utils:content_type_json_map(to_json), Req, State}.

to_json(Req0, #handler{ent_name = EntName, user_name = UserName}=State) ->
    {OrgName, Req0}  = cowboy_req:binding(org_name, Req0),
    {ProjName, Req0} = cowboy_req:binding(proj_name, Req0),

    case process_qs_vals(Req0) of
        {error, Error} ->
            chef_log:log(error, "Error in deliv_hand_changes query string: ~p", [Error]),
            %% TODO: format errors and let the user know what's up
            deliv_web_utils:error_response(400, bogus_qs_vals, <<>>, Req0, State);
        {Parameters, Req} ->
            case deliv_change_common:get_changes_as_ejson(EntName, OrgName, ProjName, UserName, Parameters) of
                {ok, Ejson} ->
                    deliv_web_utils:content(Ejson, Req, State);
                {error, Error} ->
                    chef_log:log(error, "Error is ~p", [Error]),
                    %% TODO: be more intelligent about what the error is
                    deliv_web_utils:error_response(500, system_error, <<>>, Req, State)
            end
    end.

%% Query String Parameter Processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_qs_vals(Req) ->
    {EntName, Req}  = cowboy_req:binding(ent_name, Req),
    {OrgName, Req}  = cowboy_req:binding(org_name, Req),
    {ProjName, Req} = cowboy_req:binding(proj_name, Req),
    case deliv_web_utils:process_qs_vals(
           Req,
           fun (QsKey, Val) ->
                   inner_qs_val(QsKey, Val, EntName, OrgName, ProjName)
           end,
           [{<<"pipeline">>, null},
            {<<"state">>,<<"all">>},
            {<<"id">>, null},
            {<<"sort">>, <<"new_to_old">>},
            {<<"limit">>, <<"10">>}]) of
        {ok, Req1, Wrapped} ->
            {[KeyValue || {_QsKey, {_,_} = KeyValue} <- Wrapped], Req1};
        {error, _} = Error ->
            Error
    end.


%% @doc Extract and process a query string parameter. Default values
%% are set here, and given values are validated and processed to
%% whatever datatype is appropriate.
%%
%% The Cowboy request that is returned is NOT guaranteed to be
%% identical to the one that was passed in, so don't count on that
%% being the case!
%%
%% Values are returned tagged with which parameter they represent.
-spec inner_qs_val(ParamName :: binary(), term(),  binary(), binary(), binary() ) ->
                          {ParamNameAtom :: atom(), ProcessedValue :: term() |
                                                                      {error, {Message :: atom(),
                                                                               Value :: term()}}}.
inner_qs_val(<<"limit">>, Limit, _, _, _) ->
    {limit, to_limit(Limit)};
inner_qs_val(<<"sort">>, Sort, _, _, _) ->
    {sort, to_sort(Sort)};
inner_qs_val(<<"id">>, Id, _, _, _) ->
    %% TODO: validate that ID is syntactically valid, and refers to an
    %% existing change, if it's not undefined
    {id, Id};
inner_qs_val(<<"state">>, State, _, _, _) ->
    {state, to_state(State)};
inner_qs_val(<<"pipeline">>, PipeName, EntName, OrgName, ProjName) ->
    case PipeName of
        null ->
            {pipeline, null};
        PipeName ->
            case deliv_pipeline:fetch(EntName, OrgName, ProjName, PipeName) of
                {error, not_found} =Error->
                    chef_log:log(info, "Error is ~p", [Error]),
                    {pipeline, {error, {unknown_pipeline, PipeName}}};
                {ok, Pipe} ->
                    chef_log:log(info, "Pipe is ~p", [Pipe]),
                    {pipeline, PipeName}
            end
    end.

to_limit(Bin) when erlang:is_binary(Bin) -> try erlang:binary_to_integer(Bin) of
                                                Int ->
                                                    to_limit(Int)
                                            catch error:badarg ->
                                                    {error, {invalid_limit, Bin}}
                                            end;
to_limit(Limit) when Limit >= 0   -> Limit;
to_limit(Other)                   -> {error, {invalid_limit, Other}}.

to_state(<<"all">>)       -> all;
to_state(<<"open">>)      -> open;
to_state(<<"merged">>)    -> merged;
to_state(Other)           -> {error, {unknown_state, Other}}.

to_sort(<<"new_to_old">>) -> new_to_old;
to_sort(<<"old_to_new">>) -> old_to_new;
to_sort(Other) ->            {error, {unknown_sort, Other}}.
