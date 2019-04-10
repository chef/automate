-module(deliv_hand_org_reviews).
-behaviour(deliv_rest).

-include_lib("mixer/include/mixer.hrl").

-export([
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         init/3,
         rest_init/2,
         to_json/2
        ]).

-include("deliv_types.hrl").

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
    {deliv_web_utils:content_type_json_map(from_json), Req, State}.

to_json(Req, #handler{ent_name = EntName} = State) ->
    %% If we get this far, we know we've got an org name, and that
    %% it's in this enterprise (otherwise the authz/n checks would
    %% have failed miserably).
    {OrgName, Req} = cowboy_req:binding(org_name, Req),

    case deliv_organization:get_changes_for_review(EntName, OrgName) of
        {error, _Why} ->
            deliv_web_utils:error_response(500, internal_error, Req, State);
        {ok, Proplists} ->
            %% Based on the database query, the proplist already has
            %% all the appropriate key names, so all we need to do is
            %% wrap each one in a tuple to make it legit ejson

            %% NOTE: I would normally use a list comprehension for
            %% this, but apparently Dialyzer is not happy when the
            %% lists could be empty (which is the case here); see
            %% http://erlang.org/pipermail/erlang-questions/2011-March/056812.html
            Ejson = lists:map(fun to_json/1, Proplists),

            %% We're not currently doing any HAL for this endpoint, so
            %% we can just return this list of ejson objects
            deliv_web_utils:content(Ejson, Req, State)
    end.

%% @doc Convert a Sqerl result set into an Ejson object, but ensuring
%% that all our pseudo-`datetime` results are converted to a timestamp
%% string.
%%
%% Note that this just works off the keys right now, and not the
%% format of the value in the key-value pair. We could potentially
%% switch over to that to make this even more general.
to_json(TupleList) ->
    Fun = fun({K, V}) when K =:= <<"created_at">>;
                           K =:= <<"updated_at">> ->
                  {K, chef_utils:format_timestamp(V)};
             (Tuple) ->
                  Tuple
          end,
    {lists:map(Fun, TupleList)}.
