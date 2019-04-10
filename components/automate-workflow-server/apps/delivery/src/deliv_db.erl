%% @doc This module is a thin wrapper around sqerl, mainly to add some logging
%% and some custom behaviors (such as a special return values for conflicts)
%% Object-specific funs should live in their object's module, not here.
-module(deliv_db).

-include("deliv_types.hrl").

%% The number of times to retry a fetch if we get a syntax error.
-define(MAX_RETRIES, 5).

-export([
         delete/3,
         delete2/2,
         delete_unique_by_scoping_params/3,
         fetch/2,
         fetch/3,
         fetch2/3,
         fetch_by_id/2,
         fetch_names/2,
         list_all/1,
         rename/3,
         update/1,
         update/2,
         insert/1,
         insert/2,
         insert/3,
         qfetch/3,
         delete/1,
         select/3,
         select/4,
         select/5,
         map_query/4,
         query_to_json/3
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%% TODO: metrics

%% @doc Helper function for doing sqerl_rec inserts
-spec insert(deliv_sqerl_record()) -> db_op_result().
insert(Item) ->
    handle_op_result(insert, sqerl_rec:insert(Item), Item).

%% @private
handle_op_result(Type, {error, {ErrorType, _} = Why}, Item) ->
    Level = case ErrorType of
                conflict -> debug;
                _ -> error
            end,
    chef_log:log(Level, "QUERY ~s FAILED on ~p: ~p", [Type, Item, Why]),
    {error, ErrorType};
handle_op_result(Type, {error, _} = Why, Item) ->
    chef_log:log(error, "QUERY ~s FAILED on ~p: ~p", [Type, Item, Why]),
    Why;
handle_op_result(Type, {ok, none}, Item) ->
    handle_op_result(Type, {ok, []}, Item);
handle_op_result(Type, {ok, N, Result}, Item) when is_integer(N) ->
    handle_op_result(Type, {ok, Result}, Item);
handle_op_result(Type, Result, Item) when element(1, Result) == ok;
                                          is_list(Result) ->
    chef_log:log(debug, "QUERY ~s OK: ~p", [Type, Item]),
    Result;
handle_op_result(Type, {_, _} = ErrorSpecial, Item) ->
    handle_op_result(Type, {error, ErrorSpecial}, Item).

%% @doc Delete a given list of deliv_sqerl_record
-spec delete([deliv_sqerl_record()] | deliv_sqerl_record()) -> [ok | {error, _Why}] | ok | {error, _Why}.
delete(Recs) when is_list(Recs) ->
    [delete2(Rec, id) || Rec <- Recs];
%% @doc Delete a given deliv_sqerl_record
delete(Rec) ->
    delete2(Rec, id).

%% @doc A few functions prefer to delete given a specific field,
%% this deletes a record by looking it up with the given field
-spec delete2(deliv_sqerl_record(), atom()) -> ok | {error, _Why}.
delete2(Rec, By) ->
    case sqerl_rec:delete(Rec, By) of
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, delete2, [Rec, By], Why),
            Error;
        {ok, Int}  when erlang:is_integer(Int) ->
            chef_log:log_item(info, Rec,
                               "delete successful (~p deleted)", [Int]),
            ok
    end.

%% Fetch result based on query
-spec qfetch(atom(), atom(), [any()]) -> db_op_result() | {error, _Why}.
qfetch(Module, Query, Vals) ->
    case with_retry(fun() -> sqerl_rec:qfetch(Module, Query, Vals) end) of
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, qfetch, [Module, Query, Vals], Why),
            Error;
        List when erlang:is_list(List) ->
            chef_log:debug("Successfully fetched ~p obj query ~p and vals ~p",
                            [Module, Query, Vals]),
            List
    end.

%% Fetch record by giving key and value
-spec fetch2(atom(), atom(), any()) -> {ok, [deliv_sqerl_record()]} |
                                       {error, _}.
fetch2(Module, By, Val) ->
    case with_retry(fun() -> sqerl_rec:fetch(Module, By, Val) end) of
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, fetch2, [Module, By, Val], Why),
            Error;
        List when erlang:is_list(List) ->
            chef_log:debug("Successfully fetched ~p obj with ~p = ~p",
                            [Module, By, Val]),
            {ok, List}
    end.

%% If we get a syntax error, that typically means that the app has
%% started before sqitch has fully deployed. Retry a few times to
%% see if it can catch up, otherwise bail
-type retriable_fun() :: fun(() -> {error, _Why} | [deliv_sqerl_record()]).

-spec with_retry(retriable_fun()) -> {error, _Why} | [deliv_sqerl_record()].
with_retry(Fun) ->
    with_retry(Fun, 0).

with_retry(Fun, Retry) when Retry < ?MAX_RETRIES ->
    case Fun() of
        {error, {syntax, SyntaxError}} ->
            NextRetry = Retry + 1,
            WaitTime = NextRetry * 10,
            chef_log:info("Syntax Error (~p) - retrying. Attempt ~p/~p. Waiting ~p seconds", [SyntaxError, NextRetry, ?MAX_RETRIES, WaitTime]),
            timer:sleep(WaitTime * 1000),
            with_retry(Fun, NextRetry);
        Result -> Result
    end;
with_retry(Fun, _) ->
    Fun().

%% @doc We have many objects that are scoped by other names, such as
%% orgs and users to ents, or projetcs to ents & orgs; and that are
%% uniquely identified by the 'scoping' names and their own names This
%% helper function is meant to delete such objects by their name and
%% scoping names.  It assumes that the `Module' defines a
%% `delete_by_scoping_params' sqerl query
-spec delete_unique_by_scoping_params(atom(), [any()], any()) ->
                                             {ok, 0 | 1} | {error, _Why}.
delete_unique_by_scoping_params(Module, ScopingNames, ObjName) ->
    Names = ScopingNames ++ [ObjName],
    case sqerl_rec:cquery(Module, delete_by_scoping_params, Names) of
        {ok, 0} = Result ->
            chef_log:debug("Could not delete ~p ~p : not found",
                            [Module, names_to_str(Names)]),
            Result;
        {ok, 1} = Result ->
            chef_log:info("Deleted ~p ~p", [Module, names_to_str(Names)]),
            Result;
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, delete_unique_by_scoping_params,
                                 [names_to_str(Names), ObjName], Why),
            Error
    end.

%% @doc Pretty much the same idea as
%% `delete_unique_by_scoping_params'; assumes the `Module' defines a
%% `fetch_by_scoping_params' sqerl query
-spec fetch_unique_by_scoping_params(atom(), [any()], any()) ->
                                            [deliv_sqerl_record()] |
                                            {error, not_found} |
                                            {error, any()}.
fetch_unique_by_scoping_params(Module, ScopingNames, ObjName) ->
    Names = ScopingNames ++ [ObjName],
    case sqerl_rec:qfetch(Module, fetch_by_scoping_params, Names) of
        [] ->
            chef_log:info("Failed to fetch ~p ~p : not found",
                           [Module, names_to_str(Names)]),
            {error, not_found};
        [Object] ->
            chef_log:log_item(debug, Object, "Successfully fetched ~p ~p",
                               [Module, names_to_str(Names)]),
            [Object];
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, fetch_unique_by_scoping_params,
                                    names_to_str(Names), Why),
            Error
    end.

%% @doc Same idea still, but for insert this time; assumes the
%% `Module' defines a `insert_by_scoping_params' sqerl query `Data'
%% must be a proplist containing the data to save (short of the ent id
%% of course), or an already prepared record
-spec insert_unique_by_scoping_params(atom(), [any()], Data) ->
                                             [deliv_sqerl_record()] |
                                             {error, no_enterprise} |
                                             {error, conflict} |
                                             {error, _Why} when
      Data :: proplist(atom(), any()) |
              deliv_sqerl_record().
insert_unique_by_scoping_params(Module, ScopingNames, PropList)
  when erlang:is_list(PropList) ->
    Rec = Module:fromlist(PropList),
    insert_unique_by_scoping_params(Module, ScopingNames, Rec);
insert_unique_by_scoping_params(Module, ScopingNames, Rec) when is_tuple(Rec) ->
    %% we remove the scoping parent's id field name from the insert fields
    {_ParentModule, ScopingFieldName} = Module:scoping_parent(),
    {[_ScopingField], Fields} = lists:partition(
                                  fun(FieldName) ->
                                          FieldName =:= ScopingFieldName
                                  end,
                                  Module:'#insert_fields'()
                                 ),
    Values = [Module:getval(FieldName, Rec) || FieldName <- Fields],
    ObjName = Module:getval(name, Rec),
    Names = ScopingNames ++ [ObjName],
    case sqerl_rec:qfetch(Module,
                          insert_by_scoping_params, ScopingNames ++ Values) of
        {error, {conflict, _}} ->
            chef_log:log_item(info, Rec, "Could not insert ~p ~p : conflict",
                               [Module, names_to_str(Names)]),
            {error, conflict};
        {error, {{ok, 0}, _}} ->
            chef_log:log_item(error, Rec,
                               "Could not insert ~p ~p : "
                               "scoping objects do not exist",
                               [Module, names_to_str(Names)]),
            {error, no_enterprise};
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, insert_unique_by_scoping_params,
                [Module, names_to_str(Names), Rec], Why),
            Error;
        [Object] = Result ->
            chef_log:log_item(info, Object, "Successfully inserted ~p ~p",
                               [Module, names_to_str(Names)]),
            Result
    end.

%% @doc Delete an object with given scoping params
-spec delete(atom(), [any()], any()) -> ok | {error, any()}.
delete(Module, Params, ObjName) ->
    case delete_unique_by_scoping_params(Module, Params, ObjName) of
        {ok, _} ->
            ok;
        {error, _Reason} = Error ->
            Error
    end.

%% @doc Fetch object given a name
-spec fetch(atom(), binary()) -> {error, not_found | _Why} |
                                 {ok, deliv_sqerl_record()}.
fetch(Module, ObjName) ->
    case fetch2(Module, name, ObjName) of
        {ok, [Obj]} ->
            {ok, Obj};
        {ok, []} ->
            {error, not_found};
        {ok, _} ->
            {error, multiple_results_found};
        {error, _} = Error ->
            Error
    end.

%% @doc Fetch object given an id
-spec fetch_by_id(atom(),
                  binary() | non_neg_integer()) -> {error, term() | not_found} |
                                                   {ok, deliv_sqerl_record()}.
fetch_by_id(Module, ObjId) ->
    case fetch2(Module, id, ObjId) of
        {ok, [Obj]} ->
            {ok, Obj};
        {ok, []} ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

%% @doc Fetch object given name and scoping params
-spec fetch(atom(), [binary()] | atom(),
            binary() | non_neg_integer() | any()) -> {error, any()} |
                                                     {ok, deliv_sqerl_record()}.
fetch(Module, Params, Name) when erlang:is_list(Params) ->
    case fetch_unique_by_scoping_params(Module, Params, Name) of
        [Obj] ->
            {ok, Obj};
        {error, _} = Error->
            Error
    end;
fetch(Module, By, Val) ->
    Query = chef_utils:join_atoms([fetch_by, '_', By]),
    qfetch(Module, Query, [Val]).

%% @doc Insert object into DB
-spec insert(atom(), binary()) -> db_op_result(deliv_sqerl_record()).
insert(Module, Name) ->
    handle_op_result(insert, sqerl_rec:insert(Module:from_name(Name)),
                     Module:from_name(Name)).

%% @doc Insert object while applying scoping params
-spec insert(atom(), [binary()],
             binary() | deliv_sqerl_record() | proplist(atom(), any()))
            -> db_op_result(deliv_sqerl_record()).
insert(Module, Params, Name) when erlang:is_binary(Name) ->
    insert_unique_by_scoping_params(Module, Params, [{name, Name}]);
insert(Module, Params, Data) ->
    insert_unique_by_scoping_params(Module, Params, Data).

%% @doc List all objects with given record type (module name)
-spec list_all(atom()) -> db_op_result(deliv_sqerl_record()).
list_all(Module) ->
    sqerl_rec:fetch_all(Module).

%% @doc Rename object to a new name given current name
-spec rename(atom(), binary() | deliv_sqerl_record(), binary()) ->
                    {error, conflict | not_found | any()} |
                    {ok, deliv_sqerl_record()}.
rename(Module, OldName, NewName) when erlang:is_binary(OldName)->
    case Module:fetch(OldName) of
        {error, not_found} ->
            {error, not_found};
        {ok, Obj} ->
            NewObj = Module:setvals([{name, NewName}], Obj),
            Module:update(NewObj)
    end;
rename(Module, Obj, NewName) ->
    NewObj = Module:setvals([{name, NewName}], Obj),
    Module:update(NewObj).

%% @doc Update object in DB
-spec update(deliv_sqerl_record()) -> {error, conflict | any()} |
                                      {ok, deliv_sqerl_record()}.
update(Obj) ->
    case handle_op_result(update, sqerl_rec:update(Obj), Obj) of
        [UpdatedObj] ->
            {ok, UpdatedObj};
        {error, _} = Error ->
            Error
    end.

%% @doc Sets the values from the list, then updates in the DB
-spec update(proplist(atom(), any()), deliv_sqerl_record())
            -> {error, conflict | any()} | {ok, deliv_sqerl_record()}.
update(PropList, Obj) when is_list(PropList) ->
    Module = erlang:element(1, Obj),
    UpdatedObj = Module:setvals(PropList, Obj),
    update(UpdatedObj).

%% @doc Return all object names of immediate children based on the
%%  Ent/Org/Proj hierarchy. Params is in the form of [EntName,
%%  OrgName, ..]
-spec fetch_names(atom(), [binary()]) -> db_op_result(binary()).
fetch_names(Module, Params) ->
    sqerl_rec:scalar_fetch(Module, fetch_names, Params).

%% @private @doc Builds a user-friendly identifier for scoped stuff to
%% integrate in log messages, such as `EntName/OrgName/ProjName'
names_to_str(Names) ->
    string:join(lists:map(fun chef_utils:to_str/1, Names), "/").

%% @doc Just a wrapper for `sqerl:select/4' with some logging
-spec select(atom(), atom(), [any()], atom(), [any()])
            -> {ok, [Row :: list() | tuple()]} | {error, _Why}.
select(Module, Query, Params, XformName, XformArgs) ->
    RealQ = chef_utils:join_atoms([Module, '_', Query]),
    OpResult = sqerl:select(RealQ, Params, XformName, XformArgs),
    CallDetail = {query, RealQ, Params},
    handle_op_result(select, OpResult, CallDetail).

-spec select(atom(), atom(), [any()], atom())
            -> {ok, [Row :: list() | tuple()]} | {error, _Why}.
select(Module, Query, Params, XformName) ->
    select(Module, Query, Params, XformName, []).

-spec select(atom(), atom(), [any()])
            -> {ok, [Row :: list() | tuple()]} | {error, _Why}.
select(Module, Query, Params) ->
    select(Module, Query, Params, identity, []).

%% @doc This simply runs the query, and then maps on the returned rows
-spec map_query(atom(), atom(), [any()], Fun)
               -> [FunResult] | {error, _Why} when
      Fun :: fun((Row) -> FunResult),
      Row :: proplist(binary(), any()),
      FunResult :: any().
map_query(Module, Query, Params, Fun) ->
    case select(Module, Query, Params) of
        {ok, Rows} -> lists:map(Fun, Rows);
        {error, _Why} = Error -> Error
    end.

%% @doc Simply runs a query and returns the result as is, as a JSON
-spec query_to_json(atom(), atom(), [any()]) -> [json()] | {error, _Why}.
query_to_json(Module, Query, Params) ->
    map_query(Module, Query, Params,
              fun(PropList) -> {PropList} end).
