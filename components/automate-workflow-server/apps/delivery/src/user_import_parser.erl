%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc Provides functions to import internal users from a TSV input
%% Accepts both binaries and lists strings as inputs, but always returns binary data.

-module(user_import_parser).

-export([
         intern_users_from_tsv/1
        ]).

-include("deliv_types.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.

%% @doc Returns a list of `#intern_user{}' records given a string containing appropriate TSV data
%% The data has to respect the following syntax for each line:
%% ```
%%     enterprise_name\tfirst_name\tlast_name\tusername[\tpassword[\temail[\tssh_pub_key]]]
%% '''
%% where `\t' is a tab
%% Only the first four fields are mandatory.
-spec intern_users_from_tsv(Input) -> [d_intern_user()] when
    Input :: str_or_binary().
intern_users_from_tsv(Input) ->
    intern_users_from_tsv(Input, fun fetch_enterprise_id/1).

%% @private
%% @doc Internal function that can use any `EnterpriseIdFetcher'
-spec intern_users_from_tsv(Input, EnterpriseIdFetcher)
        -> [d_intern_user() | {error, {invalid_intern_user_input, [binary() | undefined]}}] when
      Input :: str_or_binary(),
      EnterpriseIdFetcher :: fun((EnterpriseName) -> {ok, Id} | {error, _}),
      EnterpriseName :: binary(),
      Id :: integer().
intern_users_from_tsv(Input, EnterpriseIdFetcher) when erlang:is_list(Input) ->
    intern_users_from_tsv(erlang:iolist_to_binary(Input), EnterpriseIdFetcher);
intern_users_from_tsv(Input, EnterpriseIdFetcher) when erlang:is_binary(Input) ->
    tsv_parser:process_tsv_string_with(
        Input,
        fun(eof, {Users, _IdFetcher}) -> Users;
           (empty_line, Acc) -> Acc;
           ({line, Line}, {Users, IdFetcher}) ->
               {NewUser, NewIdFetcher} = intern_user_from_line(Line, IdFetcher),
               {[NewUser | Users], NewIdFetcher}
        end,
        {[], chef_utils:caching_wrapper(EnterpriseIdFetcher)},
        7 % min elements per line
    ).

%% @private
-spec intern_user_from_line([binary() | undefined], fun())
        -> {d_intern_user() | {error, {invalid_intern_user_input, [binary() | undefined]}}, fun()}.
intern_user_from_line(Line = [EnterpriseName, FirstName, LastName, UserName,
        Password, Email, SshPubKey], CachedIdFetcher) ->
    %% none of the first 4 fields can be `undefined'
    case lists:member(undefined, lists:sublist(Line, 4)) of
        true ->
            chef_log:error("Missing data when importing users on line: ~p", [Line]),
            {{error, {invalid_intern_user_input, Line}}, CachedIdFetcher};
        %% the normal case
        false ->
            %% TODO: handle error case from IdFetcher/1
            {NewCachedIdFetcher, {ok, EnterpriseId}} = CachedIdFetcher(EnterpriseName),
            case user_password:hash(Password) of
                {error, Reason, Message} ->
                    {{error, {Reason, Message, Password}}, NewCachedIdFetcher};
              HashedPassword ->
                    HashType = <<"bcrypt">>,
                    {[
                      {enterprise_name, EnterpriseName},
                      {enterprise_id, EnterpriseId},
                      {first_name, FirstName},
                      {last_name, LastName},
                      {name, UserName},
                      {hashed_pass, HashedPassword},
                      {hash_type, HashType},
                      {email, Email},
                      {ssh_pub_key, SshPubKey}
                     ],
                     NewCachedIdFetcher}
            end
    end;
%% a line that's too long!
intern_user_from_line(Line, CachedIdFetcher) ->
    chef_log:error("Unexpectedly long line when importing users: ~p", [Line]),
    {{error, {invalid_intern_user_input, Line}}, CachedIdFetcher}.

-spec fetch_enterprise_id(binary()) -> {ok, non_neg_integer()} | {error, _Why}.
fetch_enterprise_id(EntName) ->
    case deliv_enterprise:fetch(EntName) of
        {ok, Ent} -> {ok, deliv_enterprise:getval(id, Ent)};
        {error, _Why} = Error -> Error
    end.
