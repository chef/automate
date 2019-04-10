%% @doc Represents an OAuth user alias
-module(deliv_oauth_user_alias).

-include("deliv_types.hrl").

-compile({parse_transform, sqerl_gobot}).

%% DB operations
-export([
         delete/1,
         delete/2,
         fetch/2,
         insert/3,
         update/1
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-record(deliv_oauth_user_alias, {
          id            :: db_id(),
          user_id       :: db_id(),
          oauth_app_id  :: db_id(),
          alias         :: binary()
         }).

'#insert_fields'() -> [user_id, oauth_app_id, alias].
'#update_fields'() -> [alias].

'#statements'() ->
    [default,
     {fetch_by_alias, sqerl_rec:gen_fetch(?MODULE, [oauth_app_id, alias])},
     {fetch_by_user_id, sqerl_rec:gen_fetch(?MODULE, [oauth_app_id, user_id])},
     {delete_by_scoping_params, sqerl_rec:gen_delete(?MODULE, [oauth_app_id, alias])}
    ].

'#table_name'() -> "oauth_user_aliases".

-spec delete(d_oauth_user_alias()) -> {ok, integer()} | {error, _}.
delete(UserAlias) ->
    deliv_db:delete(UserAlias).

-spec delete(binary(), binary()) -> ok | {error, _}.
delete(OauthAppId, AliasName) ->
    deliv_db:delete(?MODULE, [OauthAppId], AliasName).

-spec fetch(d_oauth_application(), binary() | non_neg_integer()) -> {error, atom()} |
                                                                    {ok, d_oauth_user_alias()}.
fetch(OauthApp, AliasName) when is_binary(AliasName) ->
    OauthAppId = deliv_oauth_application:getval(id, OauthApp),
    do_fetch(deliv_db:qfetch(?MODULE, fetch_by_alias, [OauthAppId, AliasName]),
             OauthApp, alias, AliasName);
fetch(OauthApp, UserId) when is_integer(UserId) ->
    OauthAppId = deliv_oauth_application:getval(id, OauthApp),
    do_fetch(deliv_db:qfetch(?MODULE, fetch_by_user_id, [OauthAppId, UserId]),
             OauthApp, user_id, UserId).

-spec insert(non_neg_integer(), non_neg_integer(), binary()) -> {ok, d_oauth_user_alias()} |
                                                                {error, atom()}.
insert(OauthAppId, UserId, AliasName) ->
    case deliv_db:insert(#deliv_oauth_user_alias{user_id = UserId,
                                                 oauth_app_id = OauthAppId,
                                                 alias = AliasName}) of
        [InsertedAlias] ->
            chef_log:info("Added alias ~s for user #~p to OAuth application #~p",
                           [AliasName, UserId, OauthAppId]),
            {ok, InsertedAlias};
        {error, Why} = Err ->
            chef_log:error("Failed to insert alias ~s for user #~p associated with Oauth application #~p beacuse ~p",
                            [AliasName, UserId, OauthAppId, Why]),
            Err
    end.

-spec update(d_oauth_user_alias()) -> {ok, d_oauth_user_alias()} | {error, atom()}.
update(UserAlias) ->
    case deliv_db:update(UserAlias) of
        {ok, _User} = Result ->
            Result;
        {error, Why} = Error ->
            chef_log:error("Failed to update alias for user #~p associated with Oauth application #~p beacuse ~p",
                            [getval(user_id, UserAlias), getval(oauth_app_id, UserAlias), Why]),
            Error
    end.

%% @private
do_fetch([], OauthApp, FetchBy, FetchValue) ->
    AppName = deliv_oauth_application:getval(name, OauthApp),
    chef_log:error("Could not find user associated for OAuth application ~s with ~s ~s",
                    [AppName, FetchBy, FetchValue]),
    {error, not_found};
do_fetch([Alias], _OauthApp, _FetchBy, _FetchValue) ->
    {ok, Alias};
do_fetch({error, Why} = Err, OauthApp, FetchBy, FetchValue) ->
    AppName = deliv_oauth_application:getval(name, OauthApp),
    chef_log:error("Failed not find user associated for OAuth application ~s with ~s ~s - ~p",
                    [AppName, FetchBy, FetchValue, Why]),
    Err.
