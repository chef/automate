%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

%%% @doc EUnit helper functions for setting up and tearing down
%%% Postgres database data.
%%%
%%% Note that the setup will truncate the entire test database,
%%% leaving each test with a clean slate from which to work.
%%%
%%% Do NOT add test-specific test data to this module, or Oliver will
%%% hunt you down. That stuff goes with your tests!
%%%
%%% Example Usage:
%%%
%%%   setup() ->
%%%     error_logger:tty(false),
%%%     eu_database:setup().
%%%
%%%   teardown(_) ->
%%%     eu_database:teardown(),
%%%     error_logger:tty(true).
%%%
-module(eu_database).

-define(TEST_DB_NAME, "delivery_eunit").
-define(DB_HOST, os:getenv("PGHOST", "localhost")).
-define(DB_PORT, 5432).
-define(DB_PASS, "delivery").

-export([
         setup/0,
         teardown/0
        ]).

database_environment_setup() ->
    User = get_user(),
    SqerlEnv = [{db_host, ?DB_HOST},
                {db_port, ?DB_PORT},
                {db_user, User},
                {db_pass, ?DB_PASS},
                {db_name, ?TEST_DB_NAME},
                {idle_check, 10000},
                {prepared_statements, {sqerl_rec, statements, [[{app, delivery},
                                                                {app, github},
                                                                {app, scm},
                                                                {app, audit},
                                                                {app, notification},
                                                                {app, auth},
                                                                {app, jobs}]]}},
                {column_transforms, []}],
    [ ok = application:set_env(sqerl, Key, Val) || {Key, Val} <- SqerlEnv ],

    PoolConfig = [{name, sqerl},
                  {max_count, 10},
                  {init_count, 3},
                  {start_mfa, {sqerl_client, start_link, []}}],
    ok = application:set_env(pooler, pools, [PoolConfig]).

applications() ->
    [crypto, asn1, public_key, ssl, pooler, epgsql, sqerl].

get_user() ->
    case os:getenv("PGUSER") of
        false -> os:getenv("USER");
        User -> User
    end.

setup() ->
    database_environment_setup(),
    [ application:start(A) || A <- applications() ],
    sqerl:execute(<<"TRUNCATE enterprises CASCADE">>).

teardown() ->
    [ application:stop(A) || A <- lists:reverse(applications()) ].
