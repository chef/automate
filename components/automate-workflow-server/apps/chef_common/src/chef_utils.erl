-module(chef_utils).

%% @doc Groups a few helper funs together

-include("chef_common.hrl").
-include_lib("mixer/include/mixer.hrl").

%% Timeout in milliseconds to get feedback (output, exit status, etc)
%% from run_cmd/1|2  while the command is executing.
-define(DEFAULT_GATHER_DATA_TIMEOUT, 10000).

-export([
         caching_wrapper/1,
         capitalize_str/1,
         db_now/0,
         format_timestamp/1,
         format_db_timestamp/1,
         is_genuine_list/1,
         iodata_strify/1,
         join_atoms/1,
         join_binaries/2,
         merge_proplists/2,
         parse_timestamp/1,
         priv_dir_for_mod/1,
         random_string/1,
         random_hex_string/1,
         run_cmd/1, run_cmd/2, run_cmd/3, run_cmd/4,
         run_exe/2, run_exe/3,
         trunc_timestamp/1,
         to_bin/1,
         to_bin/2,
         to_str/1,
         to_int/1,
         to_atom/1,
         iodata_to_str/1,
         string_to_term/1,
         read_config_file/1,
         find_executable/1
        ]).

%% This enabled mocking of functions we could not mock (such as
%% erlang:open_port/2) by making them available as functions of this module
%% (such as chef_utils:open_port/2).
-mixin([{erlang, [open_port/2, port_info/1, port_close/1, send_after/3]}]).

%% An IOList where every sub-element is a string
-type str_or_list_of_str() :: string() | [str_or_list_of_str()].

%% calendar doesn't export these
-type hour()   :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.

-spec to_bin(atom() | list() | binary() | integer() | tuple()) -> binary().
to_bin(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
to_bin(Bin) when erlang:is_binary(Bin) ->
    Bin;
to_bin(Int) when erlang:is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_bin(List) when erlang:is_list(List) ->
    erlang:iolist_to_binary(List);
to_bin(Tuple) when erlang:is_tuple(Tuple) ->
    to_bin(lists:flatten(io_lib:format("~p", [Tuple]))).

-spec to_bin(string(), list()) -> binary().
to_bin(Format, Data) ->
    to_bin(io_lib:format(Format, Data)).

-spec to_str(atom() | string() | binary() | integer() | tuple()) -> string().
to_str(Atom) when is_atom(Atom) ->
    erlang:atom_to_list(Atom);
to_str(Bin) when erlang:is_binary(Bin) ->
    erlang:binary_to_list(Bin);
to_str(Int) when erlang:is_integer(Int) ->
    erlang:integer_to_list(Int);
to_str(Str) when erlang:is_list(Str) ->
    Str;
to_str(Tuple) when erlang:is_tuple(Tuple) ->
    lists:flatten(io_lib:format("~p", [Tuple])).

%% @doc Translates an iodata element, so that each element within it,
%% even nested items, are translated to a string. This can be useful
%% for when mixing binary and regular strings in the construction of
%% a command argument, but where you want a final version that is all
%% ordinary strings
%%
%% Examples:
%%           "123"            -> "123"
%%           <<"123">>        -> "123"
%%           ["abc", 123]     -> ["abc","123"]
%%           [<<"one">>, "two", [$t, ["h","ree"]]]
%%                            -> ["one","two",["t",["h","ree"]]]
%%
-spec iodata_strify(iodata()) -> str_or_list_of_str().
iodata_strify(IoData) when erlang:is_binary(IoData) ->
    to_str(IoData);
iodata_strify(IoData) ->
    iodata_strify2(IoData).

%% @private
-spec iodata_strify2(iodata()) -> str_or_list_of_str().
iodata_strify2(IoData) when erlang:is_list(IoData) ->
    case io_lib:printable_list(IoData) of
        true ->
            IoData;
        false ->
            [iodata_strify2(X) || X <- IoData]
    end;
iodata_strify2(IoData) ->
    case is_alpha(IoData) of
        true ->
            [IoData];
        false ->
            to_str(IoData)
    end.

%% @doc Generates a random binary string by b64-encoding `N' random bytes
-spec random_string(non_neg_integer()) -> binary().
random_string(N) ->
    RandBytes = base64:encode(crypto:rand_bytes(N)),
    re:replace(RandBytes, "[/=+]", "", [global, {return, binary}]).

%% @doc Generates a random hex string by b16-encoding `N' random bytes
-spec random_hex_string(non_neg_integer()) -> binary().
random_hex_string(N) ->
    RandBytes = crypto:rand_bytes(N),
    binary_to_hex_str(RandBytes).

%% @private
%% @doc Turns a binary chunk into a lower-case hex binary string
-spec binary_to_hex_str(binary()) -> binary().
binary_to_hex_str(Bin) ->
    erlang:iolist_to_binary([io_lib:format("~2.16.0b", [X])
                      || X <- erlang:binary_to_list(Bin)]).

%% @doc Same as `run_cmd/2', but in the current working dir
-spec run_cmd([string()]) ->
                     {Status, StdErrAndOut} |
                     {error, timeout} when
      Status :: integer(),
      StdErrAndOut :: binary().
run_cmd(CmdList) ->
    {ok, Cwd} = file:get_cwd(),
    run_cmd(CmdList, Cwd, []).

%% @doc Same as `run_cmd/3', but with no additional env variables
-spec run_cmd([string()], string()) ->
    {integer(), binary()} | {error, timeout}.
run_cmd(CmdList, Dir) ->
    run_cmd(CmdList, Dir, []).

%% @doc Runs a command in the given `Dir'
%% with the given additional env variables
-spec run_cmd([string()], string(), Env) ->
                     {Status, StdErrAndOut} |
                     {error, timeout} when
      Env :: proplist(string(), string() | false),
      Status :: integer(),
      StdErrAndOut :: binary().
run_cmd(CmdList, Dir, Env) ->
    run_cmd(CmdList, Dir, Env, ?DEFAULT_GATHER_DATA_TIMEOUT).

%% @doc Runs a command in the given `Dir' with given additional env variables,
%% and an additional timeout
-spec run_cmd([string()], string(), Env, TimeoutMs) ->
                     {Status, StdErrAndOut} |
                     {error, timeout} when
      Env :: proplist(string(), string() | false),
      TimeoutMs :: integer(),
      Status :: integer(),
      StdErrAndOut :: binary().
run_cmd(CmdList, Dir, Env, TimeoutMs) ->
    Cmd = string:join(CmdList, " "),
    EnvWithPath = add_path_to_env(Env),
    Port = erlang:open_port({spawn, Cmd},
                            [exit_status,
                             binary,
                             stderr_to_stdout,
                             {env, add_proxy_to_env(EnvWithPath)},
                             {cd, Dir}]),
    gather_data(Port, TimeoutMs, <<>>).

-spec run_exe([string()], string()) ->
                     {Status, StdErrAndOut} |
                     {error, timeout} when
      Status :: integer(),
      StdErrAndOut :: binary().
run_exe(CmdList, Dir) ->
    run_exe(CmdList, Dir, []).

-spec run_exe([string()], string(), Env) ->
                     {Status, StdErrAndOut} |
                     {error, timeout} when
      Env :: proplist(string(), string() | false),
      Status :: integer(),
      StdErrAndOut :: binary().
run_exe(CmdList, Dir, Env) ->
    [Cmd|Args] = CmdList,
    EnvWithPath = add_path_to_env(Env),
    Port = erlang:open_port({spawn_executable, Cmd},
                            [{args, Args},
                             exit_status,
                             binary,
                             stderr_to_stdout,
                             {env, add_proxy_to_env(EnvWithPath)},
                             {cd, Dir}]),
    gather_data(Port, 10000, <<>>).

%% Do I need to escape these when setting them as environment variables?
%% Probably not...  but I don't really know.
fmt_proxy_port(Host, 0) -> Host;
fmt_proxy_port(Host, "") -> Host;
fmt_proxy_port(Host, Port) -> io_lib:format("~s:~B", [Host, Port]).

fmt_proxy_user(HostPort, "", _) -> HostPort;
fmt_proxy_user(HostPort, User, Password) -> io_lib:format("~s:~s@~s", [User, Password, HostPort]).

fmt_proxy("") -> "";
fmt_proxy(Host) ->
    HostPort = fmt_proxy_port(Host, delivery_app:get_env(proxy_port, 0)),
    User = delivery_app:get_env(proxy_user, ""),
    Password = delivery_app:get_env(proxy_password, ""),
    fmt_proxy_user(HostPort, User, Password).

%% Add current path to the environment if the path isn't already defined in the env
-spec add_path_to_env(Env) -> Env when
      Env :: proplist(string(), string() | false).
add_path_to_env(Env) ->
    case {proplists:is_defined("PATH", Env), os:getenv("PATH")} of
        {true, _} -> Env;
        {false, false} -> Env;
        {false, Path} -> [{"PATH", Path}| Env]
    end.

%% Add any proxy configuration provided in the delivery application configuration
%% to the given environment proplist.
-spec add_proxy_to_env(Env) -> Env when
      Env :: proplist(string(), string() | false).
add_proxy_to_env(Env) ->
    case fmt_proxy(delivery_app:get_env(proxy_host, "")) of
        "" -> Env;
        Proxy ->
            Proxies = [{"http_proxy", Proxy}, {"https_proxy", Proxy}],
            case delivery_app:get_env(no_proxy, []) of
                [] ->
                    Env ++ Proxies;
                NoProxy ->
                    Env ++ [{"no_proxy", string:join(NoProxy, ",")} | Proxies]
            end
    end.

-spec gather_data(port(), integer(),
                  Acc) ->
                         {Status, any()} |
                         {error, timeout} when
      Status :: integer(),
      Acc    :: any().
gather_data(Port, Timeout, Acc) ->
    receive
        %% should refactor the return out of this
        {Port, {exit_status, Status}} ->
            {Status, Acc};
        {Port, {data, Part}} ->
            gather_data(Port, Timeout, <<Acc/binary, Part/binary>>)
    after Timeout ->
            {error, timeout}
    end.

%% @doc Return the priv directory of the application that contains the
%% module `Mod'. Unlike `code:priv_dir/1', this should work reliably
%% for releases and test code during development.
-spec priv_dir_for_mod(atom()) -> string().
priv_dir_for_mod(Mod) ->
    case code:which(Mod) of
        non_existing ->
            {error, bad_name};
        Which ->
            %% Compute the abs path on the filename first. If the code
            %% is in cwd, this avoids a problem that `"." =
            %% filename:dirname(".")' and when cwd is `foo',
            %% `filename:absname(".")' gives `.../foo/.'. For rebar
            %% eunit test runs, cwd is `.eunit' and we hit this case.
            AbsWhich = filename:absname(Which),
            Ebin = filename:dirname(AbsWhich),
            filename:join(filename:dirname(Ebin), "priv")
    end.

%% @doc Ripped from sqerl_rec, and useful to deal with the same!
-spec join_atoms([atom()]) -> atom().
join_atoms(Atoms) when erlang:is_list(Atoms) ->
    Bins = [to_bin(A) || A <- Atoms],
    erlang:binary_to_atom(erlang:iolist_to_binary(Bins), utf8).

-spec read_config_file(file:name_all()) -> {ok, binary()} |
                                           {error, file:posix() |
                                                   badarg |
                                                   terminated |
                                                   system_limit}.
read_config_file(ConfigFile) ->
    file:read_file(ConfigFile).

%%
%% Helper functions
%%

%% @doc Returns true if char is alphanumeric, false otherwise
is_alpha(Char) when Char >= $a, Char =< $z ->
    true;
is_alpha(Char)  when Char >= $A, Char =< $Z ->
    true;
is_alpha(Char) when Char >= $0, Char =< $9 ->
    true;
is_alpha(_) ->
    false.

%% @doc Due to a quirk (?) in the database interaction, we receive
%% timestamps as *almost* a proper `calendar:datetime()' tuple, except
%% that the "seconds" portion is a float and not an integer. (This
%% even appears to occur if we declare the relevant database columns
%% to have no microsecond precision.) As a result, we need to truncate
%% the float seconds we receive to create a proper
%% `calendar:datetime()' instance.
-spec trunc_timestamp({calendar:date(),
                       {hour(), minute(), second() | float()}}) -> calendar:datetime().
trunc_timestamp({Date, {H, Min, S}}) ->
    {Date, {H, Min, trunc(S)}}.

%% @doc Transform a datetime tuple into a binary string in the form of
%% `<<2014-08-27 02:07:22">>'
%% which is ISO8601-valid
-spec format_timestamp(calendar:datetime()) -> binary().
format_timestamp(Timestamp) ->
    erlang:list_to_binary(ec_date:format("c", trunc_timestamp(Timestamp))).

%% @doc Transform a datetime tuple into a binary string including timezone in
%% the form of `<<"2014-08-27T2:07:22Z">>` which is ISO8601-valid and will not
%% be interpreted by postgres as being in localtime.
-spec format_db_timestamp(calendar:datetime()) -> binary().
format_db_timestamp(Timestamp) ->
    erlang:list_to_binary(ec_date:format_iso8601(Timestamp)).

-spec parse_timestamp(str_or_binary()) -> calendar:datetime().
parse_timestamp(Str) ->
    ec_date:parse(to_str(Str)).

%% @doc Parses a string into an int, if possible
-spec to_int(str_or_binary()) -> integer() | {error, not_an_int}.
to_int(Str) when erlang:is_list(Str) ->
    to_int(Str, list_to_integer);
to_int(Bin) when erlang:is_binary(Bin) ->
    to_int(Bin, binary_to_integer).
%% @private
to_int(Data, CastFunName) ->
    try erlang:CastFunName(Data)
    catch error:badarg ->
        {error, not_an_int}
    end.

%% @doc Parses a binary into an atom, if possible
-spec to_atom(str_or_binary()) -> atom().
to_atom(Str) when erlang:is_list(Str) ->
    to_atom(Str, list_to_existing_atom);
to_atom(Bin) when erlang:is_binary(Bin) ->
    to_atom(Bin, binary_to_existing_atom).
%% @private
to_atom(Data, CastFunName) ->
    try erlang:CastFunName(Data, utf8)
    catch error:badarg ->
        {error, not_an_atom}
    end.

%% @doc Turns an IOData into a string
-spec iodata_to_str(iodata()) -> string().
iodata_to_str(Bin) when erlang:is_binary(Bin) ->
    erlang:binary_to_list(Bin);
iodata_to_str(IOList) when erlang:is_list(IOList) ->
    erlang:binary_to_list(erlang:iolist_to_binary(IOList)).

%% @doc This is useful if you have a deterministic (at least for successes)
%% function that is somewhat expensive (e.g. makes DB calls) and that's going
%% to be called a number of times with some (expected) repetitivity in the
%% argument.
%% This wraps your function and memoizes the results.
%% The wrapped function should return `{ok, Result}' on success, in which case
%% `Result' will me memoized, or `{error, _Why}' on failure, which will prevent
%% it from being memoized
-spec caching_wrapper(Fun) -> WrappedFun when
    Fun :: fun((Arg) -> FunResult),
    WrappedFun :: fun((Arg) -> {WrappedFun, FunResult}),
    Arg :: any(),
    FunResult :: {ok, any()} | {error, _Why}.
caching_wrapper(Fun) ->
    make_caching_wrapper(Fun, dict:new()).

%% @private
make_caching_wrapper(Fun, Dict) ->
    fun(Arg) ->
        {NewDict, Result} = case dict:find(Arg, Dict) of
            {ok, _} = FunResult ->
                {Dict, FunResult};
            error ->
                FunResult = Fun(Arg),
                {case FunResult of
                    {ok, Value} -> dict:store(Arg, Value, Dict);
                    {error, _} -> Dict
                end, FunResult}
        end,
        {make_caching_wrapper(Fun, NewDict), Result}
    end.

%% @doc Returns true iff the argument is a 'genuine' list
%% (as opposed to a string)
-spec is_genuine_list(any()) -> boolean().
is_genuine_list(List) when is_list(List) -> not io_lib:printable_list(List);
is_genuine_list(_Other) -> false.

%% @doc Similar to string:join/2, takes a list of binary strings and joins
%% them with a Separator (can be <<>>) added between strings.
-spec join_binaries([binary()], binary()) -> binary().
join_binaries([], _Separator) ->
    <<>>;
join_binaries(L, <<>>) ->
    erlang:iolist_to_binary(L);
join_binaries([H | R], Separator) ->
    IOList = lists:foldl(fun(Bin, Acc) -> [Bin, Separator | Acc] end, [], R),
    IOList2 = [H | lists:reverse(IOList)],
    erlang:iolist_to_binary(IOList2).

%% @doc Takes a string representation of a term ("{foo, bar}") and turns it
%% into a tuple.
-spec string_to_term(string()) -> any().
string_to_term(String) ->
    {ok, ItemTokens, _} = erl_scan:string(String ++ "."),
    {ok, T} = erl_parse:parse_term(ItemTokens),
    T.

%% @doc Takes a string and returns it with the first letter capitalized.
-spec capitalize_str(atom() | string() | binary() | integer()) -> binary().
capitalize_str(String) ->
    [H | T] = chef_utils:to_str(String),
    erlang:iolist_to_binary([string:to_upper(H), T]).

%% @doc Merges two proplists. If both lists have an entry for a given key
%% the value from the first list is used and the value from the second list
%% is discarded.
-spec merge_proplists(proplist(), proplist()) -> proplist().
merge_proplists(L1, L2) ->
    SL1 = lists:ukeysort(1, L1),
    SL2 = lists:ukeysort(1, L2),
    lists:ukeymerge(1, SL1, SL2).

find_executable(Executable) ->
   os:find_executable(Executable).

%% @doc Returns a binary representation of the current time suitable for
%% sending to the database with the seconds as an integer.
-spec db_now() -> binary().
db_now() ->
    format_db_timestamp(calendar:universal_time()).
