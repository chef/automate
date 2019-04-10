-module(deliv_token).

-include("deliv_types.hrl").

-export([
         assign_token/2,
         is_authorized/2,
         new/0,
         extract_token_info/1,
         verify_token/4
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(TOKEN_NUM_RAND_BYTES, 32).
-define(TOKEN_HEADER, <<"chef-delivery-token">>).
-define(USER_HEADER,  <<"chef-delivery-user">>).
-define(ENT_HEADER,  <<"chef-delivery-enterprise">>).

%% @doc Candidates are authentication tokens paired with their
%% birthday. Both are required to perform a complete authentication
%% check.
-type candidate() :: {token(), calendar:datetime()}.

%% Note that with crypto 3.7, rand_bytes/1 was deprecated.
-spec new() -> token().
new() ->
    base64url:encode(crypto:strong_rand_bytes(?TOKEN_NUM_RAND_BYTES)).

%% @doc Generates and assigns a new authentication token to the user
%% in the given enterprise. If the user is not in the enterprise, the
%% token is not assigned.
-spec assign_token(EnterpriseName, UserName) -> {ok, Token} |
                                                {error, token_not_added |
                                                 term()} when
      EnterpriseName :: binary(),
      UserName :: binary(),
      Token :: token().
assign_token(EnterpriseName, UserName) ->
    Token = new(),
    case deliv_db:select(deliv_user, assign_token,
                         [EnterpriseName, UserName, Token]) of
        {ok, []} ->
            %% This could be because either the enterprise or the user
            %% don't exist; if we want to surface the exact cause, we
            %% can add more robust error handling to the assign_token
            %% database procedure
            {error, token_not_added};
        {ok, _} ->
            {ok, Token};
        {error, _} = Error ->
            Error
    end.

%% @doc A Cowboy callback for mixing in. Alas, because naming is hard,
%% this is actually *authentication*.
-spec is_authorized(Req, State) -> {Result, Req1, State1} when
      Req :: cowboy_req:req(),
      Req1 :: cowboy_req:req(),
      State :: #handler{},
      State1 :: #handler{},
      Result :: term().
is_authorized(Req, State) ->
    %% We need to accept both types of tokens because the builders
    %% currently are given an A1 token. Eventually we should get an A2
    %% token for them and remove this code.  TODO add some checking to
    %% restrict this to builders or otherwise prevent migrated A2
    %% users from using the A1 path.

    %% We should also look at refactoring and unifying some of the
    %% code since is_a2_token, and the is_authorized_aX_token code
    %% have similar shapes and paths.
    IsA2Token = is_a2_token(Req),
    case {IsA2Token, envy:get(delivery, a2_mode, false, boolean)} of
        {true, true} ->
            is_authorized_a2_token(Req, State);
        _ ->
            is_authorized_a1_token(Req, State)
    end.

-spec is_authorized_a1_token(Req, State) -> {Result, Req1, State1} when
      Req :: cowboy_req:req(),
      Req1 :: cowboy_req:req(),
      State :: #handler{},
      State1 :: #handler{},
      Result :: term().
is_authorized_a1_token(Req, State) ->
    case verify(Req, State) of
        {token_ok, EntName, UserName, Req1} ->
            NewState = State#handler{ent_name=EntName,
                                     user_name=UserName},
            {true, Req1, NewState};
        {Reason, Req1} when Reason =:= token_expired orelse
                            Reason =:= token_denied ->
            Msg  = {[{<<"error">>, erlang:atom_to_binary(Reason, utf8)}]},
            Body = chef_json:encode(Msg),
            Req2 = cowboy_req:set_resp_body(Body, Req1),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>,
                                              <<"application/json">>,
                                              Req2),
            {{false, ?WWW_AUTHENTICATE}, Req3, State};
        {{error, Reason}, Req1} ->
            chef_log:error("Error verifying token: ~p", [Reason]),
            deliv_web_utils:error_response(500, <<"system error">>, Req1, State)
    end.

-spec is_authorized_a2_token(Req, State) -> {Result, Req1, State1} when
      Req :: cowboy_req:req(),
      Req1 :: cowboy_req:req(),
      State :: #handler{},
      State1 :: #handler{},
      Result :: term().
is_authorized_a2_token(Req, State) ->
    {EntName, UserName, Token, Req1} = extract_token_info(Req),
    Headers = [{"Authorization: Bearer", Token}],
    Url = envy:get(delivery, a2_authenticate_endpoint, <<"http://0.0.0.0:10162/api/authenticate">>, string),
    case deliv_http:req(get, Url, [], Headers) of
        {ok, 200, _RespHeaders, _RespBody} ->
            NewState = State#handler{ent_name = EntName,
                                     user_name = UserName},
            {true, Req1, NewState};
        %% A2 authn_service does not differentiate between
        %% token_expired or token_denied. We return token_denied_by_a2
        %% to help delivery-cli
        {ok, 401, _RespHeaders, _RespBody} ->
            MsgBody = chef_json:encode({[{error, token_denied_by_a2}]}),
            Req2 = cowboy_req:set_resp_body(MsgBody, Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>,
                                              <<"application/json">>,
                                              Req2),
            {{false, ?WWW_AUTHENTICATE}, Req3, State};
        {error, Reason} ->
            chef_log:error("Error verifying token: ~p", [Reason]),
            deliv_web_utils:error_response(500, <<"system error">>, Req, State)
    end.

is_a2_token(Req) ->
    {_EntName, _UserName, Token, _Req1} = extract_token_info(Req),
    %% This is objectively terrible, but works well enough for now.
    %% Longer term, we should actually parse the token and decide.
    case Token of
        undefined ->
            false; %% The is_authorized_a2_token function doesn't handle undefined, but the a1 version does.
        _ ->
            length(binary:matches(Token, <<".">>)) > 0
    end.

%% @doc Perform token-based authentication of a request, respecting
%% TTL policy. Upon successful authentication, returns both the
%% enterprise name and user name to the caller. See documentation for
%% the `#handler{}' record for more.
-spec verify(Req, State) -> {token_ok, EnterpriseName, UserName, ResultReq} |
                            {token_expired, ResultReq} |
                            {token_denied, ResultReq} |
                            {{error, Reason}, ResultReq} when
      Req :: cowboy_req:req(),
      ResultReq :: cowboy_req:req(),
      State :: #handler{},
      EnterpriseName :: binary(),
      UserName :: binary() | undefined,
      Reason :: term().
verify(Req, State) ->
    {Method, Req} = cowboy_req:method(Req),
    Ttl = ttl_for(action(Method), State),

    {EntName, UserName, Token, Req1} = extract_token_info(Req),

    case verify_token(EntName, UserName, Token, Ttl) of
        token_ok ->
            {token_ok, EntName, UserName, Req1};
        token_expired ->
            {token_expired, Req1};
        token_denied ->
            {token_denied, Req1};
        {error, _Reason} = Error ->
            {Error, Req1}
    end.

%% @doc Convert an HTTP method into an action in order to determine
%% the proper token TTL to use for authenticating the request ("read"
%% and "write" operations can have different policies).
%%
%% Mapping based on HTTP method is admittedly rather crude, but it'll
%% suffice for now.
action(<<"GET">>)     -> read;
action(<<"HEAD">>)    -> read;
action(<<"OPTIONS">>) -> read;
action(<<"PUT">>)     -> write;
action(<<"DELETE">>)  -> write;
action(<<"POST">>)    -> write.

%% @doc Convenience function for extracting a specific TTL from the
%% handler state. See `delivery_sup:set_ttls/1' for more.
ttl_for(read, #handler{read_ttl=ReadTtl})    -> ReadTtl;
ttl_for(write, #handler{write_ttl=WriteTtl}) -> WriteTtl.

%% @doc Extract all information necessary for token authentication
%% from an HTTP request. Does not modify the request object at all.
%%
%% NOTE: Similar to `verify_token/4' below, this implementation
%% assumes we're only authenticating *users* and not build nodes. Once
%% we get build nodes implemented, however, we can probably inspect
%% the request here and extract the relevant information relatively
%% transparently.
-spec extract_token_info(Req) -> {EnterpriseName, UserName, Token, Req} when
      Req :: cowboy_req:req(),
      EnterpriseName :: binary(),
      UserName :: binary() | undefined,
      Token :: binary() | undefined.
extract_token_info(Req) ->
    %% If the endpoint is not enterprise scoped, request is coming from Visbility,
    %% safe to pull the enterprise out of header.
    {EntName, Req1} = get_entname(Req),
    {UserName, Req2} = extract_from_req(?USER_HEADER, Req1),
    {Token, Req3}    = extract_from_req(?TOKEN_HEADER, Req2),

    %% When the token, enterprise or username comes to us in a cookie it's uri encoded
    DecodedToken = deliv_web_utils:decode_url(Token),
    DecodedUserName = deliv_web_utils:decode_url(UserName),
    DecodedEntName = deliv_web_utils:decode_url(EntName),

    {DecodedEntName, DecodedUserName, DecodedToken, Req3}.

extract_from_req(Key, Req) ->
    case cowboy_req:header(Key, Req) of
        {undefined, Req1} ->
            case cowboy_req:cookie(Key, Req1) of
                {undefined, Req2} ->
                    cowboy_req:qs_val(Key, Req2);
                Val ->
                    Val
            end;
        Val ->
            Val
    end.

get_entname(Req) ->
    case cowboy_req:binding(ent_name, Req) of
        {undefined, NewReq} -> extract_from_req(?ENT_HEADER, NewReq);
        Result -> Result
    end.

%% @doc Determine if the given token belongs to the specified user and
%% is still alive, according to the TTL.
%%
%% NOTE: This is specifically authenticating a *user*, a
%% flesh-and-blood person with a Delivery account, and not a build
%% node that is communicating with the Delivery server in the course
%% of carrying out its operations. That side of the system doesn't
%% even *exist* yet, so this is not a huge inconvenience right
%% now. I'm stating this explicitly for those who come after when
%% implementing that functionality.
-spec verify_token(EntName, UserName, Token, Ttl) -> token_ok |
                                                     token_expired |
                                                     token_denied |
                                                     {error, Reason} when
      EntName :: binary(),
      UserName :: binary() | undefined,
      Token :: token() | undefined,
      Ttl :: deliv_ttl(),
      Reason :: term().
verify_token(_EntName, UserName, Token, _Ttl) when
      UserName  =:= undefined orelse
      Token =:= undefined ->
    %% If either the user header or token header was absent, we'll get
    %% 'undefined'... not much point in going any further, in that
    %% case, eh?
    %% TODO: Log this situation?
    token_denied;
verify_token(EntName, UserName, Token, Ttl) ->
    case candidate_tokens(EntName, UserName, Token) of
        {ok, Candidates} when erlang:is_list(Candidates) ->
            chef_log:debug("verifying token against ~p candidates",
                            [length(Candidates)]),
            verify_against_candidates(Token, Ttl, Candidates);
        {error, _} = Error  ->
            Error
    end.

%% @doc Select the token(s) for this user that has/have the same
%% prefix as the given `Token'. As a user can have several tokens, it
%% is possible to receive more than one candidate token back from the
%% database, since there is a rare chance that two (or more) tokens
%% will share the same prefix.
%%
%% The "birthday" of the retrieved token(s) is/are returned as well,
%% for TTL comparisons.
%%
%% This bit of indirection is used, rather than simply querying the
%% database directly for the exact token, as a guard against
%% timing-based attacks on tokens. Upon receiving candidate tokens, a
%% constant-time comparison between them and the given `Token' should
%% be done to give the final verdict on whether `Token' is valid or
%% not. (See `verify_against_candidates/3' for more on this.)
%%
%% The nature of the token prefix is opaque as far as the application
%% code is concerned; it is implemented completely within the
%% database.
-spec candidate_tokens(EnterpriseName, UserName, Token) -> {ok, Candidates} |
                                                           {error, term()} when
      EnterpriseName :: binary(),
      UserName :: binary(),
      Token :: token(),
      Candidates :: [candidate()].
candidate_tokens(EnterpriseName, UserName, Token)
  when erlang:is_binary(EnterpriseName),
       erlang:is_binary(UserName),
       erlang:is_binary(Token) ->
    case deliv_db:select(deliv_user, candidate_tokens,
                         [EnterpriseName, UserName, Token]) of
        {ok, Rows} when erlang:is_list(Rows) ->
            {ok, [to_candidate(R) || R <- Rows]};
        {error, _} = Error ->
            Error
    end.

%% @doc Converts the proplist row from sqerl into a `candidate()` tuple.
%%
%% Due to a quirk (?) in the database interaction, we receive
%% timestamps as *almost* a proper `calendar:datetime()' tuple, except
%% that the "seconds" portion is a float and not an integer. (This
%% even appears to occur if we declare the relevant database columns
%% to have no microsecond precision.) As a result, we need to truncate
%% the float seconds we receive to create a proper
%% `calendar:datetime()' instance.
-spec to_candidate(list()) -> candidate().
to_candidate(Proplist) ->
    Token    = proplists:get_value(<<"token">>, Proplist),
    Birthday = proplists:get_value(<<"birthday">>, Proplist),
    {Token, chef_utils:trunc_timestamp(Birthday)}.

%% @doc A user-supplied token is compared against possibly several
%% candidate tokens from the database whose initial prefixes
%% match. Processing stops at the first candidate token that is an
%% exact match.
%%
%% If the matching token is still "alive", judged by comparing its
%% birthday to the supplied TTL, then `token_ok' is returned;
%% otherwise `token_expired'.
%%
%% If none of the candidate tokens match (or no candidate tokens were
%% found!), the user is *not* authenticated, and the atom
%% `token_denied' is returned.
-spec verify_against_candidates(UserToken, Ttl, Candidates)
                               -> token_ok |
                                  token_expired |
                                  token_denied when
      UserToken :: token(),
      Ttl :: deliv_ttl(),
      Candidates :: [candidate()].
verify_against_candidates(UserToken, Ttl,
                          [{CandidateToken, Birthday} | Candidates]) ->
    chef_log:debug("verifying token with bday: ~p", [Birthday]),
    case compare_tokens_in_constant_time(UserToken, CandidateToken) of
        true ->
            case token_still_alive(Birthday, Ttl) of
                true  -> token_ok;
                false -> token_expired
            end;
        false ->
            verify_against_candidates(UserToken,
                                      Ttl,
                                      Candidates)
    end;
verify_against_candidates(_UserToken, _Ttl, []) ->
    token_denied.

%% against timing attacks
%% This is taken from the erlpass since it is private there.

%% @doc Verifies two hashes for matching purpose, in constant time. That allows
%% a safer verification as no attacker can use the time it takes to compare hash
%% values to find an attack vector (past figuring out the complexity)
compare_tokens_in_constant_time(<<T1/binary>>, <<T2/binary>>) ->
    compare_tokens_in_constant_time(erlang:binary_to_list(T1), erlang:binary_to_list(T2));
compare_tokens_in_constant_time(T1, T2) when erlang:is_list(T1) and erlang:is_list(T2) ->
    case length(T1) == length(T2) of
        true ->
            compare_tokens_in_constant_time(T1, T2, 0);
        false ->
            false
    end;
compare_tokens_in_constant_time(_, _) ->
    false.

compare_tokens_in_constant_time([X|RestX], [Y|RestY], Result) ->
    compare_tokens_in_constant_time(RestX, RestY, (X bxor Y) bor Result);
compare_tokens_in_constant_time([], [], Result) ->
    Result == 0.


%% @doc Is a token with the given timestamp still alive according to
%% the given TTL?
%%
%% NOTE: It is assumed that `TokenBirthday' is given in UTC.
%%       Furthermore, it is also assumed that the year given is 1970
%%       or greater.  This should be a valid assumption, given our
%%       database constraints, but I'm just stating it for the record.
-spec token_still_alive(calendar:datetime(), deliv_ttl()) -> boolean().
token_still_alive(TokenBirthday, Ttl) ->
    BirthdaySeconds = calendar:datetime_to_gregorian_seconds(TokenBirthday),
    NowSeconds      = calendar:datetime_to_gregorian_seconds(
                        calendar:now_to_universal_time(os:timestamp())),
    ValidUntil      = BirthdaySeconds + Ttl,

    NowSeconds =< ValidUntil.
