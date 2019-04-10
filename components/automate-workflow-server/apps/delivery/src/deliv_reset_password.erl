-module(deliv_reset_password).

-include("deliv_types.hrl").

-export([
         generate_reset_token/2,
         consume_reset_token/4,
         reset_url/3
        ]).

-define(TOKEN_NUM_RAND_BYTES, 32).

-spec generate_reset_token(binary(), binary()) -> {ok, Token} | {error, atom()}
                                               when Token :: binary().
generate_reset_token(EntName, UserName) ->
    Token = deliv_token:new(),
    %% Format is {{Hour, Minute, Second.Microsecond}, Days, Months}
    Expiry = {{2, 0, 0}, 0, 0}, % so this is two hours
    case sqerl_rec:scalar_fetch(deliv_intern_user, assign_password_reset_token,
                                [EntName, UserName, Token, Expiry]) of
        {error, _} = Error -> Error;
        [null] -> {error, token_not_added};
        [] -> {error, token_not_added};
        _ -> {ok, Token}
    end.

-spec consume_reset_token(binary(), binary(), binary(), binary()) ->
    ok | {error, atom()}.
consume_reset_token(EntName, UserName, Token, NewPassword) ->
    case sqerl_rec:scalar_fetch(deliv_intern_user, use_password_reset_token,
                                [EntName, UserName, Token]) of
        [null] -> {error, token_not_found}; % happens with postgres 9.6
        [] -> {error, token_not_found};     % postgres 9.4, 9.5
        {error, _} = Error -> Error;
        _DeletedTokens -> deliv_intern_user:reset_password(EntName, UserName, NewPassword)
    end.

%% Convenience function for usage from enterprise_ctl
-spec reset_url(binary(), binary(), binary()) -> binary().
reset_url(EntName, UserName, Token) ->
    erlang:iolist_to_binary([deliv_web_utils:protocol(),
                             "://",
                             deliv_web_utils:hostname(),
                             "/e/", EntName,
                             "/#/reset-password/",
                             UserName, "/", Token]).
