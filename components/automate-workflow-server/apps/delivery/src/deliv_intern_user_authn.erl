-module(deliv_intern_user_authn).

-include("deliv_types.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
         verify_password/3
        ]).

-spec verify_password(binary(), binary(), binary())
        -> verified | denied | {error, any()}.
verify_password(EntName, UserName, Password) ->
    verify_password_for_user(deliv_intern_user:fetch(EntName, UserName), Password).

%% @private
verify_password_for_user({ok, User}, Password) ->
    SaltAndHash = deliv_intern_user:getval(hashed_pass, User),
    case user_password:match(Password, SaltAndHash) of
        ok -> verified;
        _ -> denied
    end;
verify_password_for_user({error, not_found}, _Password) ->
    denied;
verify_password_for_user({error, _Other} = Error, _Password) ->
    Error.
