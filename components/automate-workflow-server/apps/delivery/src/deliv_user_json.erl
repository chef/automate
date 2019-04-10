-module(deliv_user_json).

-export([
         validate/1,
         validate_update/2,
         translate_input/1,
         translate_json_key/1
        ]).

-include("deliv_types.hrl").

%% @doc Validates an EJSON blob for a user in accordance to the EJSON's
%% user_type value.
-spec validate(json()) -> json() | {error, term()}.
validate(Json) ->
    return_json_if_ok(chef_json:validate(find_user_type(Json), Json), Json).

%% @doc Validates an EJSON blob for a user in accordance to the EJSON's
%% user_type value.
%% Note that to allow a change of user-type, we need to validate with the
%% target user-type's schema.
-spec validate_update(d_user(), json()) -> json() | {error, term()}.
validate_update(User, Json) ->
    case update_spec(User, Json) of
        {ok, Schema} ->
            return_json_if_ok(chef_json:validate(Schema, Json), Json);
        Error -> Error
    end.

find_user_type(Json) ->
    translate_type(ej:get({"user_type"}, Json)).

%% value for 'user_type' ~> JSON schema name in priv/schemas/<name>.json
translate_type(<<"internal">>) ->
    intern_user;
translate_type(<<"external">>) ->
    extern_user;
translate_type(<<"saml">>) ->
    extern_user;
translate_type(<<"a2">>) ->
    extern_user;
%% If there's no user_type defined, return the most restrictive schema.
%% This includes a required "user_type" property, so validate/1 returns
%% an invalid json error.
translate_type(undefined) ->
    intern_user.

%% @doc Finds the proper spec for the user_type and the JSON it is going to be
%% updated with: if the JSON contains a user-type, this may change selection of
%% the schema used for validation.
%% If the JSON does not contain a user_type, the (existing) User record
%% determines the used schema.
%% Note that some user_type changes are forbidden: this will then return {error,
%% invalid_type_change}.
-spec update_spec(d_user(), json()) -> json() | {error, invalid_type_change}.
update_spec(User, Json) ->
    OldType = deliv_user:getval(user_type, User), % always exists
    NewType = ej:get({"user_type"}, Json), % may exist, valid_type_change/2 handles that
    case valid_type_change(OldType, NewType) of
        error -> {error, invalid_type_change};
        ValidationType ->
            Schema = chef_json:read(translate_type(ValidationType)),
            {ok, Schema}
    end.

%% @doc For a pair of user_types, returns the user type to be used for
%% validating the update, or error if it's not an allowed type change.
-spec valid_type_change(binary(), binary()) -> binary() | error.
valid_type_change(Type, Type) -> Type;
valid_type_change(_Old, <<"internal">>) -> error;
valid_type_change(<<"internal">>, New) when New =:= <<"external">>;
                                            New =:= <<"saml">> ->
    New;
valid_type_change(<<"saml">>, <<"external">>) -> <<"external">>;
valid_type_change(<<"external">>, <<"saml">>) -> <<"saml">>.

return_json_if_ok(ok, Json) ->
    Json;
return_json_if_ok(Error, _Json) ->
    Error.

%% @doc Translates the JSON (binary) property names into their (atom) equivalents
%% to store in the DB
-spec translate_json_key(binary()) -> {key, atom()} | ignore.
translate_json_key(<<"first">>) -> {key, first_name};
translate_json_key(<<"last">>) -> {key, last_name};
translate_json_key(<<"user_type">>) -> {key, user_type};
translate_json_key(Other) -> deliv_web_utils:translate_json_key(Other).

%% @doc Should return a list of the externally mapped fields for that user (by
%% its type)
-spec externally_mapped_fields(d_user()) -> [binary()].
externally_mapped_fields(User) ->
    case deliv_user:getval(user_type, User) of
        <<"internal">> -> [];
        _ -> [<<"first">>, <<"last">>, <<"email">>]
    end.

%% @doc For external/saml users, updates to fields filled in automatically (from
%% either LDAP search or SAML assertions) are forbidden. Returns a fun that can
%% be passed to `deliv_web_utils:translate_proplist' to validate the input.
-spec translate_input(d_user()) -> fun((binary()) -> {key, atom()} | {error, binary()} | ignore).
translate_input(User) ->
    case deliv_user:getval(user_type, User) of
        %% internal user, nothing fancy
        <<"internal">> ->
            fun deliv_user_json:translate_json_key/1;
        %% external or saml
        _ ->
            ExternallyMappedFields = externally_mapped_fields(User),
            fun(KeyName, Value) ->
                Translated = translate_json_key(KeyName),
                case lists:member(KeyName, ExternallyMappedFields) of
                    true ->
                        {key, TranslatedKey} = Translated,
                        %% then we have to check we're not changing the value
                        case Value =:= chef_json:undef_to_null(deliv_user:getval(TranslatedKey, User)) of
                            true -> Translated;
                            false -> {error,
                                <<"The field '", KeyName/binary, "' is an externally mapped field and cannot be modified">>}
                        end;
                    %% not an externally mapped field, all good
                    false ->
                        Translated
                end
            end
    end.
