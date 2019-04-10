-include_lib("public_key/include/public_key.hrl").

%% Generic types
-type str_or_binary() :: binary() | list().
-type proplist(KeyType, ValueType) :: list({KeyType, ValueType}).
-type proplist(Type) :: proplist(Type, Type).
-type proplist() :: proplist(str_or_binary() | atom(), _).
%% `ej_invalid' record from the ej lib
-type ej_invalid() :: tuple().
%% we use the jiffy JSON format
-type json() :: ej:json_object() | [ej:json_object()].
%% the type passed to ej:valid to check if a JSON is valid
-type json_spec() :: ej:json_object().

%% ej doesn't export this
-type json_array() :: [ej:json_term()].

%% @doc GET, POST, PUT, or DELETE
-type http_method_bin() :: binary().

%% HTTP-related stuff
-define(CONTENT_TYPE_HEADER, "content-type").
-define(JSON_CONTENT_TYPE, "application/json").

%% we'll add more methods as we go
-type http_method() :: get | patch | post | put | delete.
-type http_status() :: non_neg_integer().
-type http_headers(Type) :: list({Type, Type}).
-type http_headers() :: http_headers(binary()).
-type http_body() :: binary() | json() | json_array().

%% DB-related types
-type sqerl_record() :: tuple().
%% what's being returned by calls to sqerl_rec methods
-type db_op_result(SqerlRecord) :: [SqerlRecord] | {error, conflict}
    | {error, not_found} | {error, no_enterprise} | {error, any()}.
-type db_op_single_result(SqerlRecord) :: {ok, SqerlRecord} | {error, term()}.
-type db_op_result() :: db_op_result(sqerl_record()).
-type db_id() :: non_neg_integer().
-type db_guid() :: binary().

%% not exported by public_key
-type rsa_private_key() :: #'RSAPrivateKey'{}.
-type rsa_public_key() :: #'RSAPublicKey'{}.
