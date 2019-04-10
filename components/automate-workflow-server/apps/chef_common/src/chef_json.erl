-module(chef_json).

-export([
         encode/1,
         decode/1,
         init_schemas/1,
         init_schema/2,
         validate/2,
         read/1,
         rigid_object_spec/1,
         loose_object_spec/1,
         simple_string_dict_spec/1,
         simple_string_loose_spec/1,
         record_to_json/1,
         undef_to_null/1
        ]).

-include("chef_common.hrl").

-spec encode(json()) -> binary().
encode(Json) ->
    erlang:iolist_to_binary(jiffy:encode(undef_to_null(timestamp_to_bin(Json)))).

%% @doc jiffy can't encode the timestamp fields returned by sqerl, so convert
%% them to binary strings beforehand.
timestamp_to_bin(List) when erlang:is_list(List) ->
    [timestamp_to_bin(Item) || Item <- List];
timestamp_to_bin({PropList}) when erlang:is_list(PropList) ->
    {[{Key, timestamp_to_bin(Value)} || {Key, Value} <- PropList]};
timestamp_to_bin({{_,_,_},{_,_,_}} = Value) -> chef_utils:format_db_timestamp(Value);
timestamp_to_bin(Other) -> Other.

%% @doc Missing fields in sqerl records have the value `undefined'
%% which we need to translate to `null' before we can send them to
%% jiffy to be encoded
%% If that turns out to be a performance issue, it's rather trivial to
%% fork jiffy to do that for us, or even better to try and get a
%% patch merged upstream to configure jiffy with a list of atoms
%% that should be treated as (and translated to) `null'
undef_to_null(List) when erlang:is_list(List) ->
    [undef_to_null(Item) || Item <- List];
undef_to_null({PropList}) when erlang:is_list(PropList) ->
    {[{Key, undef_to_null(Value)} || {Key, Value} <- PropList]};
undef_to_null(undefined) -> null;
undef_to_null(Other) -> Other.

-spec decode(binary()) -> json() | {error, invalid_json}.
decode(String) ->
    try
        jiffy:decode(String)
    catch
        _:_ ->
            {error, invalid_json}
    end.


%% @doc Loads jesse schemas from ../priv/schemas relative to where Module
%% lives (which should be src/ for your application) or you can pass an explicit
%% path in.
-spec init_schemas(atom() | list()) -> ok.
init_schemas(Module) when is_atom(Module) ->
    SchemasDir = filename:join([filename:dirname(code:which(Module)), "..", "priv/schemas/"]),
    init_schemas(SchemasDir);
init_schemas(SchemasDir)  ->
    Schemas = filelib:fold_files(SchemasDir
                                 , "\\.json$", false,
                                 fun(Path, A) ->
                                         SchemaName = filename:basename(Path, ".json"),
                                         [{erlang:list_to_atom(SchemaName),
                                           Path} | A]
                                 end, []),
    [ init_schema(X,Y) || {X,Y} <- Schemas ],
    ok.

-spec init_schema(str_or_binary(), str_or_binary()) -> ok.
init_schema(SchemaName, SchemaFile) ->
    {ok, SchemaBin} = file:read_file(SchemaFile),
    Schema = decode(SchemaBin),
    ok = jesse:add_schema(chef_utils:to_str(SchemaName), Schema).

%% @doc Validate that provided json matches schema
%% `Schema' must be either an atom, in which case it's the name of a pre-loaded schema
%% or else it can be a JSON itself
-spec validate(atom() | json(), json()) -> ok | {error, term()}.
validate(SchemaName, Json) when is_atom(SchemaName) ->
    handle_validate_result(jesse:validate(chef_utils:to_str(SchemaName), Json));
validate(Schema, Json) ->
    handle_validate_result(jesse:validate_with_schema(Schema, Json)).

%% @doc Read a schema from the Jesse Database
%% Wraps the provided name to ensure we are reading it using the right term
-spec read(atom()) -> jesse:json_term() | no_return().
read(SchemaName) ->
    jesse_database:load(chef_utils:to_str(SchemaName)).

handle_validate_result({ok, _}) -> ok;
handle_validate_result({error, _} = Error) -> Error.

%% @doc Returns a simple jesse spec that matches objects with said required fields
%% and with said types
-spec rigid_object_spec(proplist(Key, Type | [Type])) -> json() when
    Key :: atom() | str_or_binary(),
    Type :: atom() | str_or_binary().
rigid_object_spec(RequiredFields) ->
    object_spec(RequiredFields, false).

%% @doc Returns a simple jesse spec that allows for additional properties to be
%% defined beyond those marked as required.
-spec loose_object_spec(proplist(Key, Type | [Type])) -> json() when
    Key :: atom() | str_or_binary(),
    Type :: atom() | str_or_binary().
loose_object_spec(RequiredFields) ->
    object_spec(RequiredFields, true).

%% @private
object_spec(RequiredFields, AdditionalProperties) ->
    {[
        {<<"type">>, <<"object">>},
        {<<"additionalProperties">>, AdditionalProperties},
        {<<"properties">>, {
            lists:map(fun({FieldName, FieldType}) ->
                Type = case chef_utils:is_genuine_list(FieldType) of
                    true -> [to_type(T) || T <- FieldType];
                    false -> to_type(FieldType)
                end,
                {chef_utils:to_bin(FieldName),
                    {[ {<<"type">>, Type}, {<<"required">>, true} ]} }
            end,
            RequiredFields)
        }}
    ]}.

to_type(null) -> null;
to_type(Other) -> chef_utils:to_bin(Other).

%% @doc Generates a rigid spec for a flat object with only string fields
-spec simple_string_dict_spec([atom() | str_or_binary()]) -> json().
simple_string_dict_spec(RequiredFields) ->
    rigid_object_spec(lists:map(fun(Field) -> {Field, <<"string">>} end,
                                RequiredFields)).

%% @doc Generates a loose spec for a flat object with only string fields
-spec simple_string_loose_spec([atom() | str_or_binary()]) -> json().
simple_string_loose_spec(RequiredFields) ->
    loose_object_spec(lists:map(fun(Field) -> {Field, <<"string">>} end,
                                RequiredFields)).

%% @doc Turns a sqerl record into a JSON object in a generic way
-spec record_to_json(sqerl_record()) -> json().
record_to_json(Rec) ->
    RecModule = erlang:element(1, Rec),
    Fields = RecModule:fields(),
    {[{chef_utils:to_bin(F), RecModule:getval(F, Rec)} || F <- Fields]}.
