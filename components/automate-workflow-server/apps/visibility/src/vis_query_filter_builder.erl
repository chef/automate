%% Appends additional filters to elasticsearch query json based on
%% input Vis API filter json.
-module(vis_query_filter_builder).

-export([
         append_to_existing_bool_filters/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("chef_common/include/chef_common.hrl").

%% Whether or not we need to append .raw to elasticsearch query terms
%% from our API terms.
-define(RAW_KEYS,
        #{"status" => not_raw,
          "environment" => raw,
          "roles" => raw,
          "cookbooks" => raw,
          "recipes" => raw,
          "attributes" => raw,
          "resource_names" => raw,
          "source_fqdn" => raw,
          "organization_name" => raw,
          "name" => raw
          }
       ).

%% Parse request json and append to elasticsearch json.
%%
%% RequestJson expected format:
%%
%% {
%%   ...
%%   "filter" : {
%%     "must" : [
%%       { "cookbooks" : "cookbook_name" }
%%       ...
%%     ],
%%     "should" :  [
%%       { "roles" : "cookbook_name" }
%%       ...
%%     ]
%%   }
%%   ...
%% }
%%
%% ESJson expected format:
%% {
%%   ...
%%   "query": {
%%     "bool": { ... }
%%   ...
%% }
-spec append_to_existing_bool_filters(json(), json()) -> json().
append_to_existing_bool_filters(RequestJson, ESJson) ->
    case ej:get({<<"filter">>}, RequestJson) of
        undefined ->
            ESJson;
        FilterJson ->
            ESJsonAppendedMust =
                create_filter_json_from_request_json_if_filter_present(FilterJson,
                                                                       ESJson,
                                                                       <<"must">>,
                                                                       <<"filter">>),
            ESJsonAppendedMustAndShould =
                create_filter_json_from_request_json_if_filter_present(FilterJson,
                                                                       ESJsonAppendedMust,
                                                                       <<"should">>,
                                                                       <<"should">>),
            ESJsonFinal =
                add_minimum_should_match_if_should_filter(ej:get({<<"should">>}, FilterJson),
                                                          ESJsonAppendedMustAndShould),

            ESJsonFinal
    end.

add_minimum_should_match_if_should_filter(undefined, AccJson) ->
    AccJson;
add_minimum_should_match_if_should_filter([], AccJson) ->
    AccJson;
add_minimum_should_match_if_should_filter(_ContainedShouldFilter, AccJson) ->
    ej:set({<<"query">>, <<"bool">>, <<"minimum_should_match">>},
           AccJson,
           1).

-spec create_filter_json_from_request_json_if_filter_present(
        json(), json(), undefined | binary(), binary()
       ) -> json().
create_filter_json_from_request_json_if_filter_present(FilterJson, AccJson, InputKey, OutputKey) ->
    create_filter_json_from_request_json(ej:get({InputKey}, FilterJson), AccJson, OutputKey).

-spec create_filter_json_from_request_json(undefined | json_array(), json(), binary()) ->
                                             json().
create_filter_json_from_request_json(undefined, AccJson, _OutputKey) ->
    AccJson;
create_filter_json_from_request_json(FilterArray, AccJson, OutputKey) ->
    ESTermArray = lists:foldl(fun(JsonElem, Acc) ->
                                         Acc ++ [
                                                 {[{
                                                     term_or_wildcard(JsonElem),
                                                     add_raw_to_key(JsonElem)
                                                   }]}
                                                ]
                                 end,
                                 [], FilterArray),
    ESTermArrayPlusExistingTermArray =
        get_existing_filter_array(AccJson, OutputKey) ++  ESTermArray,
    Result = ej:set({<<"query">>, <<"bool">>, OutputKey},
           AccJson,
           ESTermArrayPlusExistingTermArray),
    Result.

-spec get_existing_filter_array(json(), binary()) -> json_array().
get_existing_filter_array(AccJson, Filter) ->
    case ej:get({<<"query">>, <<"bool">>, Filter}, AccJson) of
        undefined ->
            [];
        ExistingArray ->
            ExistingArray
    end.

-spec add_raw_to_key(json()) -> json().
add_raw_to_key(JsonElem) ->
    {[{Key, Value}]} = JsonElem,
    NewKey = case maps:get(binary_to_list(Key), ?RAW_KEYS, not_raw) of
                 raw -> <<Key/binary, ".raw">>;
                 not_raw -> Key
             end,
    {[{NewKey, Value}]}.

-spec term_or_wildcard(json()) -> binary().
term_or_wildcard(JsonElem) ->
    {[{_Key, Value}]} = JsonElem,
    case string:chr(binary_to_list(Value), $*) of
        0 ->
            <<"term">>;
        _WildcardExists ->
            <<"wildcard">>
    end.
