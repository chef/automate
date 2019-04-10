%% Copyright 2014 Opscode, Inc. All Rights Reserved.

%% @doc This module provides functions to read-in hal mappings, look-up a hal
%% mapping and filter a list of hal links based off a users authz permissions.
-module(deliv_hal).

-include("deliv_types.hrl").

-export([
         add_authorized_hal/4,
         hal_map/0,
         hal_mapping_for/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(HAL_MAP_FILENAME, "hal_map").

%% @doc This function reads in the hal map from the file at priv/hal_map. It is
%% expected that the file contain one or more tuples of the following form. All
%% elements are atoms.
%%
%% {module_handling_request, [
%%   {http_method_of_current_request, [
%%     {hal_tag, http_method_to_handle_hal, module_to_handle_hal},
%%     {hal_tag, http_method_to_handle_hal, module_to_handle_hal}
%%   ]}
%% ]}.
%%
%% {deliv_hand_intern_users, [
%%   {post, [
%%     {full, get, deliv_hand_users_named},
%%     {reset_password, post, deliv_hand_intern_users_change_passwd}
%%   ]}
%% ]}.
-spec hal_map() -> deliv_hal_map().
hal_map() ->
    Priv = chef_utils:priv_dir_for_mod(?MODULE),
    MapFile = filename:join(Priv, ?HAL_MAP_FILENAME),
    {ok, Map} = file:consult(MapFile),
    convert_map(Map).

%% doc This function is used to look up the hal mapping for a given module.
-spec hal_mapping_for(module(), deliv_hal_map()) -> deliv_hal_method_mapping().
hal_mapping_for(Module, Map) ->
    case lists:keyfind(Module, 1, Map) of
        {Module, MethodMappings} ->
            %% Otherwise, have fun and knock yourself out
            MethodMappings;
        false ->
            %% If you're looking for a mapping, but didn't
            %% define any, we're gonna crash, yo.
            erlang:error({undefined_mapping, Module})
    end.

%% @doc This function is used to filter hal links based on authz permissions
%% and then add any that the user has permission to access to the Resp.
-spec add_authorized_hal(json(), [deliv_hal_link()],
                         cowboy_req(), req_handler()) -> json().
add_authorized_hal(Resp, HalLinks, Req, State)
  when erlang:is_list(HalLinks) ->
    AuthdHalLinks = lists:map(fun({HalTag, HalLink, _}) ->
                                      {chef_utils:to_bin(HalTag),HalLink};
                                 ({HalTag, HalLink, _, ignore_authz}) ->
                                      {chef_utils:to_bin(HalTag),HalLink}
                              end,
                              authd_hal_links(HalLinks, Req, State)),

    case AuthdHalLinks of
        [] -> Resp;
        _ ->
            HAL = deliv_web_utils:make_hal(AuthdHalLinks),
            ej:set({"_links"}, Resp, HAL)
    end.

authd_hal_links(HalLinks, Req, State) ->
    {HttpMethod, Req} = cowboy_req:method(Req),
    lists:filtermap(
      fun({_, _, _, ignore_authz}=Link) ->
              {true, Link};
         (Link) ->
              case handle_hal_auth(HttpMethod, Link, Req, State) of
                  allow ->
                      {true, Link};
                  _ ->
                      false
              end
      end,
      HalLinks).

handle_hal_auth(HttpMethod, Link, Req,
                #handler{hal_authz = AuthzMap, hal_map = HalMappings}=State) ->
    HalTag = hal_tag_from_link(Link),
    {HalMethod, HalModule} = method_mapping_for(HttpMethod,
                                                chef_utils:to_bin(HalTag),
                                                HalMappings),
    HalAuthz = deliv_authz:authz_rules_for(HalModule,
                                           AuthzMap),
    Bindings = case Link of {_,_,HalBindings} -> HalBindings; _ -> [] end,
    deliv_authz:authorize(HalMethod, HalAuthz, Req, State, Bindings).


method_mapping_for(HttpMethod, HalTag, HalMappings) ->
    case lists:keyfind(HttpMethod, 1, HalMappings) of
        {HttpMethod, HalTagMappings} ->
            method_mapping_for(HalTag, HalTagMappings);
        false ->
            %% If you're looking for a mapping, but didn't
            %% define any, we're gonna crash, yo.
            erlang:error({undefined_mapping, HttpMethod})
    end.

method_mapping_for(HalTag, HalTagMappings) ->
    case lists:keyfind(HalTag, 1, HalTagMappings) of
        {HalTag, HalMethod, HalModule} ->
            {HalMethod, HalModule};
        false ->
            %% If you're looking for a mapping, but didn't
            %% define any, we're gonna crash, yo.
            erlang:error({undefined_map, {HalTag, HalTagMappings}})
    end.

hal_tag_from_link({HalTag, _HalLink, _Bindings}) ->
    HalTag;
hal_tag_from_link({HalTag, _HalLink, _Bindings, ignore_authz}) ->
    HalTag.

convert_map(Map) ->
    [convert_mapping(Mapping) || Mapping <- Map].

convert_mapping({Module, MethodMappings}) when erlang:is_list(MethodMappings) ->
    {Module, [convert_method_mapping(Mapping) || Mapping <- MethodMappings]};
convert_mapping(Mapping) ->
    erlang:error({hal_map_parse_error, Mapping}).

convert_method_mapping({HttpMethod, HalMappings})
  when is_atom(HttpMethod),erlang:is_list(HalMappings) ->
    {deliv_web_utils:bin_method(HttpMethod),
     [convert_hal_mapping(HalMapping) || HalMapping <- HalMappings]};
convert_method_mapping(MethodMappings) ->
    erlang:error({hal_map_parse_error, MethodMappings}).

convert_hal_mapping({HalTag, HttpMethod, Module})
  when is_atom(HalTag), is_atom(HttpMethod), is_atom(Module) ->
    {chef_utils:to_bin(HalTag),
     deliv_web_utils:bin_method(HttpMethod),
     Module};
convert_hal_mapping(HalMapping) ->
    erlang:error({hal_map_parse_error, HalMapping}).
