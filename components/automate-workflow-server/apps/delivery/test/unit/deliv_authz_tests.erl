-module(deliv_authz_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

deliv_authz_test_() ->
    {
      with,
      deliv_authz:authz_rules(),
      [fun rules_have_proper_structure/1]
    }.

forbidden_test_() ->
    [
        eunit_sugar:fixture(?MODULE, "forbidden_none_")
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

missing_rules_file_test_() ->
    {setup,
     fun() ->
             meck:new(file, [unstick, passthrough]),
             meck:expect(file, consult,
                         fun(_) -> {error, enoent} end)
     end,
     fun(_) ->
             ct_meck:unload(file)
     end,
     [
      fun() ->
              ?assertError({authz_rules_file, enoent, _},
                           deliv_authz:authz_rules())
      end
     ]}.

rules_have_proper_structure(Rules) ->
    [module_authz_is_valid(Rule) || Rule <- Rules],
    ?assert(erlang:is_list(Rules)).

module_authz_is_valid({Module,ScopeRule}) when is_tuple(ScopeRule) ->
    ?assert(erlang:is_atom(Module)), %% TODO: ensure it's an actual handler module,
    scope_is_valid(ScopeRule);
module_authz_is_valid({Module, ScopeRules}) when erlang:is_list(ScopeRules) ->
    ?assert(erlang:is_atom(Module)),
    [scope_is_valid(Rule) || Rule <- ScopeRules].

scope_is_valid({Scope, Rules}) ->
    ?assert(lists:member(Scope, [enterprise, organization, project, pipeline])),
    [handler_authz_is_valid(Rule) || Rule <- Rules].

handler_authz_is_valid({Method, Roles}) ->
    ?assert(lists:member(Method, [<<"GET">>,<<"PUT">>,<<"POST">>,<<"DELETE">>])),
    %% if `Roles' is not the atom `all', then it must be a list
    %% containing a list of valid roles
    ?assert(Roles =:= all orelse erlang:is_list(Roles) andalso erlang:length(Roles) =/= 0),
    ?assertNot(Roles =/= all andalso sets:is_disjoint(
        sets:from_list(Roles),
        sets:from_list([<<"admin">>,
                        <<"committer">>,
                        <<"reviewer">>,
                        <<"shipper">>,
                        <<"observer">>]))).

forbidden_none_returns_false() ->
    Actual = deliv_authz:forbidden_none(req, state),
    ?assertMatch({false, req, state}, Actual).
