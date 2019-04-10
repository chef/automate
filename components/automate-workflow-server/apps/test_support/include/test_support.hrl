-define(assertStringContains(String, ExpectedSubString),
        ((fun () ->
                  case string:str(deliv_utils:to_str(String),
                                  deliv_utils:to_str(ExpectedSubString)) of
                      0 -> erlang:error({assertStringContains_failed,
                                         [{module, ?MODULE},
                                          {line, ?LINE},
                                          {full_string, (??String)},
                                          {substring, (??ExpectedSubString)}]});
                      __Pos -> ok
                  end
          end)())).

-define(assertStringNotContains(String, ExpectedSubString),
        ((fun () ->
                  case string:str(deliv_utils:to_str(String),
                                  deliv_utils:to_str(ExpectedSubString)) of
                      0 -> ok;
                      __Pos -> erlang:error({assertStringNotContains_failed,
                                             [{module, ?MODULE},
                                              {line, ?LINE},
                                              {full_string, (??String)},
                                              {substring, (??ExpectedSubString)}]})
                  end
          end)())).

-define(assertFunctionExported(Fun),
        ((fun () ->
                  {module, FunMod} = erlang:fun_info(Fun, module),
                  {name, FunName} = erlang:fun_info(Fun, name),
                  {arity, FunArity} = erlang:fun_info(Fun, arity),
                  Exports = FunMod:module_info(exports),

                  case lists:member({FunName, FunArity}, Exports) of
                      true -> ok;
                      _ -> erlang:error({assertFunctionExported_failed,
                                         [{module, ?MODULE},
                                          {line, ?LINE},
                                          {missing_export, {FunMod, FunName, FunArity}}
                                         ]})
                  end
        end)())).
