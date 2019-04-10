-module(deliv_stage_data_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../../src/deliv_stage.hrl").

-compile(export_all).

stages_should_return_stages_data_macro_test() ->
    ?assertEqual(?STAGES_DATA, deliv_stage_data:stages()).
