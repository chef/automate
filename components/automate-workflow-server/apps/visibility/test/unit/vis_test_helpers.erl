-module(vis_test_helpers).

-export([
         open_json_fixture/2
        ]).

-include_lib("hoax/include/hoax.hrl").


open_json_fixture(FixtureFolderName, FixtureFile) ->
    FixturesDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "test", "fixtures", FixtureFolderName]),
    {ok, JsonBinary} = file:read_file(filename:join([FixturesDir, FixtureFile])),
    chef_json:decode(JsonBinary).
