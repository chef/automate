-module(deliv_json).

-export([
         init_schemas/0
        ]).


%% @doc This loads schemas directly from our documentation. It reads the schema
%% directory and assumes all files that end with json are schema files, and
%% their name is their schema key. So intern_user.json would correspond to the
%% intern_user schema.
-spec init_schemas() -> ok.
init_schemas() ->
    chef_json:init_schemas(?MODULE).
