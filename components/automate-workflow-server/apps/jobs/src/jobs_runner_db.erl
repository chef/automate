-module(jobs_runner_db).

-include("jobs_types.hrl").

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-export([
         fetch_all/0,
         insert/2,
         delete/1,
         fetch_by_name/3
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(jobs_runner_db, {id                 :: db_guid(),
                         enterprise_name    :: binary(),
                         name               :: binary(), % is a hostname, but sqerl works with name
                         private_key        :: binary(),
                         os                 :: binary(),
                         platform           :: binary(),
                         platform_family    :: binary(),
                         platform_version   :: binary()
                        }).

'#insert_fields'() -> [name, private_key].
'#update_fields'() -> [name, private_key].
'#table_name'() -> "jobs_runners".
'#statements'() ->
    [default, % insert, update, delete_by_$first, fetch_by_$first ($first is id here)
     {new_runner_returning, "WITH enterprise AS (SELECT id, name FROM enterprises WHERE name = $1)
                   INSERT INTO jobs_runners(enterprise_id, name, private_key, os, platform_family, platform, platform_version)
                     (SELECT id, $2, $3, $4, $5, $6, $7 FROM enterprise)
                   RETURNING jobs_runners.id AS id, (SELECT name FROM enterprise) AS enterprise_name, jobs_runners.name AS name,
                     jobs_runners.private_key AS private_key, jobs_runners.os AS os, jobs_runners.platform_family AS platform_family,
                     jobs_runners.platform AS platform, jobs_runners.platform_version AS platform_version"},
     {fetch_all, "SELECT runners.id AS id, ent.name AS enterprise_name, runners.name AS name, runners.private_key AS private_key,
                    runners.os AS os, runners.platform_family AS platform_family, runners.platform AS platform,
                    runners.platform_version AS platform_version
                  FROM jobs_runners runners, enterprises ent
                  WHERE runners.enterprise_id = ent.id"},
     {fetch_by_name, "SELECT runners.id AS id, ent.name AS enterprise_name, runners.name AS name,
                        runners.private_key AS private_key, runners.os AS os, runners.platform_family AS platform_family,
                        runners.platform AS platform, runners.platform_version AS platform_version
                      FROM jobs_runners runners, enterprises ent
                      WHERE ent.id = runners.enterprise_id
                        AND ent.name = $1
                        AND runners.name = $2"}
    ].

-spec insert(binary(), jobs_runner() | {error, any()}) -> [jobs_runner()] | {error, any()}.
insert(EntName, #runner{hostname = Hostname,
                        private_key = PrivateKey,
                        pid = Pid,
                        os = Os,
                        platform_family = PlatformFamily,
                        platform = Platform,
                        platform_version = PlatformVersion}) ->
    case sqerl_rec:qfetch(?MODULE, new_runner_returning, [EntName, Hostname, PrivateKey,
                                                          Os, PlatformFamily, Platform, PlatformVersion]) of
        [NewRunner] -> [(deserialize_one(NewRunner))#runner{ pid = Pid }];
        AnythingElse -> AnythingElse
    end;
insert(_EntName, {error, _} = Error) -> Error.

-spec delete(jobs_runner()) -> {ok, integer()} | {error, _}.
delete(#runner{id = Id}) ->
    delete_by_id(Id).

-spec delete_by_id(binary()) -> {ok, integer()} | {error, _}.
delete_by_id(Id) ->
    sqerl_rec:cquery(?MODULE, delete_by_id, [Id]).

-spec fetch_by_name(binary(), binary(), pid()) -> [jobs_runner()] | {error, any()}.
fetch_by_name(EntName, Hostname, Pid) ->
    case sqerl_rec:qfetch(?MODULE, fetch_by_name, [EntName, Hostname]) of
        [Record] -> [(deserialize_one(Record))#runner{ pid = Pid }];
        AnythingElse -> AnythingElse
    end.

-spec fetch_all() -> [jobs_runner()] | {error, any()}.
fetch_all() ->
    deserialize(sqerl_rec:fetch_all(?MODULE)).

-spec deserialize_one(#jobs_runner_db{}) -> jobs_runner().
deserialize_one(#jobs_runner_db{id = Id,
                                enterprise_name = EntName,
                                name = Hostname,
                                private_key = PrivateKey,
                                os = Os,
                                platform_family = PlatformFamily,
                                platform = Platform,
                                platform_version = PlatformVersion}) ->
    jobs_key:hydrate(#runner{id = Id,
            enterprise_name = EntName,
            hostname = Hostname,
            private_key = PrivateKey,
            os = Os,
            platform_family = PlatformFamily,
            platform = Platform,
            platform_version = PlatformVersion}).

-spec deserialize([#jobs_runner_db{}] | {error, any()}) -> [jobs_runner()] | {error, any()}.
deserialize({error, _} = Error) ->
    Error;
deserialize(Runners) ->
    lists:map(fun deserialize_one/1, Runners).
