-module(deliv_project_github_metadata).

-include_lib("delivery/include/deliv_types.hrl").

%% API
-export([
         client_details/1
        ]).

%% DB operations
-export([
         fetch_by_id/1,
         delete_by_id/1
        ]).

%% sqerl callbacks
-export([
         '#insert_fields'/0,
         '#update_fields'/0,
         '#statements'/0,
         '#table_name'/0
        ]).

-compile({parse_transform, sqerl_gobot}).

-record(?MODULE, {project_id,
                  repo_owner,
                  repo_name,
                  token}).

'#insert_fields'() -> [project_id, repo_owner, repo_name].

'#update_fields'() -> [repo_owner, repo_name].

'#statements'() ->
    [default,
     {fetch_by_id, sqerl_rec:gen_fetch(?MODULE, project_id)},
     {delete_by_id, sqerl_rec:gen_delete(?MODULE, [project_id])}].

'#table_name'() -> "project_github_metadata".

%% @doc Fetch project by id
-spec fetch_by_id(binary()) -> {ok, d_project_github_metadata()} | {error, not_found} | {error, term()}.
fetch_by_id(ProjectId) ->
    deliv_db:fetch_by_id(?MODULE, ProjectId).

%% @doc Delete the project record by project ID
-spec delete_by_id(non_neg_integer()) -> ok.
delete_by_id(ProjectId) ->
    case sqerl_rec:cquery(?MODULE, delete_by_id, [ProjectId]) of
        {ok, 1} ->
            {error, not_found};
        {ok, 0} ->
            ok;
        {error, Why} = Error ->
            chef_log:failed_call(?MODULE, delete, [ProjectId], Why),
            Error
    end.

%% @doc
%%
%% NOTE: It seems that there is quite a bit of overlap between this
%% and the notion of a `github_client' (see record and attendant
%% functions in the `deliv_github_client' module). Consider merging the
%% two concepts.
%%
%% NOTE: I have mixed feelings about using the database ID of the
%% project here; not strong enough to propose anything else at the
%% moment, just stating it.
-spec client_details(non_neg_integer()) -> {ok, binary(), binary()} | {error, term()}.
client_details(ProjectId) ->
    case deliv_db:fetch2(?MODULE, project_id, ProjectId) of
        {ok, [#deliv_project_github_metadata{repo_owner=Owner, repo_name=Name}]} ->
            {ok, Owner, Name};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            chef_log:failed_call(?MODULE, client_details, [ProjectId], Reason),
            {error, not_found}
    end.
