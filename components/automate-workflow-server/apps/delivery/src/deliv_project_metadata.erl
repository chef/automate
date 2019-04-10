%% @doc This module implements the project metadata behaviour. This ensures
%% each of the metadata modules have fetch_by_id and inject_json.
-module(deliv_project_metadata).

-include("deliv_types.hrl").

%%@doc Fetches metadata given project_id.
-callback fetch_by_id(ProjectId :: non_neg_integer()) -> {ok, tuple()} | {error, term()}.
