%% @doc Convenience API for finding data in Github Issue JSON
-module(deliv_github_issue).

-export([
    comment_author/1,
    comment_body/1,
    issue_number/1,
    url/1
]).

-include_lib("delivery/include/deliv_types.hrl").

%% @doc Github user name of comment writer
-spec comment_author(github_issue()) -> binary().
comment_author(Payload) ->
    ej:get([<<"comment">>, <<"user">>, <<"login">>], Payload).

%% @doc The text of a new comment
-spec comment_body(github_issue()) -> binary().
comment_body(Payload) ->
    ej:get([<<"comment">>, <<"body">>], Payload).

%% @doc Issue Id
-spec issue_number(github_issue()) -> binary().
issue_number(Payload) ->
    ej:get([<<"issue">>, <<"number">>], Payload).

%% @doc Issue url
-spec url(github_issue()) -> binary().
url(Payload) ->
    ej:get([<<"issue">>, <<"url">>], Payload).
