%% @doc Provide an interface for PostgreSQL error codes, including our
%% own custom codes.
%%
%% PostgreSQL allows us to define our own custom errors, which is
%% kind of awesome. We can use these in our stored procedures to capture
%% information about specific meaningful error conditions and convey that
%% to application code.
%%
%% Note: All error codes are 5-character strings, and our codes begin
%% with the string "CD" for "CHEF Delivery".  This provides some basic
%% namespacing; 1000 error codes should be more than enough.
%%
%% See
%% http://www.postgresql.org/docs/9.3/static/plpgsql-errors-and-messages.html
%% for more.
-module(deliv_pg).

-export([
         translate_error/1
        ]).

%% @doc Translate PostgreSQL error codes into Erlang terms.
translate_error({Code, _}) ->
    translate_error(Code);
translate_error(<<"CD001">>) -> membership_prerequisite_not_met;
translate_error(<<"CD002">>) -> user_type_cannot_be_changed;
translate_error(<<"CD003">>) -> {not_found, enterprise};
translate_error(<<"CD004">>) -> {not_found, user};
translate_error(<<"CD005">>) -> {not_found, organization};
translate_error(<<"CD006">>) -> {not_found, project};
translate_error(<<"CD007">>) -> {not_found, pipeline};
translate_error(<<"CD008">>) -> new_patchset_identical_to_old_one;
translate_error(<<"CD009">>) -> negative_pagination_limit;
translate_error(<<"CD010">>) -> invalid_change_id_for_paging;
translate_error(<<"CD011">>) -> invalid_state_for_paging;
translate_error(<<"CD012">>) -> forbidden_update_on_draft_comment;
translate_error(<<"CD013">>) -> forbidden_update_on_published_comment;
translate_error(<<"CD014">>) -> unknown_comment_comment_parent_id;
translate_error(<<"CD015">>) -> unrelated_change_and_patchset;
translate_error(<<"CD016">>) -> unknown_comment;
translate_error(<<"CD017">>) -> unknown_sequence_number_for_change;
translate_error(<<"CD018">>) -> unknown_change;
translate_error(<<"CD019">>) -> github_project_already_linked;
translate_error(Error)       -> {system_error, Error}.
