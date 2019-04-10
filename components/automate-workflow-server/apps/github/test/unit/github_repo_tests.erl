-module(github_repo_tests).

-include_lib("hoax/include/hoax.hrl").
-include_lib("delivery/include/deliv_types.hrl").
-include_lib("delivery/include/deliv_coordinates.hrl").

-compile(export_all).

fixture_test_() ->
    hoax:fixture(?MODULE).

add_user_pub_key_add_posts_to_github_api() ->
    EntName = <<"EntName">>,
    Route = ["/keys"],
    Title = <<"chef-delivery builder">>,
    Key = <<"ssh-rsa ..... root@os-XXXXXXX">>,
    Body = {[{<<"title">>, Title}, {<<"key">>, Key}]},
    ResponseJson = <<"{\"id\": 1}">>,

    hoax:mock(deliv_github_client,
              ?expect(user_req,
                      ?withArgs([EntName, post, Route, Body]),
                      ?andReturn({ok, 201, [], ResponseJson}))),

    ?assertEqual({ok, 1}, github_repo:add_user_pub_key(EntName, Title, Key)),
    ?verifyAll.

webhook_create_posts_to_github_api() ->
    app_test_helpers:setup_app_env(),
    Coords = #proj_coordinates{ent_name = <<"EntName">>,
                               org_name = <<"OrgName">>,
                               proj_name = <<"ProjName">>},

    %% HTTP Request Config
    Route = ["/hooks"],
    Body = {[{<<"name">>, <<"web">>},
             {<<"config">>, {[{<<"url">>, <<"http://127.0.0.1/api/v0/e/EntName/orgs/OrgName/projects/ProjName/github-webhook">>},
                              {<<"content_type">>, <<"json">>},
                              {<<"insecure_ssl">>, <<"0">>}]}},
             {<<"events">>, [<<"pull_request">>, <<"issue_comment">>]},
             {<<"active">>, true}]},
    WebhookId = 1,
    ResponseJson = <<"{\"id\": 1}">>,

    hoax:mock(deliv_github_client,
              ?expect(req,
                      ?withArgs([Coords, post, Route, Body, []]),
                      ?andReturn({ok, 201, [], ResponseJson}))),

    ?assertEqual({ok, WebhookId}, github_repo:create_webhook(Coords, verify_ssl)),

    ?verifyAll.

add_builder_pub_key_returns_erro_if_no_user() ->
    EntName = <<"EntName">>,

    hoax:mock(deliv_intern_user,
              ?expect(fetch,
                      ?withArgs([EntName, <<"builder">>]),
                      ?andReturn({error, not_found}))),

    ?assertEqual({error, not_found}, github_repo:add_builder_pub_key(EntName)),
    ?verifyAll.

add_builder_pub_key_returns_error_when_it_fails_to_add_key() ->
    EntName = <<"EntName">>,
    Route = ["/keys"],
    Title = <<"Delivery Builder (EntName)">>,
    Key = <<"ssh-rsa ..... root@os-XXXXXXX">>,
    Body = {[{<<"title">>, Title}, {<<"key">>, Key}]},
    User = deliv_intern_user:setvals([{ssh_pub_key, Key}], deliv_intern_user:'#new'()),

    hoax:mock(deliv_intern_user, [
                          ?expect(fetch,
                                  ?withArgs([EntName, <<"builder">>]),
                                  ?andReturn({ok, User})),
                          ?expect(getval,
                                  ?withArgs([ssh_pub_key, User]),
                                  ?andReturn(Key))
                        ]),
    hoax:mock(deliv_github_client,
              ?expect(user_req,
                      ?withArgs([EntName, post, Route, Body]),
                      ?andReturn({ok, 204, [], <<>>}))),

    ?assertEqual({error, no_content}, github_repo:add_builder_pub_key(EntName)),
    ?verifyAll.

add_builder_pub_key_adds_key_to_github_repo() ->
    EntName = <<"EntName">>,
    Route = ["/keys"],
    Title = <<"Delivery Builder (EntName)">>,
    Key = <<"ssh-rsa ..... root@os-XXXXXXX">>,
    Body = {[{<<"title">>, Title}, {<<"key">>, Key}]},
    ResponseJson = <<"{\"id\": 1}">>,
    User = deliv_intern_user:setvals([{ssh_pub_key, Key}], deliv_intern_user:'#new'()),
    KeyId = 1,

    hoax:mock(deliv_intern_user, [
                          ?expect(fetch,
                                  ?withArgs([EntName, <<"builder">>]),
                                  ?andReturn({ok, User})),
                          ?expect(getval,
                                  ?withArgs([ssh_pub_key, User]),
                                  ?andReturn(Key))
                        ]),
    hoax:mock(deliv_github_client,
              ?expect(user_req,
                      ?withArgs([EntName, post, Route, Body]),
                      ?andReturn({ok, 201, [], ResponseJson}))),

    ?assertEqual({ok, KeyId}, github_repo:add_builder_pub_key(EntName)),
    ?verifyAll.
