-module(deliv_rest).

%%
%% This was taken from cowboy's repo:
%% https://github.com/ninenines/cowboy/blob/43adacc7607898f/src/cowboy_rest.erl
%%
%% Since the file went into cowboy during the development of 2.0.0, it has been
%% adapted to work with cowboy 1.0.0's callbacks. (The only difference is the
%% first argument to init/3, which is init/2 in 2.0.0; and its return type --
%% see https://github.com/ninenines/cowboy/commit/5ce4c2bfb40ecc4b.)
%%
%% When we upgrade cowboy to 2.0.0, this can be deleted, and the handlers'
%% `-behaviour(deliv_rest)` can be changed to `-behaviour(cowboy_rest)`.
%%

%% init/3 doesn't exist in cowboy 2.0.0, and init/2 works slightly different.
-callback init(any(), Req, any())
    -> {ok | module(), Req, any()}
    | {upgrade, protocol, module(), Req, any()}
    when Req::cowboy_req:req().

%% Note: the rest of this file was copied from the source mentioned above.

-callback terminate(any(), cowboy_req:req(), any()) -> ok.
-optional_callbacks([terminate/3]).

%% REST handler callbacks.

-callback allowed_methods(Req, State)
    -> {[binary()], Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([allowed_methods/2]).

-callback allow_missing_post(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([allow_missing_post/2]).

-callback charsets_provided(Req, State)
    -> {[binary()], Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([charsets_provided/2]).

-callback content_types_accepted(Req, State)
    -> {[{binary() | {binary(), binary(), '*' | [{binary(), binary()}]}, atom()}], Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([content_types_accepted/2]).

-callback content_types_provided(Req, State)
    -> {[{binary() | {binary(), binary(), '*' | [{binary(), binary()}]}, atom()}], Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([content_types_provided/2]).

-callback delete_completed(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([delete_completed/2]).

-callback delete_resource(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([delete_resource/2]).

-callback expires(Req, State)
    -> {calendar:datetime() | binary() | undefined, Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([expires/2]).

-callback forbidden(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([forbidden/2]).

-callback generate_etag(Req, State)
    -> {binary() | {weak | strong, binary()}, Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([generate_etag/2]).

-callback is_authorized(Req, State)
    -> {true | {false, iodata()}, Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([is_authorized/2]).

-callback is_conflict(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([is_conflict/2]).

-callback known_methods(Req, State)
    -> {[binary()], Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([known_methods/2]).

-callback languages_provided(Req, State)
    -> {[binary()], Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([languages_provided/2]).

-callback last_modified(Req, State)
    -> {calendar:datetime(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([last_modified/2]).

-callback malformed_request(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([malformed_request/2]).

-callback moved_permanently(Req, State)
    -> {{true, iodata()} | false, Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([moved_permanently/2]).

-callback moved_temporarily(Req, State)
    -> {{true, iodata()} | false, Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([moved_temporarily/2]).

-callback multiple_choices(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([multiple_choices/2]).

-callback options(Req, State)
    -> {ok, Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([options/2]).

-callback previously_existed(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([previously_existed/2]).

-callback resource_exists(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([resource_exists/2]).

-callback service_available(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([service_available/2]).

-callback uri_too_long(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([uri_too_long/2]).

-callback valid_content_headers(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([valid_content_headers/2]).

-callback valid_entity_length(Req, State)
    -> {boolean(), Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([valid_entity_length/2]).

-callback variances(Req, State)
    -> {[binary()], Req, State}
    | {stop, Req, State}
    when Req::cowboy_req:req(), State::any().
-optional_callbacks([variances/2]).

%% End of REST callbacks. Whew!
