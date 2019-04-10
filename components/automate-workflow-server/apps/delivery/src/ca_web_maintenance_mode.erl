%%====================================================================
%%
%% @author Jon Morrow <jmorrow@chef.io>
%% @copyright CHEF Software, Inc. - 2017
%%
%% Simple module responsible for turning web maintenance mode on/off.
%%
%%====================================================================
-module(ca_web_maintenance_mode).

% interface
-export([on/0, off/0]).

%% Our nginx config has logic to branch behavior based on the
%% presence/absence of this file. When it is present, the webui is
%% disabled and displays a maintence page. It is hard-coded into the
%% nginx-server.conf.erb.
-define(WEBUI_MAINT_FILE, "/var/opt/delivery/delivery/etc/unlicensed.flag").

%% @doc Turn on web maintenance mode.
-spec on() -> ok | {error, file:posix() | badarg | terminated}.
on() ->
    chef_log:info("event=webui_maintenance_on"),
    V = io_lib:format("~p", [os:timestamp()]),
    file:write_file(?WEBUI_MAINT_FILE, V).

%% @doc Turn of web maintenance mode.
-spec off() -> ok | {error, file:posix() | badarg}.
off() ->
    chef_log:info("event=webui_maintenance_off"),
    file:delete(?WEBUI_MAINT_FILE).
