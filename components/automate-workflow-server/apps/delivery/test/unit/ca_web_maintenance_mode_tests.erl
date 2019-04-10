%%====================================================================
%%
%% @author Jon Morrow <jmorrow@chef.io>
%% @copyright CHEF Software, Inc. - 2017
%%
%% Test for license checker
%%
%%====================================================================
-module(ca_web_maintenance_mode_tests).

-include_lib("hoax/include/hoax.hrl").

-compile(export_all).

%% Our nginx config has logic to branch behavior based on the
%% presence/absence of this file. When it is present, the webui is
%% disabled and displays a maintence page. It is hard-coded into the
%% nginx-server.conf.erb.
-define(WEBUI_MAINT_FILE, "/var/opt/delivery/delivery/etc/unlicensed.flag").

%% Hoax fixtures
ca_license_checker_test_() ->
    [
        hoax:fixture(?MODULE, "on_"),
        hoax:fixture(?MODULE, "off_")
    ].

on_logs_and_writes_maintenance_file() ->
    hoax:expect(receive
        chef_log:info("event=webui_maintenance_on") -> ignored;
        file:write_file(?WEBUI_MAINT_FILE, ?any) -> ignored
    end),
    ca_web_maintenance_mode:on(),
    ?verifyAll.

off_logs_and_writes_maintenance_file() ->
    hoax:expect(receive
        chef_log:info("event=webui_maintenance_off") -> ignored;
        file:delete(?WEBUI_MAINT_FILE) -> ignored
    end),
    ca_web_maintenance_mode:off(),
    ?verifyAll.
