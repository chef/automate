-module(scm_routes).

-export([
         routes/0
        ]).

-include_lib("delivery/include/deliv_types.hrl").

%% TODO: We should probably extract the authz mapping stuff into a generic
%% module/applicaiton that all the apps can use.
routes() ->
    State = deliv_routes:set_ttls(#handler{}),
    [
     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/[:org_name]/bitbucket-projects",
      scm_hand_projects,
      State#handler{authz = {organization, [
                                            {
                                              deliv_web_utils:bin_method(post),
                                              [chef_utils:to_bin(admin)]
                                             }]}}},
     {deliv_web_utils:route_prefix()
      ++ ":ent_name/orgs/[:org_name]/github-projects",
      scm_hand_projects,
      State#handler{authz = {organization, [
                                            {
                                              deliv_web_utils:bin_method(post),
                                              [chef_utils:to_bin(admin)]
                                             }]}}},
     {deliv_web_utils:route_prefix()
      ++ ":ent_name/scm/:scm_type/servers",
      scm_hand_servers,
      State#handler{authz = {enterprise, [
                                          {
                                            deliv_web_utils:bin_method(post),
                                            [chef_utils:to_bin(admin)]
                                          },
                                          {
                                            deliv_web_utils:bin_method(get),
                                            [chef_utils:to_bin(admin)]
                                          }]}}},
     {deliv_web_utils:route_prefix()
      ++ ":ent_name/scm/:scm_type/servers/:server_url",
      scm_hand_servers_named,
      State#handler{authz = {enterprise, [
                                          {
                                            deliv_web_utils:bin_method(put),
                                            [chef_utils:to_bin(admin)]
                                          },
                                          {
                                            deliv_web_utils:bin_method(delete),
                                            [chef_utils:to_bin(admin)]
                                          }]}}}
    ].
