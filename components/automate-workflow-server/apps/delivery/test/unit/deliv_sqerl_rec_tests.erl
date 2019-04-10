%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(deliv_sqerl_rec_tests).

-include_lib("eunit/include/eunit.hrl").

gen_scoped_sqerl_statements_test_() ->
    [
     {"Queries for users",
      ?_assertEqual(
        [{insert_by_scoping_params,
          "INSERT INTO users (enterprise_id, name, ssh_pub_key, first_name, last_name, email, user_type, telemetry_enabled) "
          "SELECT enterprises.id, $2, $3, $4, $5, $6, $7, $8 "
          "FROM enterprises "
          "WHERE enterprises.name = $1 "
          "RETURNING id, enterprise_id, name, ssh_pub_key, first_name, last_name, email, user_type, telemetry_enabled"},
         {delete_by_scoping_params,
          "DELETE FROM users "
          "USING enterprises "
          "WHERE enterprises.name = $1 "
          "AND users.name = $2 "
          "AND users.enterprise_id = enterprises.id"},
         {fetch_by_scoping_params,
          "SELECT users.id, users.enterprise_id, users.name, users.ssh_pub_key, users.first_name, users.last_name, users.email, users.user_type, users.telemetry_enabled "
          "FROM users "
          "JOIN enterprises "
          "ON users.enterprise_id = enterprises.id "
          "WHERE enterprises.name = $1 "
          "AND users.name = $2"}],
         flattened_scoped_statements_for(deliv_user)
      )},
      {"Queries for projects",
       ?_assertEqual(
        [{insert_by_scoping_params,
          "INSERT INTO projects (organization_id, name) "
          "SELECT organizations.id, $3 "
          "FROM organizations "
          "JOIN enterprises "
          "ON organizations.enterprise_id = enterprises.id "
          "WHERE enterprises.name = $1 "
          "AND organizations.name = $2 "
          "RETURNING id, organization_id, scm_module, guid, name"},
         {delete_by_scoping_params,
          "DELETE FROM projects "
          "USING organizations "
          "JOIN enterprises "
          "ON organizations.enterprise_id = enterprises.id "
          "WHERE enterprises.name = $1 "
          "AND organizations.name = $2 "
          "AND projects.name = $3 "
          "AND projects.organization_id = organizations.id"},
         {fetch_by_scoping_params,
          "SELECT projects.id, projects.organization_id, projects.scm_module, projects.guid, projects.name "
          "FROM projects "
          "JOIN organizations "
          "ON projects.organization_id = organizations.id "
          "JOIN enterprises "
          "ON organizations.enterprise_id = enterprises.id "
          "WHERE enterprises.name = $1 "
          "AND organizations.name = $2 "
          "AND projects.name = $3"}],
         flattened_scoped_statements_for(deliv_project)
       )},
       {"Queries for pipelines",
        ?_assertEqual(
         [{insert_by_scoping_params,
           "INSERT INTO pipelines (project_id, name) "
           "SELECT projects.id, $4 FROM projects "
           "JOIN organizations "
           "ON projects.organization_id = organizations.id "
           "JOIN enterprises "
           "ON organizations.enterprise_id = enterprises.id "
           "WHERE enterprises.name = $1 "
           "AND organizations.name = $2 "
           "AND projects.name = $3 "
           "RETURNING id, project_id, name"},
          {delete_by_scoping_params,
           "DELETE FROM pipelines "
           "USING projects "
           "JOIN organizations "
           "ON projects.organization_id = organizations.id "
           "JOIN enterprises "
           "ON organizations.enterprise_id = enterprises.id "
           "WHERE enterprises.name = $1 "
           "AND organizations.name = $2 "
           "AND projects.name = $3 "
           "AND pipelines.name = $4 "
           "AND pipelines.project_id = projects.id"},
          {fetch_by_scoping_params,
           "SELECT pipelines.id, pipelines.project_id, pipelines.name "
           "FROM pipelines "
           "JOIN projects "
           "ON pipelines.project_id = projects.id "
           "JOIN organizations "
           "ON projects.organization_id = organizations.id "
           "JOIN enterprises "
           "ON organizations.enterprise_id = enterprises.id "
           "WHERE enterprises.name = $1 "
           "AND organizations.name = $2 "
           "AND projects.name = $3 "
           "AND pipelines.name = $4"}],
          flattened_scoped_statements_for(deliv_pipeline)
        )},
        {"Queries for changes",
         ?_assertEqual(
          [{insert_by_scoping_params,
            "INSERT INTO changes (pipeline_id) "
            "SELECT pipelines.id FROM pipelines "
            "JOIN projects "
            "ON pipelines.project_id = projects.id "
            "JOIN organizations "
            "ON projects.organization_id = organizations.id "
            "JOIN enterprises "
            "ON organizations.enterprise_id = enterprises.id "
            "WHERE enterprises.name = $1 "
            "AND organizations.name = $2 "
            "AND projects.name = $3 "
            "AND pipelines.name = $4 "
            "RETURNING id, pipeline_id, feature_branch, merge_sha, "
            "title, description, approved_by, approved_at, changeset_id, "
            "latest_patchset_status, latest_patchset, submitted_at, "
            "submitted_by, delivered_at, delivered_by, pipeline_name_at_creation, superseding_change_id"},
           {delete_by_scoping_params,
            "DELETE FROM changes "
            "USING pipelines "
            "JOIN projects "
            "ON pipelines.project_id = projects.id "
            "JOIN organizations "
            "ON projects.organization_id = organizations.id "
            "JOIN enterprises "
            "ON organizations.enterprise_id = enterprises.id "
            "WHERE enterprises.name = $1 "
            "AND organizations.name = $2 "
            "AND projects.name = $3 "
            "AND pipelines.name = $4 "
            "AND changes.feature_branch = $5 "
            "AND changes.merge_sha IS NULL "
            "AND changes.pipeline_id = pipelines.id"},
           {fetch_by_scoping_params,
            "SELECT changes.id, changes.pipeline_id, changes.feature_branch, "
            "changes.merge_sha, changes.title, changes.description, changes.approved_by, changes.approved_at, "
            "changes.changeset_id, changes.latest_patchset_status, changes.latest_patchset, "
            "changes.submitted_at, changes.submitted_by, changes.delivered_at, changes.delivered_by, "
            "changes.pipeline_name_at_creation, "
            "changes.superseding_change_id "
            "FROM changes "
            "JOIN pipelines "
            "ON changes.pipeline_id = pipelines.id "
            "AND changes.merge_sha IS NULL "
            "JOIN projects "
            "ON pipelines.project_id = projects.id "
            "JOIN organizations "
            "ON projects.organization_id = organizations.id "
            "JOIN enterprises "
            "ON organizations.enterprise_id = enterprises.id "
            "WHERE enterprises.name = $1 "
            "AND organizations.name = $2 "
            "AND projects.name = $3 "
            "AND pipelines.name = $4 "
            "AND changes.feature_branch = $5"}],
           flattened_scoped_statements_for(deliv_change)
         )},
         {"Queries for patchsets",
          %% these queries look quite horrible, but they actually use
          %% only indexes (pgSQL is pretty good at re-ordering the
          %% clauses to match indexes!)
          ?_assertEqual(
           [{insert_by_scoping_params,
             "INSERT INTO patchsets (change_id) "
             "SELECT changes.id FROM changes "
             "JOIN pipelines "
             "ON changes.pipeline_id = pipelines.id "
             "AND changes.merge_sha IS NULL "
             "JOIN projects "
             "ON pipelines.project_id = projects.id "
             "JOIN organizations "
             "ON projects.organization_id = organizations.id "
             "JOIN enterprises "
             "ON organizations.enterprise_id = enterprises.id "
             "WHERE enterprises.name = $1 "
             "AND organizations.name = $2 "
             "AND projects.name = $3 "
             "AND pipelines.name = $4 "
             "AND changes.feature_branch = $5 "
             "RETURNING id, change_id, sequence_number, submitted_at, sha, submitter_id, verified_against_sha, is_verified, status"},
            {delete_by_scoping_params,
             "DELETE FROM patchsets "
             "USING changes "
             "JOIN pipelines "
             "ON changes.pipeline_id = pipelines.id "
             "AND changes.merge_sha IS NULL "
             "JOIN projects "
             "ON pipelines.project_id = projects.id "
             "JOIN organizations "
             "ON projects.organization_id = organizations.id "
             "JOIN enterprises "
             "ON organizations.enterprise_id = enterprises.id "
             "WHERE enterprises.name = $1 "
             "AND organizations.name = $2 "
             "AND projects.name = $3 "
             "AND pipelines.name = $4 "
             "AND changes.feature_branch = $5 "
             "AND patchsets.sequence_number = $6 "
             "AND patchsets.change_id = changes.id"},
            {fetch_by_scoping_params,
             "SELECT patchsets.id, patchsets.change_id, patchsets.sequence_number, patchsets.submitted_at, patchsets.sha, patchsets.submitter_id, patchsets.verified_against_sha, patchsets.is_verified, patchsets.status "
             "FROM patchsets "
             "JOIN changes "
             "ON patchsets.change_id = changes.id "
             "JOIN pipelines "
             "ON changes.pipeline_id = pipelines.id "
             "AND changes.merge_sha IS NULL "
             "JOIN projects "
             "ON pipelines.project_id = projects.id "
             "JOIN organizations "
             "ON projects.organization_id = organizations.id "
             "JOIN enterprises "
             "ON organizations.enterprise_id = enterprises.id "
             "WHERE enterprises.name = $1 "
             "AND organizations.name = $2 "
             "AND projects.name = $3 "
             "AND pipelines.name = $4 "
             "AND changes.feature_branch = $5 "
             "AND patchsets.sequence_number = $6"}],
            flattened_scoped_statements_for(deliv_patchset)
          )}
    ].

flattened_scoped_statements_for(Module) ->
    lists:map(
        fun({Name, Content}) -> {Name, lists:flatten(Content)} end,
        deliv_sqerl_rec:gen_scoped_sqerl_statements(Module)
    ).

gen_fetch_names_by_scoping_names_test_() ->
    [{"Fetch projects' names",
      ?_assertEqual(
         "SELECT projects.name "
         "FROM projects "
         "JOIN organizations "
         "ON projects.organization_id = organizations.id "
         "JOIN enterprises "
         "ON organizations.enterprise_id = enterprises.id "
         "WHERE enterprises.name = $1 "
         "AND organizations.name = $2 "
         "ORDER BY projects.name",
         lists:flatten(deliv_sqerl_rec:gen_fetch_names_by_scoping_names(deliv_project))
      )},
     {"Fetch organizations' names",
      ?_assertEqual(
         "SELECT organizations.name "
         "FROM organizations "
         "JOIN enterprises "
         "ON organizations.enterprise_id = enterprises.id "
         "WHERE enterprises.name = $1 "
         "ORDER BY organizations.name",
         lists:flatten(deliv_sqerl_rec:gen_fetch_names_by_scoping_names(deliv_organization))
      )}].

gen_fetch_scoping_params_from_id_test_() ->
    {fetch_scoping_params_from_id, ChangeQuery} = deliv_sqerl_rec:gen_fetch_scoping_params_from_id(deliv_change),
    {fetch_scoping_params_from_id, OrgQuery} = deliv_sqerl_rec:gen_fetch_scoping_params_from_id(deliv_organization),
    [{"Fetch change's scoping params",
      ?_assertEqual(
         "SELECT enterprises.name, organizations.name, projects.name, pipelines.name "
         "FROM changes "
         "JOIN pipelines "
         "ON changes.pipeline_id = pipelines.id "
         "JOIN projects "
         "ON pipelines.project_id = projects.id "
         "JOIN organizations "
         "ON projects.organization_id = organizations.id "
         "JOIN enterprises "
         "ON organizations.enterprise_id = enterprises.id "
         "WHERE changes.id = $1",
         lists:flatten(ChangeQuery)
      )},
     {"Fetch organizations's scoping params",
      ?_assertEqual(
         "SELECT enterprises.name "
         "FROM organizations "
         "JOIN enterprises "
         "ON organizations.enterprise_id = enterprises.id "
         "WHERE organizations.id = $1",
         lists:flatten(OrgQuery)
      )}].
