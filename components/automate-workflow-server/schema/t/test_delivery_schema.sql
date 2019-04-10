CREATE OR REPLACE FUNCTION test_delivery_schema()
RETURNS SETOF TEXT
LANGUAGE plpgsql
AS $$
BEGIN
  RETURN QUERY SELECT tables_are(ARRAY[
                                       'users',
                                       'user_passwords',
                                       'user_tokens',
                                       'user_cache',
                                       'enterprise_user_roles',
                                       'enterprises',
                                       'organization_user_roles',
                                       'organizations',
                                       'password_reset_tokens',
                                       'pipelines',
                                       'pipeline_user_roles',
                                       'project_user_roles',
                                       'projects',
                                       'changes',
                                       'changesets',
                                       'patchsets',
                                       'comments',
                                       'reviews',
                                       'stage_runs',
                                       'phase_runs',
                                       'stage_ordering',
                                       'patchset_changed_files',
                                       'patchset_diffstats',
                                       'patchset_project_commits',
                                       'project_bitbucket_metadata',
                                       'project_commits',
                                       'project_github_metadata',
                                       'github_patchsets',
                                       'external_basic_auth_applications',
                                       'external_oauth_applications',
                                       'oauth_tokens',
                                       'oauth_user_aliases',
                                       'audit_stage_events',
                                       'enterprise_default_searches',
                                       'scm_changes',
                                       'dependency_failures',
                                       'notification_config',
                                       'notification_subscriptions',
                                       'phase_run_logs',
                                       'saml_config',
                                       'jobs_runners',
                                       'teams',
                                       'team_members',
                                       'profiles',
                                       'nodes_tags',
                                       'nodes',
                                       'nodes_agents',
                                       'tags',
                                       'jobs_profiles',
                                       'jobs',
                                       'agents',
                                       'jobs_nodes',
                                       'jobs_tags',
                                       'nodes_secrets',
                                       'results',
                                       's_secrets_tags',
                                       's_secrets',
                                       'node_managers',
                                       's_tags'
                                       ]);

  -- Views
  RETURN QUERY SELECT views_are(ARRAY[
                                      'internal_users',
                                      'most_recent_patchsets',
                                      'patchset_commits',
                                      'scoped_stage_runs',
                                      'stage_runs_to_restart',
                                      'oauth_integrations'
                                      ]);

  -- Domains
  RETURN QUERY SELECT domains_are(ARRAY[
                                        'cd_timestamp',
                                        'ssh_pub_key',
                                        'token_prefix'
                                        ]);

  RETURN QUERY SELECT domain_type_is('cd_timestamp', 'timestamp with time zone');

  RETURN QUERY SELECT domain_type_is('ssh_pub_key', 'text');
  RETURN QUERY SELECT domain_type_is('token_prefix', 'bpchar');

  -- Enums
  RETURN QUERY SELECT enums_are(ARRAY[
                                      'delivery_role',
                                      'delivery_scope',
                                      'cd_comment_type',
                                      'cd_comment_status',
                                      'cd_patchset_status',
                                      'cd_changeset_status',
                                      'credential',
                                      'password_hash_type',
                                      'user_type'
                                      ]);

  RETURN QUERY SELECT enum_has_labels(
    'delivery_role',
    ARRAY['admin', 'committer', 'reviewer', 'shipper', 'observer']
  );

  RETURN QUERY SELECT enum_has_labels(
    'delivery_scope',
    ARRAY['enterprise', 'organization', 'project', 'pipeline']
  );

  RETURN QUERY SELECT enum_has_labels(
    'password_hash_type',
    ARRAY['bcrypt']
  );

  RETURN QUERY SELECT enum_has_labels(
    'user_type',
    ARRAY['internal', 'external', 'saml']
  );

  RETURN QUERY SELECT enum_has_labels(
    'credential',
    ARRAY['password', 'token']
  );

  RETURN QUERY SELECT enum_has_labels(
    'cd_comment_type',
    ARRAY['patchset', 'line', 'comment']
  );

  RETURN QUERY SELECT enum_has_labels(
    'cd_comment_status',
    ARRAY['draft', 'published']
  );

  RETURN QUERY SELECT enum_has_labels(
    'cd_patchset_status',
    ARRAY['open', 'superseded', 'withdrawn', 'accepted', 'merged']
  );

  RETURN QUERY SELECT enum_has_labels(
    'cd_changeset_status',
    ARRAY['open', 'closed']
  );

  -- Schemata
  RETURN QUERY SELECT has_schema('utility');

END;
$$;
