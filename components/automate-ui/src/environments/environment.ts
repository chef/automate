/*
The file contents for the current environment will overwrite these during build.
The build system defaults to the dev environment which uses `environment.ts`,
but if you do `ng build --env=prod` then `environment.prod.ts` will be used
instead. The list of which env maps to which file can be found in
`.angular-cli.json`.

Any keys added to an environment file need to be present in all of them or the
app will fail to build. These are the current environment files:
  * habitat/environments/environment.prod.ts
  * src/environments/environment.ts
*/

export const environment = {
  production: false,
  applications_url: '/api/v0/applications',
  iam_url: '/apis/iam/v2',
  users_url: '/api/v0/users',
  secrets_url: '/api/v0/secrets',
  nodes_url: '/api/v0/nodes',
  nodemgrs_url: '/api/v0/nodemanagers',
  profiles_url: '/api/v0/profiles',
  cds_url: '/api/beta/content',
  compliance_url: '/api/v0/compliance',
  config_mgmt_url: '/api/v0/cfgmgmt',
  retention_url: '/api/v0/data-lifecycle',
  ingest_url: '/api/v0/ingest',
  deployment_url: '/api/v0/deployment',
  infra_proxy_url: '/api/v0/infra',
  gateway_url: '/api/v0',
  user_preference_url: '/api/v0/user-settings',
  compliance_stats_url: '/api/v0/compliance/reporting/stats',
  client_runs_stats_url: '/api/v0/cfgmgmt/telemetry',
  // TODO:eng-ex remove elasticsearch_url when all es requests go through config-mgmt
  // don't forget to remove it from the proxy
  elasticsearch_url: '/api/v0/elasticsearch',
  notifier_url: '/api/v0/notifications',
  data_feed_url: '/api/v0/datafeed',
  license_id: '00000000-0000-0000-0000-111111111111',
  install_id: '00000000-0000-0000-0000-111111111111',
  use_default_session: false
};
