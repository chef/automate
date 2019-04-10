/*
Any keys added to an environment file need to be present in all of them or the
app will fail to build. These are the current environment files:
  * habitat/environments/environment.prod.ts
  * src/environments/environment.ts
*/

export const environment = {
  production: true,
  applications_url: '/apis/beta/applications',
  auth_url: '/api/v0/auth',
  auth_v2_url: '/apis/iam/v2beta',
  users_url: '/api/v0/users',
  secrets_url: '/api/v0/secrets',
  nodes_url: '/api/v0/nodes',
  nodemgrs_url: '/api/v0/nodemanagers',
  profiles_url: '/api/v0/profiles',
  compliance_url: '/api/v0/compliance',
  config_mgmt_url: '/api/v0/cfgmgmt',
  retention_url: '/api/v0/retention',
  ingest_url: '/api/v0/ingest',
  deployment_url: '/api/v0/deployment',
  gateway_url: '/api/v0',
  elasticsearch_url: '/api/v0/elasticsearch',
  notifier_url: '/api/v0/notifications',
  visibility_url: '/api/v0/visibility',
  license_id: '00000000-0000-0000-0000-000000000000',
  install_id: '00000000-0000-0000-0000-000000000000',
  use_default_session: false
};
