export default {
  build_version: '0.0.1',
  external_auth: false,
  password_recovery_url: 'http://google.com',
  embedly_api_key: 'e0435c6ccfd74dfaacf7dfc987c9a7fa',
  dev_mode: true,

  api: {
    base_url: '/workflow/api/',
    version: 'v0',
    endpoints: {
      auth: '/get-token',
      projects: '/orgs/:org/projects/:project',
      orgs: '/orgs/:orgname',
      users: '/users/:user',
      searches: '/searches'
    }
  }
};
