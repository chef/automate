import { browser } from 'protractor';

describe('Deprecated routes redirect to the correct new route for backwards compat', () => {
  beforeEach(() => {
    browser.waitForAngularEnabled(false);
  });

  // Note: This is "old path" -> "new path", and the new path will be checked
  // using a regular expression: the string, anchored to the right by '$'.
  [
    ['/settings/node-lifecycle',               '/settings/data-lifecycle'],
    ['/admin/settings',                        '/settings/data-lifecycle'],
    ['/admin/tokens',                          '/settings/tokens'],
    ['/admin/tokens/my-object',                '/settings/tokens/my-object'],
    ['/admin/teams',                           '/settings/teams'],
    ['/admin/teams/my-object',                 '/settings/teams/my-object'],
    ['/admin/users',                           '/settings/users'],
    ['/admin/users/my-object',                 '/settings/users/my-object'],
    ['/admin/policies',                        '/settings/policies'],
    ['/admin/policies/my-object',              '/settings/policies/my-object'],
    ['/admin/policies/my-object/add-members',  '/settings/policies/my-object/add-members'],
    ['/admin/projects',                        '/settings/projects'],
    ['/admin/projects/my-object',              '/settings/projects/my-object'],
    ['/admin/roles',                           '/settings/roles'],
    ['/admin/roles/my-object',                 '/settings/roles/my-object'],
    ['/integrations',                          '/settings/node-integrations'],
    ['/integrations/add',                      '/settings/node-integrations/add'],
    ['/integrations/edit/my-object',           '/settings/node-integrations/edit/my-object'],
    ['/notifications',                         '/settings/notifications'],
    ['/notifications/my-object',               '/settings/notifications/my-object'],
    ['/compliance/credentials',                '/settings/node-credentials'],
    ['/compliance/credentials/add',            '/settings/node-credentials/add'],
    ['/compliance/credentials/my-object/edit', '/settings/node-credentials/my-object/edit']
  ].forEach(([oldPath, newPath]) => {
    it(`${oldPath} -> ${newPath}`, () => {
      browser.get(oldPath);
      expect(browser.getCurrentUrl()).toMatch(new RegExp(`${newPath}$`));
    });
  });
});
