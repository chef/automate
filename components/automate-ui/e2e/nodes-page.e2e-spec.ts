import { browser, by, $, $$ } from 'protractor';
import { fakeServer } from './helpers/fake_server';
import { expectUrlToBeAccessible } from './helpers/accessibility_helpers';

describe('Nodes page', () => {
  let expectedVersion;

  beforeEach(() => {
    expectedVersion = '20180416135645';
    const body = `{"build_timestamp":"${expectedVersion}"}`;
    fakeServer().get('/api/v0/version').many().reply(200, body);

    const node_counts = '{"success":2,"failure":1,"missing":0,"total":3}';
    fakeServer().get('/api/v0/cfgmgmt/stats/node_counts').many().reply(200, node_counts);

    // tslint:disable-next-line:max-line-length
    const nodes = '[{"id":"45100301-a7ff-4c29-b28e-5d04ee06c6c6","name":"Ad-nobis-dolorem","fqdn":"Ad-nobis-dolorem.robel.biz","checkin":"2017-09-22T15:48:16Z","uptime_seconds":15376433,"organization":"org1","environment":"","platform":"ubuntu","platform_family":"robinson","platform_version":"0.0.42","policy_name":"policy2","policy_group":"prod","policy_revision":"cf3b1437a438484ad290498683d2f0620a62f0be954beb5255c60344b6595894","status":"success","source_fqdn":"chef-server.robel.biz","latest_run_id":"884cd70f-2e4f-4680-b636-55428c27cdc3"},{"id":"f4c30570-83db-43cd-982f-6748b19f5855","name":"Adipisci-quia-deserunt","fqdn":"Adipisci-quia-deserunt.robel.biz","checkin":"2017-09-22T15:27:40Z","uptime_seconds":6118499,"organization":"org1","environment":"acceptance-org-proj-master","platform":"ubuntu","platform_family":"robinson","platform_version":"0.0.42","policy_name":"policy2","policy_group":"dev","policy_revision":"8aac29ef9bdce306f8c0c8e04fb391ec9498ab75d31015af0973debdbe9b4d02","status":"failure","source_fqdn":"chef-server.robel.biz","latest_run_id":"d0937f82-10dd-4c84-825c-ed17971daf08"}]';

    fakeServer()
      .get('/api/v0/cfgmgmt/nodes?pagination.page=1&pagination.size=100&' +
        'sorting.field=name&sorting.order=ASC')
      .reply(200, nodes);
  });

  it('displays the node counts', () => {
    browser.waitForAngularEnabled(false);
    browser.get('/client-runs');

    const legend = $('.chart-legend');

    const total_node_count = $$('chef-radial-chart .innerText').first();
    expect(total_node_count.getText()).toBe('3 Total Nodes');

    const node_counts = legend.all(by.css('.display5'));

    const failed_nodes = node_counts.all(by.css('.failure')).first();
    expect(failed_nodes.getText()).toBe('1');

    const successful_nodes = node_counts.all(by.css('.success')).first();
    expect(successful_nodes.getText()).toBe('2');

    const missing_nodes = node_counts.all(by.css('.missing')).first();
    expect(missing_nodes.getText()).toBe('0');
  });

  it('displays the node list', () => {
    browser.waitForAngularEnabled(false);
    browser.get('/client-runs');

    const node_list = $$('chef-table.node-list chef-tbody chef-tr');

    expect(node_list.count()).toBe(2);

    const node_icons = node_list.$$('.node-name chef-icon');
    expect(node_icons.get(0).getText()).toBe('check_circle');
    expect(node_icons.get(1).getText()).toBe('warning');
  });

  xit('is accessible', (done) => {
    browser.waitForAngularEnabled(false);
    expectUrlToBeAccessible('/client-runs', done);
  });
});
