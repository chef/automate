import { uuidv4, eventExist } from '../../../support/helpers';

describe('Config-mgmt action', () => {
  const cypressPrefix = 'test-action';
  const actionId = uuidv4();
  const actionName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
  const actionStart = Cypress.moment().utc().subtract(3, 'day').valueOf().toString();
  const actionEnd = Cypress.moment().utc().endOf('day').valueOf().toString();
  const dateTime = Cypress.moment().utc().subtract(1, 'day').format();
  before(() => {
    // Ingest an action
    cy.fixture('action/environment_create.json').then((action) => {
      action.organization_name = '75th Rangers';
      action.service_hostname = 'example.org';
      action.id = actionId;
      action.entity_name = actionName;
      action.recorded_at = dateTime;
      action.parent_name = 'bob';
      action.parent_type = 'system';

      cy.sendToDataCollector(action);
    });
    // wait for the action to be ingested
    cy.waitForAction(actionName, actionStart, actionEnd);
  });

  it('Test the fields from the event', () => {
    cy.request({
      headers: {
        projects: ['*'],
        'api-token': Cypress.env('ADMIN_TOKEN')
      },
      method: 'GET',
      url: `api/v0/eventfeed?collapse=false&page_size=100&start=${actionStart}&end=${actionEnd}`
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.events && resp.body.events.length > 0 &&
        eventExist(actionName, resp.body.events)).to.equal(true);

      const event = findEvent(actionName, resp.body.events);

      expect(event.entity_name).to.equal(actionName);
      expect(event.event_type).to.equal('environment');
      expect(event.task).to.equal('create');
      expect(event.start_time).to.equal(dateTime);
      expect(event.end_time).to.equal(dateTime);
      expect(event.requestor_type).to.equal('user');
      expect(event.requestor_name).to.equal('mkrasnow');
      expect(event.service_hostname).to.equal('example.org');
      expect(event.parent_name).to.equal('bob');
      expect(event.parent_type).to.equal('system');
    });
  });
});

export function findEvent(entityName: string, events: any[]): any {
  for (const event of events) {
    if (event.entity_name === entityName) {
      return event;
    }
  }

  return {};
}
