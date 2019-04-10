import AuditPage from '../page_objects/audit.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('audit page', () => {

  let auditPage;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {

    mockApi([
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/audit$',
          method: 'GET'
        },
        response: {
          body: [
            {
              change_title: 'THIS IS A FAKE CHANGE',
              submitted_by: 'testUser1',
              submitted_at: '2015-08-28 22:17:45',
              ent: 'test_ent',
              org: 'testing_organization',
              proj: 'delivery',
              change_id: '11111',
              status: 'passed',
              stage_name: 'verify',
              action: 'running'
            },
            {
              change_title: 'Another cool change',
              submitted_by: 'sillyUser3',
              submitted_at: '2015-08-23 22:17:45',
              ent: 'test_ent',
              org: 'testing_organization',
              proj: 'delivery',
              change_id: '33333',
              status: 'running',
              stage_name: 'build',
              action: 'finished',
              approved_by: 'testUser1'
            },
            {
              change_title: 'A funny change',
              submitted_by: 'testUser2',
              submitted_at: '2015-08-20 22:17:45',
              ent: 'test_ent',
              org: 'testing_organization',
              proj: 'octopus',
              change_id: '55555',
              status: 'failed',
              stage_name: 'acceptance',
              action: 'started',
              approved_by: 'testUser2',
              delivered_by: 'testUser3'
            }
          ]
        }
      },
    ]);

    auditPage = new AuditPage();
    auditPage.get();
  });

  it('should display an audit log', () => {
    expect(auditPage.auditLog).toBeDisplayed();
    expect(auditPage.auditLogItems.count()).toBe(3);
  });

  it('should display the project name', () => {
    expect(auditPage.projectLinks.first().getText()).toBe('delivery');
  });

  it('should display the change name', () => {
    expect(auditPage.changeLinks.first().getText()).toBe('THIS IS A FAKE CHANGE');
  });

  describe('filter control', () => {

    it('removes entries that do not match', () => {
      expect(auditPage.changeLinks.first().getText()).toBe('THIS IS A FAKE CHANGE');
      auditPage.filterInput.sendKeys('change:co');
      expect(auditPage.changeLinks.first().getText()).not.toBe('THIS IS A FAKE CHANGE');
      expect(auditPage.changeLinks.first().getText()).toBe('Another cool change');
    });

    it('filters by project', () => {
      auditPage.filterInput.sendKeys('project:octo');
      expect(auditPage.projectLinks.first().getText()).toEqual('octopus');
    });

    it('filters by change', () => {
      auditPage.filterInput.sendKeys('change:fun');
      expect(auditPage.changeLinks.first().getText()).toEqual('A funny change');
    });

    it('filters by stage name', () => {
      auditPage.filterInput.sendKeys('stage:accept');
      expect(auditPage.stageItems.first().getText()).toEqual('acceptance');
    });

    it('filters by status', () => {
      auditPage.filterInput.sendKeys('status:fail');
      expect(auditPage.statusItems.first()).toHaveClass('failed');
    });

    it('filters by action', () => {
      auditPage.filterInput.sendKeys('action:fin');
      expect(auditPage.actionItems.first().getText()).toEqual('finished');
    });

    it('filters by submitter', () => {
      auditPage.filterInput.sendKeys('submitted:sill');
      expect(auditPage.submittedByItems.first().getText()).toEqual('sillyUser3');
    });

    it('filters by approver', () => {
      auditPage.filterInput.sendKeys('approved:testUser2');
      expect(auditPage.approvedByItems.first().getText()).toEqual('testUser2');
    });

    it('filters by deliverer', () => {
      auditPage.filterInput.sendKeys('delivered:te');
      expect(auditPage.deliveredByItems.first().getText()).toEqual('testUser3');
    });

    it('filters on multiple field values', () => {
      auditPage.filterInput.sendKeys('project:delive stage:buil');
      expect(auditPage.changeLinks.first().getText()).toEqual('Another cool change');
    });

    it('clears the field when the escape key is pressed', () => {
      auditPage.filterInput.sendKeys('blahblah');
      expect(auditPage.filterInput.getAttribute('value')).toEqual('blahblah');
      auditPage.filterInput.sendKeys(protractor.Key.ESCAPE);
      expect(auditPage.filterInput.getAttribute('value')).toEqual('');
    });

    it('auto-completes when the tab key is pressed', () => {
      auditPage.filterInput.sendKeys('proj', protractor.Key.TAB);
      expect(auditPage.filterInput.getAttribute('value')).toEqual('project:');
    });

    it('responds to clicked items', () => {
      auditPage.stageItems.first().click();
      expect(auditPage.filterInput.getAttribute('value')).toEqual('stage:verify');
    });
  });
});
