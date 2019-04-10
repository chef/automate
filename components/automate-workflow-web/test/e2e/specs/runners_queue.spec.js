import JobQueuePage from '../page_objects/runners_queue.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('job queue page', () => {
  let queuePage;

  beforeAll(login);

  afterAll(logout);

  describe('when there are no jobs', () => {

    beforeEach(() => {

      mockApi([
        authorizedLogin,
        {
          request: {
            url: '/api/v0/e/Chef/jobs',
            method: 'GET'
          },
          response: {
            body: []
          }
        },
      ]);

      queuePage = new JobQueuePage();
      queuePage.get();
    });

    it('does not display any', () => {
      expect(queuePage.jobEntries.count()).toEqual(0);
    });
  });

  describe('when there is an active job', () => {

    beforeEach(() => {

      mockApi([
          authorizedLogin,
          {
            request: {
              url: '/api/v0/e/Chef/jobs',
              method: 'GET'
            },
            response: {
              body: [
              { status: 'running',
                project: 'test-project',
                change: { id: 'some-id',
                          title: 'this is a test change A' },
                org: 'Chef',
                stage: 'verify',
                phase: 'unit',
                runner: 'myhostname1.example.com',
                timeInState: '20:00',
                submittedAt: '2016-08-26 10:45:00'
              }]
            }
          },
      ]);

      queuePage = new JobQueuePage();
      queuePage.get();
    });

    it('displays the job\'s information', () => {
      expect(queuePage.jobEntries.count()).toEqual(1);
      expect(queuePage.jobStages.first().getText()).toEqual('Verify');
      expect(queuePage.jobProjects.first().getText()).toEqual('test-project');
      expect(queuePage.jobPhases.first().getText()).toEqual('Unit');
      expect(queuePage.jobStatus.first().getText()).toEqual('Running');
      expect(queuePage.jobChanges.first().getText()).toEqual('this is a test change A');
      expect(queuePage.jobTimeInStates.first().getText()).toEqual('20:00');
      expect(queuePage.jobSubmittedAts.first().getText()).toEqual('2016-08-26 10:45:00');
    });

    it('indicates that the job is active', () => {
      expect(queuePage.jobEntries.first()).toHaveClass('active');
    });
  });

  describe('when there is a waiting job', () => {

    beforeEach(() => {

      mockApi([
          authorizedLogin,
          {
            request: {
              url: '/api/v0/e/Chef/jobs',
              method: 'GET'
            },
            response: {
              body: [
              { status: 'pending',
                project: 'test-project',
                change: { id: 'some-id',
                          title: 'this is a test change A' },
                org: 'Chef',
                stage: 'verify',
                phase: 'unit',
                runner: 'myhostname1.example.com',
                timeInState: '20:00',
                submittedAt: '2016-08-26 10:45:00'
              }]
            }
          },
      ]);

      queuePage = new JobQueuePage();
      queuePage.get();
    });

    it('displays the job\'s information', () => {
      expect(queuePage.jobEntries.count()).toEqual(1);
      expect(queuePage.jobStages.first().getText()).toEqual('Verify');
      expect(queuePage.jobProjects.first().getText()).toEqual('test-project');
      expect(queuePage.jobPhases.first().getText()).toEqual('Unit');
      expect(queuePage.jobChanges.first().getText()).toEqual('this is a test change A');
      expect(queuePage.jobStatus.first().getText()).toEqual('Pending');
      expect(queuePage.jobTimeInStates.first().getText()).toEqual('20:00');
      expect(queuePage.jobSubmittedAts.first().getText()).toEqual('2016-08-26 10:45:00');
    });

    it('indicates that the job is waiting', () => {
      expect(queuePage.jobEntries.first()).not.toHaveClass('active');
    });
  });
});
