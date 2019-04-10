import RunnersManagePage from '../page_objects/runners_manage.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('manage runners page', () => {
  let runnersPage;

  beforeAll(login);

  afterAll(logout);

  describe('when there are no runners', () => {

    beforeAll(() => {

      mockApi([
        authorizedLogin,
        {
          request: {
            url: '/api/v0/e/Chef/runners$',
            method: 'GET'
          },
          response: {
            body: []
          }
        },
      ]);

      runnersPage = new RunnersManagePage();
      runnersPage.get();
    });

    it('displays the "no runners" information', () => {
      expect(runnersPage.noRunnersInfo).toBePresent();
      expect(runnersPage.noRunnersInfo.getText()).toEqual('You don\'t have any runners yet. Let\'s get started.');
    });
  });

  describe('when there are runners', () => {

    beforeAll(() => {

      mockApi([
          authorizedLogin,
          {
            request: {
              url: '/api/v0/e/Chef/runners$',
              method: 'GET'
            },
            response: {
              body: [{
                id: 'uuid01',
                health: {},
                hostname: 'alice',
                openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@alice\n'
              },
              {
                id: 'uuid02',
                health: {},
                hostname: 'bob',
                openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@bob\n'
              }]
            }
          },
      ]);

      runnersPage = new RunnersManagePage();
      runnersPage.get();
    });

    it('displays the runner hostname', () => {
      expect(runnersPage.runnerHostnames.first().getText()).toEqual('alice');
    });

    it('does not display the "no runners" information', () => {
      expect(runnersPage.noRunnersInfo).not.toBePresent();
    });

    it('displays the unknown status icon', () => {
      expect(runnersPage.statusIcons.first()).toHaveClass('runner-status-unknown');
    });

    it('does not display command output', () => {
      expect(runnersPage.runnerOutputs.count()).toBe(0);
    });

    it('has enabled runner\'s test buttons', () => {
      expect(runnersPage.testButtons.first().isEnabled()).toBe(true);
    });
  });

  describe('when there is a healthy idle runner', () => {

    beforeAll(() => {

      mockApi([
          authorizedLogin,
          {
            request: {
              url: '/api/v0/e/Chef/runners$',
              method: 'GET'
            },
            response: {
              body: [{
                id: 'uuid01',
                hostname: 'alice',
                job: {},
                health: {
                  status: 'ok',
                  command_output: 'Success!'
                },
                openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@alice\n'
              }]
            }
          },
      ]);

      runnersPage = new RunnersManagePage();
      runnersPage.get();
    });

    it('displays the healthy status icon', () => {
      expect(runnersPage.statusIcons.first()).toHaveClass('runner-status-ok');
    });

    it('displays job Idle', () => {
      expect(runnersPage.runnerIdles.first().getText()).toBe('Idle');
    });

    it('displays an Output link with the command output', () => {
      expect(runnersPage.runnerOutputs.first().getText()).toBe('Output');
      runnersPage.runnerOutputs.first().getAttribute('data-title').then((attr) => {
        expect(attr).toBe('Success!');
      });
    });
  });

  describe('when there is a healthy runner executing a job', () => {

    beforeAll(() => {

      mockApi([
          authorizedLogin,
          {
            request: {
              url: '/api/v0/e/Chef/runners$',
              method: 'GET'
            },
            response: {
              body: [{
                id: 'uuid01',
                hostname: 'alice',
                job: {
                  id: 'change-id',
                  title: 'name of the change',
                  org: 'acme',
                  project: 'proj_01'
                },
                health: {
                  status: 'ok',
                  command_output: 'Success!'
                },
                openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@alice\n'
              }]
            }
          },
      ]);

      runnersPage = new RunnersManagePage();
      runnersPage.get();
    });

    it('displays the healthy status icon', () => {
      expect(runnersPage.statusIcons.first()).toHaveClass('runner-status-ok');
    });

    it('displays current job\'s change title (ellipsed)', () => {
      expect(runnersPage.runnerJobs.first().getText()).toBe('name of the change');
    });

    it('displays an Output link with the command output', () => {
      expect(runnersPage.runnerOutputs.first().getText()).toBe('Output');
      runnersPage.runnerOutputs.first().getAttribute('data-title').then((attr) => {
        expect(attr).toBe('Success!');
      });
    });

    it('disables the test button', () => {
      expect(runnersPage.testButtons.first().isEnabled()).toBe(false);
    });
  });

  describe('when there is an unhealthy runner', () => {

    beforeAll(() => {

      mockApi([
          authorizedLogin,
          {
            request: {
              url: '/api/v0/e/Chef/runners$',
              method: 'GET'
            },
            response: {
              body: [{
                id: 'uuid01',
                hostname: 'alice',
                health: {
                  status: 'error',
                  command_output: 'couldn\'t resolve host'
                },
                openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@alice\n'
              }]
            }
          }
      ]);

      runnersPage = new RunnersManagePage();
      runnersPage.get();
    });

    it('does display the unhealthy status icon', () => {
      expect(runnersPage.statusIcons.first()).toHaveClass('runner-status-error');
    });

    it('displays an Error link', () => {
      expect(runnersPage.runnerOutputs.first().getText()).toBe('Error');

      runnersPage.runnerOutputs.first().getAttribute('data-title').then((attr) => {
        expect(attr).toBe('couldn\'t resolve host');
      });
    });
  });

  describe('when the test button is clicked', () => {

    describe('when testing is successfully initiated', () => {

      beforeEach(() => {
        mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/runners$',
                method: 'GET'
              },
              response: {
                body: [{
                  id: 'uuid01',
                  hostname: 'alice',
                  health: {},
                  openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@alice\n',
                  job: {}
                }]
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/runners/alice/health$',
                method: 'POST'
              },
              response: {
                status: 200,
                body: {
                  id: 'uuid01',
                  hostname: 'alice',
                  health: {
                    status: 'pending',
                    command_output: null
                  },
                  openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@alice\n'
                }
              }
            },
          ]);

        runnersPage = new RunnersManagePage();
        runnersPage.get();
        runnersPage.testButtons.first().click();
      });

      it('does display the in-progress status icon', () => {
        expect(runnersPage.statusIcons.first()).toHaveClass('runner-status-in-progress');
      });

      it('does no longer display command output', () => {
        expect(runnersPage.runnerOutputs.count()).toBe(0);
      });

      it('disables the test button', () => {
        expect(runnersPage.testButtons.first().isEnabled()).toBe(false);
      });
    });

    describe('when test initiation fails', () => {

      beforeEach(() => {
        mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/runners$',
                method: 'GET'
              },
              response: {
                body: [{
                  id: 'uuid01',
                  hostname: 'alice',
                  health: {},
                  openssh_public_key: 'ssh-rsa base64edpubkey= dbuild@alice\n'
                }]
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/runners/alice/health$',
                method: 'POST'
              },
              response: {
                status: 500
              }
            },
            ]);

        runnersPage = new RunnersManagePage();
        runnersPage.get();
        runnersPage.testButtons.first().click();
      });

      it('does not display the in-progress status icon', () => {
        expect(runnersPage.statusIcons.first()).not.toHaveClass('runner-status-in-progress');
      });
    });
  });
});
