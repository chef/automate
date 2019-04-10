import ChangePage from '../page_objects/change.po';
import { lorem } from 'faker';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('change page', () => {

  let change, changePage, changeStream;

  beforeAll(login);

  afterAll(logout);

  let params = {
    org: 'Chef_Delivery',
    project: 'delivery',
    change: 'e789b453-7e11-4091-95c4-af7a03a90bb0'
  };

  function getChange(opts = {}) {
    let baseChange = {
      id: params.change,
      title: 'Add project quickFind component',
      description: 'This commit adds a project quickFind component',
      topic: 'DUN-2/sc/quick-find',
      target: 'master',
      state: 'open',
      submit_at: '2015-08-13 15:22:17',
      submit_by: 'scottopherson',
      stages:[
        {
          stage: 'verify',
          status: 'running',
          phases: [
            {
              name: 'unit',
              status: 'running',
              runDetails: [
                {
                  status: 'running',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
                  job_href: '/jobs/a',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                },
                {
                  status: 'passed',
                  description: 'delivery_cli',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
                  job_href: '/jobs/b',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }
              ]
            },
            {
              name: 'lint',
              status: 'passed',
              runDetails: [
                {
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33167',
                  job_href: '/jobs/c',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }
              ]
            },
            {
              name: 'syntax',
              status: 'running',
              runDetails: [
                {
                  status: 'running',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33168',
                  job_href: '/jobs/d',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }
              ]
            }
          ]
        }
      ],
      patchsets: [
        {
          sequence_number: 1,
          sha: 'c08b85d02cd7541645910f1fe6d02b5166487568',
          submitted_at: '2015-08-13 20:08:51',
          stats: {
            insertions: 475,
            deletions: 46
          },
          files_changed:[
            ['M', 'web/src/components/header/header.html'],
            ['A', 'web/src/components/quick_find/quick_find.html'],
            ['A', 'web/src/components/quick_find/quick_find.js']
          ],
          commit_msgs:[
            [
              'Add project quickFind component',
              '',
              'c08b85d02cd7541645910f1fe6d02b5166487568'
            ]
          ]
        }
      ],
      _links: {},
      promotion: {}
    };

    if (!!opts.delivered) {
      baseChange.delivered_at = '2015-09-09 11:22:33';
      baseChange.delivered_by = 'cnunciato';
    }

    return baseChange;
  }

  function buildMockApi() {
    mockApi([
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/orgs$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {
            orgs: []
          }
        }
      },
      {
        request: {
          url: `/orgs/${params.org}$`,
          method: 'GET'
        },
        response: {
          body: {
            name: 'Chef_Delivery',
            project_count: 4
          }
        }
      },
      {
        request: {
          url: `/orgs/${params.org}/projects$`,
          method: 'GET'
        },
        response: {
          body: [
            {
              name: 'delivery',
              git_url: 'ssh://git-url',
              scm: {
                type: 'local'
              }
            }
          ]
        }
      },
      {
        request: {
          url: `/orgs/${params.org}/projects/${params.project}$`,
          method: 'GET'
        },
        response: {
          body: {
            name: 'delivery',
            ent_name: 'Chef',
            org_name: 'Chef_Delivery'
          }
        }
      },
      {
        request: {
          url: `/orgs/${params.org}/projects/${params.project}/changes$`,
          method: 'GET'
        },
        response: {
          body: [
            change
          ]
        }
      },
      {
        request: {
          url: `/orgs/${params.org}/projects/${params.project}/pipelines$`,
          method: 'GET'
        },
        response: {
          body: {
            pipelines: ['master']
          }
        }
      },
      {
        request: {
          url: `/orgs/${params.org}/projects/${params.project}/changes/${params.change}$`,
          method: 'GET'
        },
        response: {
          body: change
        }
      },
      {
        request: {
          url: `/orgs/${params.org}/projects/${params.project}/changes/${params.change}/comments$`,
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url:
           `/orgs/${params.org}/projects/${params.project}/pipelines/master/phase_runs/33168$`,
          method: 'GET'
        },
        response: {
          body: {
            id: 33166,
            stage_run_id: 8776,
            phase: 'unit',
            status: 'passed',
            finished: true,
            run_success: true,
            run_log: `${lorem.paragraphs(5)}
              Compliance data has posted.
              ${lorem.paragraphs(5)}`,
            run_status: 'finished',
            build_node: 'builder5.shd.chef.co',
            search_query: '(recipes:delivery_builder)',
            search_description: null
          }
        }
      },
      {
        request: {
          url: `/bitbucket-servers$`,
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url:
           `/orgs/${params.org}/projects/${params.project}/pipelines/master/phase_runs/33168/log_objects$`,
          method: 'GET'
        },
        response: {
          body: [
            {
              data: '{"version": "3.4.3","examples": [{"id": "01","title": "Do not run deprecated inetd or xinetd","desc": "http://www.nsa.gov/ia/_files/os/redhat/rhel5-guide-i731.pdf, Chapter 3.2.1","code": "control code","status": "passed","code_desc": "System Package inetd should not be installed"}]}',
              run_id: "33168"
            }
          ]
        }
      }
    ]);
  }

  beforeEach(() => {
    changePage = new ChangePage();

    let streamUrl =
      `/api/v0/e/Chef` +
      `/orgs/${params.org}` +
      `/projects/${params.project}` +
      `/changes/${params.change}/streaming`;

    changeStream = mockSse(streamUrl);
  });

  // In UpgradeAdapter/ng12hybrid mode, Protractor may fail to detect the resolution
  // of outstanding promises (for example, with the $interval service) and time out.
  // This instructs Protractor to bypass that default (and usually desired) behavior.
  beforeEach(() => {
    browser.ignoreSynchronization = true;
  });

  afterEach(() => {
    browser.ignoreSynchronization = false;
  });

  describe('after a change is submitted', () => {

    beforeEach(() => {
      change = getChange();
      buildMockApi();
    });

    it('should have a change title', () => {
      changePage.get(params);
      expect(changePage.title).toBeDisplayed();
      expect(changePage.title).toHaveExactText(change.title);
    });

    it('should have a change description', () => {
      changePage.get(params);
      changePage.summaryTab.click();
      expect(changePage.description).toBeDisplayed();
      expect(changePage.description.getText()).toBe('This commit adds a project quickFind component');
    });

    it('should have a list of commits', () => {
      changePage.get(params);
      changePage.summaryTab.click();
      expect(changePage.commitItems.count()).toBe(1);
      expect(changePage.commitItems.first().getText()).toMatch('Add project quickFind component');
    });

    it('should not show a delivered-by participant', () => {
      changePage.get(params);
      expect(changePage.deliveredBy.isPresent()).toBe(false);
    });

    describe('when a change has a stage with a phase that has multiple run details', () => {

      describe('and the grouped phase list is opened', () => {

        beforeEach(() => {
          changePage.get(params);
          changePage.openNestedPhases();
        });

        it('should display the phase run detail description', () => {

          expect(changePage.phaseRunDetailDescription).toBeDisplayed();
          expect(changePage.phaseRunDetailDescription).toBeDisplayed();
          expect(changePage.phaseRunDetailDescription).toContainText(change.stages[0].phases[0].runDetails[0].description);
        });

        describe('and a run detail item log is opened and then closed', () => {

          it('the log should not be present on the page', () => {
            changePage.openRunDetailPhaseLog();
            expect(changePage.logDisplay).toBePresent();
            changePage.clickCloseButton();
            expect(changePage.logDisplay).not.toBePresent();
          });
        });
      });
    });
  });

  describe('after a change is delivered', () => {

    beforeEach(() => {
      change = getChange({ delivered: true });
      buildMockApi();
    });

    it('should show a delivered-by participant', () => {
      changePage.get(params);
      expect(changePage.deliveredBy.isPresent()).toBe(true);
    });
  });

  describe('viewing a phase log', () => {

    beforeEach(() => {
      change = getChange();
      buildMockApi();
    });

    describe('when a build event is triggered', () => {

      it('should retain its scroll position', () => {

        function openPhaseLog() {
          return changePage.openPhaseLog();
        }

        function triggerBuildEvent() {
          let data = Object.assign({}, change, { title: 'new change title' });
          return changeStream.send('build_event', data);
        }

        changePage
          .get(params)
          .then(openPhaseLog)
          .then(scrollToTop)
          .then(triggerBuildEvent)
          .then(getScrollPosition)
          .then((scrollPos) => {
            expect(scrollPos.y).toEqual(0);
          });
      });
    });

    describe('when a log phase view is opened', () => {

      beforeEach(() => {
        changePage.get(params);
        changePage.openPhaseLog();
      })

      it('the log should be present', () => {
        expect(changePage.logDisplay).toBePresent();
      });

      describe('and the user hits the close button', () => {

        beforeEach(() => {
          changePage.clickCloseButton();
        });

        it('it should close the log view', () => {
          expect(changePage.logDisplay).not.toBePresent();
        });
      });

      describe('and the user hits the scroll up button', () => {

        beforeEach(() => {
          changePage.scrollUp();
        });

        it('the log view is still present', () => {
          expect(changePage.logDisplay).toBePresent();
        });

        describe('and the user hits the scroll down position', () => {

          it('the position changes', () => {
            let position = getScrollPosition();
            changePage.scrollDown();
            expect(getScrollPosition()).not.toEqual(position);
          });
        });
      });

      describe('and the user hits the scroll down button', () => {
        beforeEach(() => {
          changePage.scrollDown();
        });

        it('the log view is still present', () => {
          expect(changePage.logDisplay).toBePresent();
        });

        describe('and the user hits the scroll up position', () => {

          it('the position changes', () => {
            let position = getScrollPosition();
            changePage.scrollUp();
            expect(getScrollPosition()).not.toEqual(position);
          });
        });
      });

      describe('and there are inspec test results in the log', () => {
        beforeEach(() => {
          featureFlag('enable', 'compliance-report');
        });

        afterEach(() => {
          featureFlag('reset', 'compliance-report');
        });

        it('should display the compliance report view', () => {
          expect(changePage.complianceReport).toBeDisplayed();
        });

        it('should display a button to switch back to the cli log view', () => {
          expect(changePage.switchViewsButton).toBeDisplayed();
        });

        // a future iteration will enable this, but for now, it is not
        // possible to download the report view (it is being parsed out of the log)
        it('the download button should not be visible', () => {
          expect(changePage.downloadButton).not.toBeDisplayed();
        });
      });
    });
  });

  describe('when no stage-run information available ', () => {

    beforeEach(() => {

      change = Object.assign(getChange(), {
        stages: []
      });

      buildMockApi();
    });

    it('should represent the change as queued', () => {
      changePage.get(params);
      expect(changePage.patchsetStatus.getText()).toBe('Queued');
    });

    it('should hide the stage-run details section of the view', () => {
      changePage.get(params);
      expect(changePage.stageRunDetails).not.toBeDisplayed();
    });
  });

  describe('when a change has a promotion status', () => {

    describe('other than "proceed"', () => {

      beforeEach(() => {
        change = getChange();

        change.promotion = {
          status: 'caution',
          reason: 'pipeline_union_failure'
        };
      });

      describe('and the change is approvable', () => {

        beforeEach(() => {
          change.state = 'open';
          buildMockApi();
        });

        it('should modify the look of the Approve button', () => {
          changePage.get(params);
          expect(changePage.approveButton).toHaveClass('caution');
        });

        it('should provide an explanatory message', () => {
          changePage.get(params);
          expect(changePage.promotionStatus).toBeDisplayed();
          expect(changePage.promotionStatus.getText()).toBe('Union stage failures. Promote with caution.');
        });
      });

      describe('and the change is deliverable', () => {

        beforeEach(() => {
          change.state = 'merged';
          buildMockApi();
        });

        it('should modify the look of the Deliver button', () => {
          changePage.get(params);
          expect(changePage.deliverButton).toHaveClass('caution');
        });

        it('should provide an explanatory message', () => {
          changePage.get(params);
          expect(changePage.promotionStatus).toBeDisplayed();
          expect(changePage.promotionStatus.getText()).toBe('Union stage failures. Promote with caution.');
        });
      });
    });
  });

  describe('when a change is superseded by another change', () => {
    let supersedingChange, supersedingParams;

    beforeEach(() => {
      supersedingChange = Object.assign(getChange(), {
        id: 'superseding-change-id',
        title: 'A superseding change'
      });
      change = Object.assign(getChange(), {
        state: 'merged',
        superseding_change: supersedingChange,
        promotion: {
          status: 'disabled',
          reason: 'change_superseded'
        },
        stages:[
          {
            stage: 'verify',
            status: 'running',
            phases:[
              {
                name: 'unit',
                status: 'running',
                runDetails: [{
                  status: 'running',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                },
                  {
                    status: 'passed',
                    description: 'delivery_cli',
                    href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
                    search_query: 'recipes:delivery_builder',
                    search_description: null
                  }]
              },
              {
                name: 'lint',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33167',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'syntax',
                status: 'running',
                runDetails: [{
                  status: 'running',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33168',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              }
            ]
          },
          {
            stage: 'build',
            status: 'passed',
            phases:[
              {
                name: 'unit',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'lint',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33167',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'syntax',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33168',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'quality',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33169',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'security',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33170',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'publish',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33171',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              }
            ]
          },
          {
            stage: 'acceptance',
            status: 'passed',
            phases:[
              {
                name: 'provision',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'deploy',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33167',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'smoke',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33168',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              },
              {
                name: 'functional',
                status: 'passed',
                runDetails: [{
                  status: 'passed',
                  description: 'delivery',
                  href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33169',
                  search_query: 'recipes:delivery_builder',
                  search_description: null
                }]
              }
            ]
          }
        ]
      });

      buildMockApi();

      supersedingParams = Object.assign({}, params, { change: supersedingChange.id });
      mockApi([
        authorizedLogin,
        {
          request: {
            url: `/orgs/${supersedingParams.org}/projects/${supersedingParams.project}/changes/${supersedingParams.change}$`,
            method: 'GET'
          },
          response: {
            body: supersedingChange
          }
        },
        {
          request: {
            url: `/orgs/${supersedingParams.org}/projects/${supersedingParams.project}/changes/${supersedingParams.change}/comments$`,
            method: 'GET'
          },
          response: {
            body: []
          }
        }
      ]);
    });

    describe('the superseded change', () => {

      it('should display a link to the superseding change', () => {
        changePage.get(params);

        expect(changePage.supersedingChangeLink).toBeDisplayed();
        expect(changePage.supersedingChangeLink.getText())
          .toBe(supersedingChange.title);
      });

      it('should mark remaining stages as unreachable', () => {
        changePage.get(params);

        expect(changePage.unreachableStages.count()).toBe(3);
      });

      it('should have a human icon in the status-nav', () => {
        changePage.get(params);

        expect(changePage.statusNavHumanIcon).toBePresent();
      });

      it('should have a human icon in the state title element', () => {
        changePage.get(params);

        expect(changePage.stageTitleHumanIcon).toBePresent();
      });
    });

    describe('the superseding change', () => {

      it('should not display a superseding change link', () => {
        changePage.get(supersedingParams);

        expect(changePage.supersedingChangeLink.isPresent()).toBe(false);
      });
    });
  });

  describe('when a change belongs to a bitbucket project', () => {

    beforeEach(() => {
      change = getChange();
      change._links = {
        external_pr: {
          href: 'https://bitbucket-pr/',
          title: 'Bitbucket Pull Request (#12)'
        }
      };
      buildMockApi();
    });

    it('should display a link to the bitbucket pull request', () => {
      changePage.get(params);
      expect(changePage.externalPrLink.getText()).toEqual('Bitbucket Pull Request (#12)');
      expect(changePage.externalPrLink.getAttribute('href')).toEqual('https://bitbucket-pr/');
    });
  });
});
