import ng from 'angular';
import 'angular-mocks';
import statusController
  from '../../../../../../src/routes/change/status/status_controller';

describe('statusController', () => {

  describe('a change in verify', () => {
    let scope, createController, organization, project, change;

    beforeEach(ng.mock.module(statusController));

    beforeEach(() => {
      organization = { name: 'fooOrg' };
      project = { name: 'fooProject' };
      change = {
        state: 'open',
        topic: 'sc/newfeature',
        comments: [
          {
            id: 0,
            patchset: 3,
            content: 'Comment on patchset 1',
            children: [],
            type: 'patchset'
          },
          {
            id: 1,
            patchset: 3,
            content: 'Line comment on patchset 2',
            children: [],
            line_range: [1, 1],
            type: 'line'
          },
          {
            id: 2,
            patchset: 3,
            content: 'Comment on patchset 3',
            children: [
              { content: "reply comment" }
            ],
            type: 'comment'
          }
        ],
        stage: [
                  {
                    stage: 'verify',
                    status: 'passed'
                  }
                ]
      };

      inject(($controller, $rootScope) => {
        scope = $rootScope.$new();
        scope.organization = organization;
        scope.project = project;
        scope.change = change;

        createController = () => {
          let ctrl = $controller('statusController', {
            $scope: scope,
            organization: organization,
            project: project,
            change: change
          });

          scope.disableAutoProgress();

          return ctrl;
        };
      });
    });

    describe('rerunLink()', () => {

      describe('when a change has a triggerVerify link', () => {
        it('should return a rerun verify link', () => {
          change.links = {
            'triggerVerify': { href: 'http://trigger-verify' }
          };
          createController();
          expect(scope.rerunLink({ stage: 'verify' }))
            .toBe('http://trigger-verify');
        });
      });

      describe('when a change does not have a triggerVerify link', () => {
        it('should return undefined', () => {
          change.links = {};
          createController();
          expect(scope.rerunLink({ stage: 'verify' })).toBeUndefined();
        });
      });
    });

    describe('getPatchsetComments()', () => {

      it('should give us an array of all comments', () => {
        createController();
        let patchset = {sequenceNumber: 3};
        expect(scope.getPatchsetComments(patchset).length).toEqual(4);
      });
    });

    describe('handleRerun()', () => {
      let rerunUrl, rerunResult;

      beforeEach(inject(($q) => {
        rerunResult = $q.defer();
        rerunUrl = 'http://trigger-verify';

        change.rerun = function () { return rerunResult.promise };
        change.links = {
          'triggerVerify': { href: rerunUrl },
        };

        createController();
      }));

      it('should set showRerunLink to false', () => {
        scope.handleRerun(rerunUrl);
        expect(scope.showRerunLink).toBe(false);
      });

      it('should rerun the provided stage url', () => {
        spyOn(change, 'rerun').and.callThrough();
        scope.handleRerun(rerunUrl);
        expect(change.rerun).toHaveBeenCalledWith(rerunUrl);
      });

      it('should set showRerunLink back to true after rerun is complete', () => {
        scope.handleRerun(rerunUrl);
        scope.$apply(rerunResult.resolve);
        expect(scope.showRerunLink).toBe(true);
      });
    });
  });

  describe('a change in acceptance', () => {
    let scope, createController, change, organization, project;

    beforeEach(ng.mock.module(statusController));

    beforeEach(() => {
      organization = { name: 'fooOrg' };
      project = { name: 'fooProject' };
      change = {
        state: 'open',
        topic: 'sc/newfeature',
        stage: [
                {
                  stage: "verify",
                  status: "passed"
                },
                {
                  stage: 'build',
                  status: 'passed'
                },
                {
                  stage: 'acceptance',
                  status: 'passed'
                }
        ],
        deliveredAt: ''
      };

      inject(($controller, $rootScope) => {
        scope = $rootScope.$new();
        scope.organization = organization;
        scope.project = project;
        scope.change = change;

        createController = (updated_change) => {
          let ctrl = $controller('statusController', {
            $scope: scope,
            organization: organization,
            project: project,
            change: updated_change
          });

          return ctrl;
        };
      });
    });

    describe('scope.delivered()', () => {


      describe('when the change is delivered', () => {
        it('returns true', () => {
          change.stage.push(  {
              stage: 'delivered',
              status: 'passed'
            });
          change.deliveredAt = '2016-02-05 02:07:43';
          createController(change);
          expect(scope.delivered()).toBe(true);
        });
      });

      describe('when the change is not delivered', () => {

        describe('and is not superseded', () => {
          it('returns false', () => {
            change.supersedingChange = null;
            createController(change);
            expect(scope.delivered()).toBe(false);
          });
        });

        describe('and is superseded by a delivered change', () => {

          it('returns true', () => {
            change.supersedingChange = {title: "change 2", deliveredAt: '2016-02-05 02:07:43'};
            createController(change);
            expect(scope.delivered()).toBe(true);
          });
        });

        describe('and is superseded by a undelivered change', () => {
          it('returns false', () => {
            change.supersedingChange = {title: "change 2", deliveredAt: ''};
            createController(change);
            expect(scope.delivered()).toBe(false);
          });
        });
      });
    });
  });
});
