import ng from 'angular';
import 'angular-mocks';
import Change from '../../../../../src/common/models/change'
import changeController
  from '../../../../../src/routes/change/change_controller';
import confirmDeletedModalTemplate
  from '../../../../../src/common/ui/modal/confirm_deleted_modal.html';

describe('changeController', () => {
  let scope, createController, $window, $document, $state, $httpParamSerializer,
      Modal, session, organization, project, change, pipeline, mockEventSource;

  beforeEach(ng.mock.module(Change));
  beforeEach(ng.mock.module(changeController));

  beforeEach(() => {
    inject((_$window_, _$document_, _$state_, _$httpParamSerializer_) => {
      $window = _$window_;
      $document = _$document_;
      $state = _$state_;
      $httpParamSerializer = _$httpParamSerializer_;
    });
  });

  beforeEach(() => {
    inject(($rootScope, Change, Session, _Modal_) => {
      Modal = _Modal_;
      session = Session.get();
      organization = { name: 'fooOrg' };
       project = { name: 'fooProject', scm: {} };
      change = Change.$buildRaw({
        state: 'open',
        topic: 'sc/newfeature'
      });
    });
  });

  beforeEach(() => {
    inject(($controller, $rootScope) => {
      scope = $rootScope.$new();

      createController = (projectType) => {
        project.scm.type = projectType ? projectType : 'local';
        return $controller('changeController', {
          $scope: scope,
          $window: $window,
          Modal: Modal,
          organization: organization,
          project: project,
          change: change
        });
      };
    });
  });

  // Need to mock out window.EventSource so we don't actually attempt to make
  // any real streaming connections. Plus we're going to need a mock to attach
  // spies to.
  beforeEach(() => {
    // Just need an object we can attach/dispatch events on
    mockEventSource = angular.extend($document[0].createElement('div'), {
      close: jasmine.createSpy('close')
    });
    spyOn($window, 'EventSource').and.returnValue(mockEventSource);
  });

  describe('setup', () => {

    it('should attach the current project to the scope', () => {
      createController();
      expect(scope.project).toBe(project);
    });

    it('should attach the current change to the scope', () => {
      createController();
      expect(scope.change).toBe(change);
    });

    describe('setting up event streaming', () => {

      beforeEach(() => {
        spyOn(mockEventSource, 'addEventListener');
      });

      it('should open a connection to the streaming endpoint', () => {
        let streamPath = `${change.$url()}/streaming`;
        let streamParams = $httpParamSerializer({
          'chef-delivery-user': session.username,
          'chef-delivery-token': session.token
        });
        let streamUrl = `${streamPath}?${streamParams}`;
        createController();
        expect($window.EventSource).toHaveBeenCalledWith(streamUrl);
      });

      it('should listen to the stream for "open" events', () => {
        createController();
        expect(mockEventSource.addEventListener)
          .toHaveBeenCalledWith('open', scope.handleStreamOpen);
      });

      it('should listen to the stream for "build_event" events', () => {
        createController();
        expect(mockEventSource.addEventListener)
          .toHaveBeenCalledWith('build_event', scope.handleBuildEvent);
      });

      it('should listen to the stream for "change_deleted" events', () => {
        createController();
        expect(mockEventSource.addEventListener)
          .toHaveBeenCalledWith('change_deleted', scope.handleDeletedEvent);
      });

      it('should listen to the stream for "phase_run_updated" events', () => {
        createController();
        expect(mockEventSource.addEventListener)
          .toHaveBeenCalledWith('phase_run_updated', scope.handlePhaseRunUpdatedEvent);
      });

      it('should listen to the stream for "error" events', () => {
        createController();
        expect(mockEventSource.addEventListener)
          .toHaveBeenCalledWith('error', scope.handleStreamError);
      });

      it('should listen to the stream for "promotion_status_update" events', () => {
        createController();
        expect(mockEventSource.addEventListener)
          .toHaveBeenCalledWith('promotion_status_update', scope.handlePromotionStatusUpdate);
      });
    });
  });

  describe('when change promotion status', () => {

    describe('is "caution"', () => {

      beforeEach(() => {
        change.promotion = {
          status: 'caution',
          reason: 'pipeline_union_failure'
        };
        createController();
      });

      it('discourages promotion', () => {
        expect(scope.promoteDiscouraged()).toBe(true);
      });

      it('provides a promotion class', () => {
        expect(scope.promoteClass()).toBe('caution');
      });

      it('allows promotion', () =>{
        expect(scope.allowPromotion()).toBe(true);
      });

      it('provides a promotion message', () => {
        expect(scope.promoteMessage()).toBe('Union stage failures.');
      });

      it('provides a promotion status reason', () => {
        expect(scope.promoteStatusReason()).toBe('pipeline_union_failure');
      });
    });

    describe('is "disabled"', () => {

      describe('and the change is superseded', () => {
        beforeEach(() => {
          change.promotion = {
            status: 'disabled',
            reason: 'change_superseded'
          };
          createController();
        });

        it('provides a promotion class', () => {
          expect(scope.promoteClass()).toBe('disabled');
        });

        it('provides a promotion message', () => {
          expect(scope.promoteMessage()).toBe('');
        });

        it('prevents promotion', () =>{
          expect(scope.allowPromotion()).toBe(false);
        });

        it('provides a promotion status reason', () => {
          expect(scope.promoteStatusReason()).toBe('change_superseded');
        });
      });

      describe('and the change is delivered', () => {
        beforeEach(() => {
          change.promotion = {
            status: 'disabled',
            reason: 'change_delivered'
          };
          createController();
        });

        it('provides a promotion class', () => {
          expect(scope.promoteClass()).toBe('disabled');
        });

        it('provides a promotion message', () => {
          expect(scope.promoteMessage()).toBe('');
        });

        it('prevents promotion', () =>{
          expect(scope.allowPromotion()).toBe(false);
        });

        it('provides a promotion status reason', () => {
          expect(scope.promoteStatusReason()).toBe('change_delivered');
        });
      });
    });

    describe('is "proceed"', () => {

      beforeEach(() => {
        change.promotion = {
          status: 'proceed',
        };
        createController();
      });

      it('does not discourage promotion', () => {
        expect(scope.promoteDiscouraged()).toBe(false);
      });

      it('provides a promotion class', () => {
        expect(scope.promoteClass()).toBe('');
      });

      it('allows promotion', () =>{
        expect(scope.allowPromotion()).toBe(true);
      });

      it('does not provide a promotion message', () => {
        expect(scope.promoteMessage()).toBe('');
      });

      it('does not provide a promotion status reason', () => {
        expect(scope.promoteStatusReason()).toBe('');
      });
    });
  });

  describe('streaming change events', () => {

    describe('when a connection is opened', () => {

      it('should fetch any new change data', () => {
        spyOn(change, '$fetch');
        createController();
        mockEventSource.dispatchEvent(new Event('open'));
        expect(change.$fetch).toHaveBeenCalled();
      });
    });

    describe('when a build_event is received', () => {

      it('should update the change with the new data', () => {
        let eventData = { fresh: 'data' };
        let buildEvent = angular.extend(new Event('build_event'), {
          data: JSON.stringify(eventData)
        });
        spyOn(change, '$decode');
        createController();
        mockEventSource.dispatchEvent(buildEvent);
        expect(change.$decode).toHaveBeenCalledWith(eventData);
      });
    });

    describe('when a phase_run_updated event is received', () => {

      it('should rebroadcast the event', () => {
        let eventData = { run_log: 'some things' };
        let phaseRunEvent = angular.extend(new Event('phase_run_updated'), {
          data: JSON.stringify(eventData)
        });
        spyOn(scope, '$broadcast');
        createController();
        mockEventSource.dispatchEvent(phaseRunEvent);
        expect(scope.$broadcast).toHaveBeenCalledWith('phase_run_updated', eventData);
      });
    });

    describe('when a readyState of 2 is received', () => {
      beforeEach(() => {
        createController();
        spyOn(scope, 'closeStream');
        spyOn(scope, 'initStream');
        scope.stream.readyState = 2;
      });

      it('should reset the SSE connection', inject(($interval) => {
        $interval.flush(5000);
        expect(scope.closeStream).toHaveBeenCalled();
        expect(scope.initStream).toHaveBeenCalled();
      }));

      describe('and the change is delivered', () => {

        it('should not reset the SSE connection', inject(($interval) => {
          scope.change.stages = [{ stage: 'delivered', status: 'passed' }];
          $interval.flush(5000);
          expect(scope.closeStream).not.toHaveBeenCalled();
          expect(scope.initStream).not.toHaveBeenCalled();
        }));
      });
    });

    describe('when a change_deleted event is received', () => {
      let eventData, deleteEvent;

      beforeEach(() => {
        eventData = { deleted_by: 'bob' };
        deleteEvent = angular.extend(new Event('change_deleted'), {
          data: JSON.stringify(eventData)
        });
        spyOn(Modal, 'open');
      });

      it('should update the change with the deletedBy name', () => {
        createController();
        mockEventSource.dispatchEvent(deleteEvent);
        expect(change.deletedBy).toBe(eventData.deleted_by);
      });

      it('should open the "confirm deleted" modal', () => {
        createController();
        mockEventSource.dispatchEvent(deleteEvent);
        expect(Modal.open).toHaveBeenCalledWith(
          'Change Deleted',
          confirmDeletedModalTemplate,
          'red-modal',
          scope
        );
      });

      describe('confirming the current change has been deleted', () => {

        beforeEach(() => {
          spyOn(Modal, 'close');
          spyOn($state, 'go');
        });

        it('should close the "confirm deleted" modal', () => {
          createController();
          scope.confirmDeleted();
          expect(Modal.close).toHaveBeenCalled();
        });

        it('should redirect to the project view', () => {
          createController();
          scope.confirmDeleted();
          expect($state.go).toHaveBeenCalledWith('main.enterprise.organizations.organization.project');
        });
      });
    });

    describe('when an error event is received', () => {

      it('should fetch change data', () => {
        spyOn(change, '$fetch');
        createController();
        mockEventSource.dispatchEvent(new Event('error'));
        expect(change.$fetch).toHaveBeenCalled();
      });
    });

    describe('when a promotion_status_update event is received', () => {
      describe('and the existing status is not promotable', () => {
        let eventData, promotionStatusUpdateEvent;

        beforeEach(() => {
          change.promotion = {
            status: 'disabled',
            reason: 'change_delivered'
          };
          eventData = {
            promotion: {
              status: 'caution',
              reason: 'pipeline_union_failure'
            }
          };
          promotionStatusUpdateEvent = angular.extend(new Event('promotion_status_update'), {
            data: JSON.stringify(eventData)
          });
          createController();
        });

        it('does not update the change object', () => {
          mockEventSource.dispatchEvent(promotionStatusUpdateEvent);
          expect(change.promotion).not.toEqual(eventData.promotion);
        })
      });

      describe('and the existing status is promotable', () => {
        beforeEach(() => {
          change.promotion = {
            status: 'proceed'
          };
        });

        describe('and the promotion status is "proceed"', () => {
          let eventData, promotionStatusUpdateEvent;

          beforeEach(() => {
            eventData = {
              promotion: {
                status: 'proceed'
              }
            };

            promotionStatusUpdateEvent = angular.extend(new Event('promotion_status_update'), {
              data: JSON.stringify(eventData)
            });
          });

          it('updates the change with the new promotion status', () => {
            createController();
            mockEventSource.dispatchEvent(promotionStatusUpdateEvent);
            expect(change.promotion).toEqual(eventData.promotion);
          });
        });

        describe('and the promotion status is "caution"', () => {
          let eventData, promotionStatusUpdateEvent;

          beforeEach(() => {
            eventData = {
              promotion: {
                status: 'caution',
                reason: 'pipeline_union_failure'
              }
            };

            promotionStatusUpdateEvent = angular.extend(new Event('promotion_status_update'), {
              data: JSON.stringify(eventData)
            });
          });

          it('updates the change with the new promotion status', () => {
            createController();
            mockEventSource.dispatchEvent(promotionStatusUpdateEvent);
            expect(change.promotion).toEqual(eventData.promotion);
          });

          describe('when the promotion status updates to "proceed"', () => {
            let proceedEventData, promotionStatusUpdateEvent2;

            beforeEach(() => {
              proceedEventData = {
                promotion: {
                  status: 'proceed'
                }
              };
              promotionStatusUpdateEvent2 = angular.extend(new Event('promotion_status_update'), {
                data: JSON.stringify(proceedEventData)
              });
              createController();
            });

            it('updates the change with the new promotion status and no promotion reason', () => {
              mockEventSource.dispatchEvent(promotionStatusUpdateEvent);
              mockEventSource.dispatchEvent(promotionStatusUpdateEvent2);
              expect(change.promotion).toEqual(proceedEventData.promotion);
            });
          });
        });

        describe('and the promotion status is "disabled"', () => {
          describe('and the promotion reason is "change_superseded"', () => {
            let eventData, promotionStatusUpdateEvent;

            beforeEach(() => {
              eventData = {
                superseding_change: {
                  topic: 'another-change'
                },
                promotion: {
                  status: 'disabled',
                  reason: 'change_superseded'
                }
              };

              promotionStatusUpdateEvent = angular.extend(new Event('promotion_status_update'), {
                data: JSON.stringify(eventData)
              });
            });

            it('updates the change with the new promotion status', () => {
              createController();
              mockEventSource.dispatchEvent(promotionStatusUpdateEvent);
              expect(change.promotion).toEqual(eventData.promotion);
            });

            it('updates the change with the superseding change', () => {
              createController();
              mockEventSource.dispatchEvent(promotionStatusUpdateEvent);
              expect(change.superseding_change.topic).toEqual(eventData.superseding_change.topic);
            });
          });
        });
      });
    });
  });

  describe('no_pr_url_text()', () => {

    it('returns bitbucket text when bitbucket project', () => {
      createController('bitbucket');
      expect(scope.no_pr_url_text()).toEqual("There is a Bitbucket PR for this change");
    });

    it('returns github text when github project', () => {
      createController('githubV2');
      expect(scope.no_pr_url_text()).toEqual("There is a GitHub PR for this change");
    });
  });

  describe('cleanup', () => {

    it('should close the streaming connection', () => {
      createController();
      scope.$broadcast('$destroy');
      expect(mockEventSource.close).toHaveBeenCalled();
    });
  });
});
