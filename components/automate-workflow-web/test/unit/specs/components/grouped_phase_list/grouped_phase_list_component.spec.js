import ng from 'angular';
import 'angular-mocks';
import groupedPhaseListComponent
  from '../../../../../src/components/grouped_phase_list/grouped_phase_list';

describe('groupedPhaseListComponent', () => {
  let scope, element, isolateScope, $httpBackend;

  beforeEach(ng.mock.module(groupedPhaseListComponent));

  function createDirective(phaseStatus, runDetails) {
    return ($compile, _$httpBackend_, $rootScope) => {
      $httpBackend = _$httpBackend_;
      scope = $rootScope.$new();
      scope.phase = {
        name: 'unit',
        status: phaseStatus,
        runDetails: runDetails
      };
      element = $compile(ng.element('<div cd-grouped-phase-list="phase">'))(scope);
      isolateScope = element.isolateScope();
      scope.$digest();
    };
  }

  describe('when associated job is found', () => {

    beforeEach(inject(($httpBackend) => {
      $httpBackend
        .when('GET', '/jobs/jobid1')
        .respond(200);
    }));

    describe('and the phase status is running', () => {

      beforeEach( () => {
        inject(createDirective('running', [{ // minimal runDetails for this test
          jobHref: '/jobs/jobid1'
        }]));
        $httpBackend.flush();
      });

      it('can be canceled', () => {
        expect(isolateScope.jobIsCancelable()).toBe(true);
      });

      describe('and the phase is canceled successfully', () => {

        beforeEach(inject(($httpBackend) => {
          $httpBackend
            .when('DELETE', '/jobs/jobid1')
            .respond(204);
        }));

        beforeEach( () => {
          isolateScope.cancelPhase();
          $httpBackend.flush();
        });

        it('cannot be canceled again', () => {
          expect(isolateScope.jobIsCancelable()).toBe(false);
        });
      });

      describe('and the phase is not canceled successfully', () => {

        beforeEach(inject(($httpBackend) => {
          $httpBackend
            .when('DELETE', '/jobs/jobid1')
            .respond(500);
        }));

        beforeEach( () => {
          isolateScope.cancelPhase();
          $httpBackend.flush();
        });

        it('can be canceled again', () => {
          expect(isolateScope.jobIsCancelable()).toBe(true);
        });
      });
    });

    describe('and the phase status is idle', () => {

      beforeEach( () => {
        inject(createDirective('idle', [{
          jobHref: '/jobs/jobid1'
        }]));
        $httpBackend.flush();
      });

      it('can be canceled', () => {
        expect(isolateScope.jobIsCancelable()).toBe(true);
      });
    });

    describe('and the phase status is passed', () => {

      beforeEach( () => {
        inject(createDirective('passed', [{
          jobHref: '/jobs/jobid1'
        }]));
        $httpBackend.flush();
      });

      it('can not be canceled', () => {
        expect(isolateScope.jobIsCancelable()).toBe(false);
      });
    });

    describe('when there is only one runDetail', () => {
      beforeEach( () => {
        inject(createDirective('running', [{
          status: 'running',
          description: 'delivery',
          href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
          jobHref: '/jobs/jobid1',
          searchQuery: 'recipes:delivery_builder',
          searchDescription: null
        }]));
        // directive creation calls backend to see if the phase run's associated
        // job can be canceled.
        $httpBackend.flush();
      });

      it('the unit phase should have one element', () => {
        expect(isolateScope.phase.runDetails.length).toBe(1);
      });

      it('the directive should begin rolled up', () => {
        expect(isolateScope.open).toBe(false);
        expect(isolateScope.logOpen).toBe(false);
      });

      describe('and toggle() is called', () => {

        beforeEach(() => {
          spyOn(isolateScope, '$emit');
        });

        it('the log should be revealed', () => {
          isolateScope.toggle();
          expect(isolateScope.logOpen).toBe(true);
          expect(isolateScope.open).toBe(false);

          expect(isolateScope.$emit).toHaveBeenCalledWith('phase_log_open_state_change', {
            href: isolateScope.phase.runDetails[0].href,
            open: isolateScope.logOpen
          });
        });
      });
    });
  });

  describe('when associated job is not found', () => {

    beforeEach(inject(($httpBackend) => {
      $httpBackend
        .when('GET', '/jobs/jobid1')
        .respond(404);
    }));

    beforeEach( () => {
      inject(createDirective('running', [{
        status: 'running',
        description: 'delivery',
        href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
        jobHref: '/jobs/jobid1',
        searchQuery: 'recipes:delivery_builder',
        searchDescription: null
      }]));
      $httpBackend.flush(); // note comment above
    });

    it('cannot be canceled', () => {
      expect(isolateScope.jobIsCancelable()).toBe(false);
    });
  });

  describe('when there are two runDetails', () => {


    beforeEach( () => {
      inject(createDirective('running', [{
        status: 'running',
        description: 'delivery',
        href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
        jobHref: '/jobs/jobid1',
        searchQuery: 'recipes:delivery_builder',
        searchDescription: null
      },
      {
        status: 'running',
        description: 'delivery',
        href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
        jobHref: '/jobs/jobid2',
        searchQuery: 'recipes:delivery_builder',
        searchDescription: null
      }]));
    });

    it('the unit phase should have two elements', () => {
      expect(isolateScope.phase.runDetails.length).toBe(2);
    });

    it('scope.open should begin as false', () => {
      expect(isolateScope.open).toBe(false);
      expect(isolateScope.logOpen).toBe(false);
    });

    it('does not show a cancel button', () => {
      expect(isolateScope.jobIsCancelable()).toBe(false);
    });

    describe('and toggle() is called', () => {

      beforeEach(() => {
        spyOn(isolateScope, '$emit');
      });

      it('should show its child phases, but no logs', () => {
        isolateScope.toggle();
        expect(isolateScope.open).toBe(true);
        expect(isolateScope.logOpen).toBe(false);
      });
    });
  });
});
