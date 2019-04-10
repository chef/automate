import ng from 'angular';
import 'angular-mocks';
import runDetailItemComponent
  from '../../../../../src/components/grouped_phase_list/run_detail_item/run_detail_item';

describe('runDetailItemComponent', () => {
  let scope, isolateScope, element, $httpBackend;

  beforeEach(ng.mock.module(runDetailItemComponent));

  function createDirective($compile, _$httpBackend_, $rootScope) {
    $httpBackend = _$httpBackend_;
    scope = $rootScope.$new();
    scope.detail = {
      status: 'running',
      description: 'delivery',
      href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
      jobHref: '/jobs/jobid1',
      searchQuery: 'recipes:delivery_builder',
      searchDescription: null
    };

    element = $compile(ng.element('<div cd-run-detail-item run-detail-item="detail" phaseName="unit">'))(scope);
    isolateScope = element.isolateScope();
    scope.$digest();
  }

  describe('when associated job is not found', () => {
    beforeEach(inject(($httpBackend) => {
      $httpBackend
        .when('GET', '/jobs/jobid1')
        .respond(404);
    }));

    beforeEach(() => {
      inject(createDirective);
      $httpBackend.flush();
    });

    describe('initial state', () => {
      it('scope.open should begin as false', () => {
        expect(isolateScope.open).toBe(false);
      });

      it('can not be canceled', () => {
        expect(isolateScope.jobIsCancelable()).toBe(false);
      });
    });

    describe('when the toggle function is called', () => {

      beforeEach(() => {
        spyOn(isolateScope, "$emit");
      });

      it('the first time, scope.open should turn to true', () => {
        isolateScope.toggle();
        expect(isolateScope.open).toBe(true);
        expect(isolateScope.$emit).toHaveBeenCalledWith("phase_log_open_state_change", {
          href: isolateScope.runDetailItem.href,
          open: isolateScope.open
        });
      });
    });
  });

  describe('when associated job is found', () => {
    beforeEach(inject(($httpBackend) => {
      $httpBackend
        .when('GET', '/jobs/jobid1')
        .respond(200);
    }));

    beforeEach(() => {
      inject(createDirective);
      $httpBackend.flush();
    });

    describe('initial state', () => {
      it('can be canceled', () => {
        expect(isolateScope.jobIsCancelable()).toBe(true);
      });
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

      it('can not be canceled again', () => {
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
});
