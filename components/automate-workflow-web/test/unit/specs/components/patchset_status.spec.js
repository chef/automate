import ng from 'angular';
import patchsetStatusComponent from '../../../../src/components/patchset_status/patchset_status';

describe('patchsetStatusComponent', () => {
  let scope, element;

  beforeEach(ng.mock.module(patchsetStatusComponent));

  function createDirective(stages) {

    return inject(($compile) => {
      scope.change.stages = stages;
      element = $compile(ng.element('<div cd-patchset-status="change"></div>'))(scope);
      scope.$digest();
    });
  }

  beforeEach(inject(($rootScope) => {
    scope = $rootScope.$new();
    scope.change = {};
  }));

  describe('when stage-run information is available', () => {

    beforeEach(() => {
      createDirective([
        {
          stage: 'verify',
          status: 'failed'
        }
      ]);
    });

    it('sets the proper status', () => {
      expect(element.text()).toBe('Verify Failure');
    });
  });

  describe('when stage-run information is not available', () => {

    beforeEach(() => {
      createDirective([]);
    });

    it('sets the status as Queued', () => {
      expect(element.text()).toBe('Queued');
    });
  });
});
