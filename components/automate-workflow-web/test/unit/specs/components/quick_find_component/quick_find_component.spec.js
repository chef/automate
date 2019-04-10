import ng from 'angular';
import 'angular-mocks';
import quickFindComponent
  from '../../../../../src/components/quick_find/quick_find';

describe('quickFindComponent', () => {
  beforeEach(ng.mock.module(quickFindComponent, ($controllerProvider) => {
    $controllerProvider.register('quickFindController', function () {
      this.getResults = jasmine.createSpy('getResults');
      this.selectResult = jasmine.createSpy('selectResult');
    });
  }));

  function createDirective(scope) {
    let element;
    inject(($compile) => {
      element = $compile('<div cd-quick-find>')(scope);
      scope.$digest();
    });
    return element;
  };

  describe('setup', () => {
    it('should listen for quickFind selections', inject(($rootScope) => {
      let scope = $rootScope.$new();
      spyOn(scope, '$on');

      createDirective(scope);

      expect(scope.$on).toHaveBeenCalled();
      expect(scope.$on.calls.mostRecent().args[0]).toBe('$typeahead.select');
    }));
  });

  describe('when a quickFind selection is made', () => {
    let ctrl, element, scope;

    beforeEach(inject(($rootScope) => {
      scope = $rootScope.$new();
      element = createDirective(scope);
      ctrl = element.controller('cdQuickFind');
    }));

    it('should select the result', () => {
      let result = { project: 'foo' };
      scope.$broadcast('$typeahead.select', result);
      expect(ctrl.selectResult).toHaveBeenCalledWith(result);
    });
  });
});
