import ng from 'angular';
import aceComponent from '../../../../src/components/ace/ace';

describe('aceComponent', () => {
  let scope, element, isolateScope;

  beforeEach(ng.mock.module(aceComponent));

  function createDirective($compile, $rootScope) {
    scope = $rootScope.$new();
    scope.comment = '';

    element = $compile(ng.element('<cd-ace ng-model="comment"></cd-ace>'))(scope);
    isolateScope = element.isolateScope();
  }

  beforeEach(inject(createDirective));

  describe('when the ace editor is loaded', () => {

    it('should autofocus to the text area', () => {
      spyOn(isolateScope, 'setFocus');
      scope.$digest();
      expect(isolateScope.setFocus).toHaveBeenCalled();
    });

  });
});
