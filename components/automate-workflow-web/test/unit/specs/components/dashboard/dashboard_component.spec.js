import ng from 'angular';
import 'angular-mocks';
import dashboardComponent from '../../../../../src/routes/dashboard/dashboard_component';

describe('dashboardComponent', () => {
  let scope, isolateScope, element, headbar;

  beforeEach(ng.mock.module(dashboardComponent));

  function createDirective(attrs) {
    return function ($compile, $rootScope) {
      scope = $rootScope.$new();
      element = $compile(ng.element('<div cd-dashboard />'))(scope);

      headbar = function () {
        return ng.element(element[0].querySelector('.head-bar'));
      };

      scope.$digest();
    };
  }

  it('keeps the header bar anchored at all times', () => {
    inject(createDirective());
    expect(headbar().hasClass('anchored')).toBe(true);
  });
});
