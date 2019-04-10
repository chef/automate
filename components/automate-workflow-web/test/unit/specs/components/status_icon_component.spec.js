import ng from 'angular';
import statusIconComponent from '../../../../src/components/status_icon/status_icon';

describe('statusIcon', () => {
  let scope, element;

  beforeEach(ng.mock.module(statusIconComponent));

  function createDirective(elementString) {

    return inject(($compile) => {
      element = $compile(ng.element(elementString))(scope);
      scope.$digest();
    });
  }

  beforeEach(inject(($rootScope) => {
    scope = $rootScope.$new();
  }));

  describe('when a status is passed in', () => {

    beforeEach(() => {
      createDirective('<span status-icon="failed"></span>');
    });

    it('targets the SVG symbol for the given status', () => {
      expect(element.find('use').attr('xlink:href')).toBe('#icon-x');
    });

    it('adds a CSS class with the same name as the status', () => {
      expect(element.hasClass('failed')).toBe(true);
    });
  });
});
