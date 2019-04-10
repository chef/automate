import ng from 'angular';
import iconButtonComponent from '../../../../src/components/icon_button/icon_button';

describe('iconButton', () => {
  let scope, element;

  beforeEach(ng.mock.module(iconButtonComponent));

  function createDirective(elementString) {

    return inject(($compile) => {
      element = $compile(ng.element(elementString))(scope);
      scope.$digest();
    });
  }

  beforeEach(inject(($rootScope) => {
    scope = $rootScope.$new();
  }));

  describe('when an SVG symbol name is provided', () => {

    beforeEach(() => {
      createDirective('<button icon-button="checkmark">Approve</button>');
    });

    it('targets the specified SVG symbol', () => {
      expect(element.find('use').attr('xlink:href')).toBe('#icon-checkmark');
    });
  });

  describe('when the content provided is static', () => {

    beforeEach(() => {
      createDirective('<button icon-button="checkmark">Approve</button>');
    });

    it('appends the content', () => {
      expect(element.find('span').text()).toBe('Approve')
    });
  });

  describe('when the content provided contains a binding expression', () => {

    beforeEach(() => {
      scope.buttonLabel = 'Click Me!';
      createDirective('<button icon-button="checkmark">{{ buttonLabel }}</button>');
    });

    it('evaluates the expression and appends the content', () => {
      expect(element.find('span').text()).toBe(scope.buttonLabel);
    });
  });
});
