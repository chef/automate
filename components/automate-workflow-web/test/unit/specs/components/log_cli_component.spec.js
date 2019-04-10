import ng from 'angular';
import 'angular-mocks';
import logCliComponent from '../../../../src/components/log/log_cli';

describe('logCliComponent', () => {
  let scope, isolateScope, element, display;

  beforeEach(ng.mock.module(logCliComponent));

  function createDirective(attrs) {
    function getElement() {

      let el = ng.element('<div cd-cli-log scrollable="true" log="log"></div>');

      for (var key in attrs) {
        el.attr(key, attrs[key]);
      }

      return el;
    }

    return function ($compile, $rootScope) {
      scope = $rootScope.$new();
      scope.log = 'this is a log';

      element = $compile(getElement())(scope);

      display = function () {
        return ng.element(element[0].querySelector('.log-display'));
      };

      isolateScope = element.isolateScope();
      scope.$digest();
    };
  }

  beforeEach(inject(createDirective()));

  describe('log display', () => {

    it('should be present', () => {
      expect(display()).toBeDefined();
    });

    it('should render the log text', () => {
      isolateScope.log = 'some awesome log text';
      isolateScope.$digest();
      expect(display().html()).toBe(isolateScope.log);
    });

    it('escapes all angle brackets', () => {
      isolateScope.log = 'some potentially malicious <script>';
      isolateScope.$digest();
      expect(display().html()).toBe('some potentially malicious &lt;script&gt;');
    });

    it('should scroll to the bottom after log text is rendered if scrollable is true', () => {
      spyOn(isolateScope, 'scrollToBottom');
      isolateScope.log = 'some more awesome log text';
      isolateScope.$digest();
      expect(isolateScope.scrollToBottom).toHaveBeenCalled();
    });

    it('should not scroll to the bottom after log text is rendered if scrollable is false', () => {
      spyOn(isolateScope, 'scrollToBottom');
      isolateScope.scrollable = false;
      isolateScope.log = 'some more awesome log text';
      isolateScope.$digest();
      expect(isolateScope.scrollToBottom).not.toHaveBeenCalled();
    });


  });
});
