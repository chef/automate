import ng from 'angular';
import 'angular-mocks';
import logComponent from '../../../../src/components/log/log';

describe('logComponent', () => {
  let scope, isolateScope, element, display, toolbar, $window, $http;

  beforeEach(ng.mock.module(logComponent));

  beforeEach(inject((_$window_, $httpBackend) => {
    $window = _$window_;
    $httpBackend
      .when('GET', /phase_runs/)
      .respond({ run_log: 'some log text' });
  }));

  function createDirective(attrs) {
    function getElement() {
      let el = ng.element('<div cd-log log-href="href">');

      for (var key in attrs) {
        el.attr(key, attrs[key]);
      }
      return el;
    }

    return function ($compile, $rootScope) {
      scope = $rootScope.$new();
      scope.href = 'http://phase_runs';
      element = $compile(getElement())(scope);

      toolbar = function () {
        return ng.element(element[0].querySelector('.log-toolbar'));
      };

      display = function () {
        return ng.element(element[0].querySelector('.log-display'));
      };

      isolateScope = element.isolateScope();
      scope.$digest();
    };
  }

  beforeEach(inject(createDirective()));

  describe('setup', () => {

    it('should add a scroll event listener to the window', () => {
      spyOn($window, 'addEventListener');
      inject(createDirective());
      expect($window.addEventListener)
        .toHaveBeenCalledWith('scroll', isolateScope.handleScroll);
      expect($window.addEventListener)
          .toHaveBeenCalledWith('mousewheel', isolateScope.handleMouseWheel);
    });

    it('should set scrollable to true', () => {
      inject(createDirective());
      expect(isolateScope.scrollable).toBe(true);
    })
  });

  describe('the log', () => {

    it('should update its own log', () => {

      scope.$broadcast('phase_run_updated', {
        href: scope.href,
        run_log: 'my phase log'
      });

      isolateScope.$digest();
      expect(isolateScope.log).toBe('my phase log');
    });

    it('should not update its own log in response to a different href', () => {

      scope.$broadcast('phase_run_updated', {
        href: scope.href,
        run_log: 'my phase log'
      });

      scope.$broadcast('phase_run_updated', {
        href: 'http://a_different_phase_run',
        run_log: 'a different phase log'
      });

      isolateScope.$digest();
      expect(isolateScope.log).toBe('my phase log');
    });
  });

  describe('when a user uses the mouse scroll', () => {
    it('should set scope.scrollable to false', () => {
      $window.dispatchEvent(new Event('mousewheel'));
      expect(isolateScope.scrollable).toBe(false);
    });
  });

  describe('log toolbar', () => {

    it('should be present', () => {
      expect(toolbar()).toBeDefined();
    });

    describe('with an optional toolbar attribute', () => {

      describe('set to false', () => {

        it('should be absent', () => {
          inject(createDirective({ 'data-toolbar': 'false' }));
          expect(toolbar().length).toBe(0);
        });
      });

      describe('set to true', () => {

        it ('should be present', () => {
          inject(createDirective({ 'data-toolbar': 'true' }));
          expect(toolbar().length).toBe(1);
        });
      });
    });

    it('should have tooltips', () => {
      function titleForButton(name) {
        return toolbar()[0].querySelector(`.btn-${name}`).getAttribute('title');
      }

      expect(titleForButton('top')).toBeTruthy();
      expect(titleForButton('bottom')).toBeTruthy();
      expect(titleForButton('close')).toBeTruthy();
    });

    describe('when the top of the log display is scrolled beyond the viewport', () => {

      beforeEach(() => {
        spyOn(element[0], 'getBoundingClientRect').and.returnValue({ top: -150 });
      });

      it('should anchor to the top of the viewport', () => {
        $window.dispatchEvent(new Event('scroll'));
        expect(toolbar().hasClass('anchored')).toBe(true);
      });
    });

    describe('when the top of the log display is scrolled within the viewport', () => {

      beforeEach(() => {
        spyOn(element[0], 'getBoundingClientRect').and.returnValue({ top: 150 });
      });

      it('should not anchor to the top of the viewport', () => {
        $window.dispatchEvent(new Event('scroll'));
        expect(toolbar().hasClass('anchored')).toBe(false);
      });
    });

    describe('download button', () => {

      it('should have its href point to the log endpoint', () => {
        let href = `${isolateScope.href}/log`;
        isolateScope.$digest();
        expect(isolateScope.downloadHref).toContain(href);
      });

      describe('when the include color checkbox is unchecked', () => {

        it('should have colorize param set to false in its href', () => {
          isolateScope.colorize = false;
          isolateScope.$digest();
          expect(isolateScope.downloadHref).toContain('colorize=false');
        });
      });

      describe('when the include color checkbox is checked', () => {

        it('should have colorize param set to true in its href', () => {
          isolateScope.colorize = true;
          isolateScope.$digest();
          expect(isolateScope.downloadHref).toContain('colorize=true');
        });
      });
    });

    describe('switchView button', () => {
      let btn;

      describe('when there are inspec test results', () => {

        beforeEach(() => {
          btn = ng.element(toolbar()[0].querySelector('.btn-switch'));
          scope.viewPlugin = 'compliance';
        });

        it('should be visible', () => {
          expect(btn).toBeVisible;
        });

        it('should switch to the log cli view when clicked', () => {
          spyOn(isolateScope, 'switchView');
          btn.triggerHandler('click');
          expect(isolateScope.switchView).toHaveBeenCalled();
        });
      });

      describe('when there are not inspec test results', () => {

        beforeEach(() => {
          btn = ng.element(toolbar()[0].querySelector('.btn-switch'));
          scope.viewPlugin = 'log';
        });

        it('should not be present', () => {
          expect(btn).not.toBeVisible;
        });
      });
    });

    describe('when scroll to bottom button is clicked', () => {
      let btn;

      beforeEach(() => {
        btn = ng.element(toolbar()[0].querySelector('.btn-bottom'));
      });

      it('should scroll display to the bottom', () => {
        spyOn(isolateScope, 'scrollToBottom');
        btn.triggerHandler('click');
        expect(isolateScope.scrollToBottom).toHaveBeenCalled();
      });
    });

    describe('when scroll to top button is clicked', () => {
      let btn;

      beforeEach(() => {
        btn = ng.element(toolbar()[0].querySelector('.btn-top'));
      });

      it('should scroll display to the top', () => {
        spyOn(isolateScope, 'scrollToTop');
        btn.triggerHandler('click');
        expect(isolateScope.scrollToTop).toHaveBeenCalled();
      });
    });

    describe('when close button is clicked', () => {
      let btn;

      beforeEach(() => {
        btn = ng.element(toolbar()[0].querySelector('.btn-close'));
      });

      it('should call the provided close handler', () => {
        spyOn(isolateScope, 'close');
        btn.triggerHandler('click');
        expect(isolateScope.close).toHaveBeenCalled();
      });
    });
  });

  describe('cleanup', () => {

    it('should remove its scroll event listener from the window', () => {
      spyOn($window, 'removeEventListener');
      scope.$broadcast('$destroy');
      expect($window.removeEventListener)
        .toHaveBeenCalledWith('scroll', isolateScope.handleScroll);
    });

    it('should remove the mouse wheel event listener from the window', () => {
      spyOn($window, 'removeEventListener');
      scope.$broadcast('$destroy');
      expect($window.removeEventListener)
        .toHaveBeenCalledWith('mousewheel', isolateScope.handleMouseWheel);
    });
  });
});
