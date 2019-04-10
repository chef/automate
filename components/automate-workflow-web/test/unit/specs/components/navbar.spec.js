import ng from 'angular';
import 'angular-mocks';
import navbarComponent
  from '../../../../src/components/navbar/navbar';

describe('navbarComponent', () => {
  let scope, element, $httpBackend;

  beforeEach(ng.mock.module(navbarComponent, ($provide) => {
    $provide.factory('welcomeModalDirective', () => {
      return { $get: () => {} };
    });
  }));


  function createDirective() {
    return ($compile, _$httpBackend_, $rootScope, CurrentUser) => {
      spyOn(CurrentUser, 'user').and.returnValue({
        first: 'rainbow',
        last: 'dash'
      });

      $httpBackend = _$httpBackend_;
      $httpBackend.when('GET', '/elasticsearch/node-state/_count').respond({count: 0});

      scope = $rootScope.$new();
      element = $compile(ng.element('<cd-navbar></cd-navbar>'))(scope);
      scope.$digest();
    };
  }

  describe('isParentActive()', () => {

    // we're not interested in the back-to-console link here
    beforeEach(inject(($httpBackend) => {
      $httpBackend.when('GET', '/workflow/status/console').respond(404);
      $httpBackend.when('GET', '/workflow/status/version').respond('delivery 0.6.47\n');
    }));

    beforeEach(() => {
      inject(createDirective());
      $httpBackend.flush();
    });

    it('checks if the current state is a child of the provided parent state', inject(($state) => {
      spyOn($state, 'includes').and.returnValue(true);

      let isActive = scope.isParentActive('parent.state');

      expect($state.includes).toHaveBeenCalledWith('parent.state');
      expect(isActive).toBe(true);
    }));

    it('stores no console link information', () => {
      expect(scope.console).toBeUndefined();
    });
  });

  describe('when the backend provides a console link', () => {

    beforeEach(inject(($httpBackend) => {
      $httpBackend
        .when('GET', '/workflow/status/console')
        .respond(200, {
          name: 'foobear console',
          url: 'https://foobear/console'
        });
        $httpBackend.when('GET', '/workflow/status/version').respond('delivery 0.6.47\n');
    }));

    beforeEach(() => {
      inject(createDirective());
      $httpBackend.flush();
    });

    it('consumes that information', () => {
      expect(scope.console.url).toBe('https://foobear/console');
      expect(scope.console.name).toBe('foobear console');
    });
  });
});
